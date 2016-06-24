library(ggplot2)
library(dplyr)

shinyServer(function(input, output, session) {
    
    filter_reactor <- reactive({

        
        t <- paste0("t", input$time)
	plot_type <- switch(input$plot_type,
	                    "Matric Potential" = "H_",
			    "Soil Moisture" = "Th_",
                            "Nitrate Concentration" = "NO3_")
        frt_strt <- switch(input$fert_start,
                           "none" = NULL,
                           paste0("FertSt", input$fert_start, "_"))
        frt_dur <- switch(input$fert_dur,
                          "none" = NULL,
                          paste0("FertDu", input$fert_dur))
	irr_sys <- switch(input$irr_sys,
	                  "Drip" =   "_D_",
			  "Fanjet" = "_F_")
	flow_rate <- switch(input$flow_rate,
	                    "1 in/day" = "1inday",
			    "2 in/day" = "2inday")
        
	soil_type <- input$soil_type
        
        unit_conversion <- switch(input$unit,
                                  "centimeters" = 1,
                                  "inches" = 0.393701)

        if (plot_type %in% c("NO3_")){
            search_terms <- c(plot_type, irr_sys, flow_rate, soil_type,
                              frt_strt, frt_dur)
        }

        if (plot_type %in% c("H_", "Th_")){
            search_terms <- c(plot_type, irr_sys, flow_rate, soil_type)
        }
        
	bools <- mgrepl(search_terms,
	                data_set_names, strict = length(search_terms) - 1)

        image_data <- maziarData[[data_set_names[bools]]]
        
        image_data <- image_data %>%
            dplyr::mutate(X = unit_conversion * X,
                          Y = unit_conversion * Y)
        
        plot <- ggplot(image_data, aes_string(x = "X", y = "Y", z = t)) +
            geom_raster(aes_string(fill = t), interpolate = FALSE) +
                scale_fill_gradient(low = "#be9b7b", high = "#3366ff",
                                    name = gsub(" ", "\n", input$plot_type)) +
        theme(axis.text=element_text(size=20),
              axis.title=element_text(size=25, face="bold"),
              legend.text=element_text(size=15),
              legend.title = element_text(size=15, face="bold")) +
                  guides(fill = guide_colorbar(barheight = 34))

        return(list(plot))
    })
    
    output$FlowTable <- renderPlot({
        filter_reactor()
    })
})
