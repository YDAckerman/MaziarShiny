library(kknn)
library(plyr)
library(ggplot2)

directory <- "~/Documents/Coding/Freelance/MaziarShiny/Data"
files <- grep("\\.csv", list.files(directory), value = TRUE)

maziarData <- llply(files, function(file){
    d <- read.csv(paste0(directory, "/", file), header = TRUE)
    ## determine position of the 'X' column
    i <- which.max(as.numeric(gsub("X", "", colnames(d))))
    tcols <- paste("t", 0:24, sep = "")
    colnames(d)[i:ncol(d)] <- c("X", "Y", tcols)
    test_d <- expand.grid(0:300, -300:0)
    colnames(test_d) <- c("X", "Y")
    ## loop through all the t's, and keep a
    ## list of the resulting images they produce
    image_data <- llply(tcols, function(t){
        ## run knn model
        form <- paste0(t," ~ X + Y")
        mod <- kknn(form, train = d, test = test_d, k = 5)
        ## get fitted data
        test_d[,t] <- mod$fitted.values
        test_d
    })
    image_data <- Reduce(function(...) merge(..., all = TRUE), image_data)
    image_data
}, .progress = "text")

names(maziarData) <- files

save(maziarData,
     file = paste0("~/Documents/Coding/Freelance/MaziarShiny",
         "/dist/shiny/maziarData.rda"))
