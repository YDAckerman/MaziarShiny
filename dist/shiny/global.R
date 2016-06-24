load("maziarData.rda")
data_set_names <- names(maziarData)

mgrepl <- function(patterns, x, ignore.case = FALSE,
                      strict = 0, fuzzy = FALSE){
    require(plyr)
    f <- if (fuzzy){ agrepl } else { grepl }
    bool_df <- ldply(patterns, function(pattern){
        f(pattern, x, ignore.case = ignore.case)
    })
    colSums(bool_df) > strict
}
