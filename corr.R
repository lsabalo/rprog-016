corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
    ldf <- lapply(filenames, read.csv)
    data <- do.call(rbind, ldf)
    nid <- unique(data$ID)
    cr <- vector(mode="numeric", length=0)
    for (i in seq(along=nid)) {
        temp <- data[data$ID == i, ]
        res <- temp[complete.cases(temp), ]
        if (nrow(res) > threshold) {
            cr <- c(cr, cor(res$sulfate, res$nitrate))
        }
    }
    cr
}