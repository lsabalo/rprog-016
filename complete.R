complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
    ldf <- lapply(filenames, read.csv)
    data <- do.call(rbind, ldf)
    res <- data.frame(id=numeric(), nobs=numeric())
    for (i in id) {
        temp <- data[data$ID == i, ]
        nobs <- nrow(temp[complete.cases(temp), ])
        res <- rbind(res, data.frame(i=i, nobs=nobs))
    }
    res
}