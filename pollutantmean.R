pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    if (pollutant != "sulfate" & pollutant != "nitrate") {
        return(NULL)
    }
    filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
    ldf <- lapply(filenames, read.csv)
    data <- do.call(rbind, ldf)
    buf <- data.frame(Date=as.Date(character()), sulfate=numeric(), nitrate=numeric(), ID=integer())
    res1 <- data.frame(Date=as.Date(character()), sulfate=numeric(), nitrate=numeric(), ID=integer())
    for (i in id) {
        buf <- data[data$ID == i, ]
        res1 <- rbind(res1, buf)
    }
    if (pollutant == "sulfate") {
        res <- res1[!is.na(res1$sulfate), ]$sulfate
    } else {
        res <- res1[!is.na(res1$nitrate), ]$nitrate
    }
    mean(res)
}