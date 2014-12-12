rankall <- function(outcome, num = "best") {
    ## Read outcome data
    file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    # If invalid outcome is given, then stop with message "invalid outcome"
    valid_outcome <- FALSE
    for (i in c("heart attack", "heart failure", "pneumonia")) {
        if (outcome == i) {
            valid_outcome <- TRUE
            break
        }
    }
    if (!valid_outcome) {
        stop("invalid outome")
    }
    
    ## For each state, find the hospital of the given rank
    res_df <- data.frame(hospital=character(), state=character())
    for (i in sort(unique(file$State))) {
        col <- if (outcome == "heart attack") {
            11
        } else if (outcome == "heart failure") {
            17
        } else {
            23
        }
        sub <- file[file$State == i, c(2, 7 , col)]
        sub[, 3] <- suppressWarnings(as.numeric(sub[, 3]))
        sub <- sub[complete.cases(sub), ]
        
        if (is.numeric(num)) {
            if (num > length(sub[, 3])) {
                rank <- -1
            } else {
                rank <- num
            }
        } else if (num == "best") {
            rank <- 1
        } else if (num == "worst") {
            rank <- length(sub[, 3])
        } else {
            stop("invalid rank")
        }
        if (rank == -1) {
            hospital <- NA
        } else {
            hospital <- sub[order(sub[,3], sub[, 1]), ][rank, 1]
        }
        res_df <- rbind(res_df, data.frame(hospital=hospital, state=as.character(i)))
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    rownames(res_df) <- res_df[, 2]
    res_df
}