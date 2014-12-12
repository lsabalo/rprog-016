rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    # If invalid state is given, then stop with message "invalid state"
    valid_state <- FALSE
    for (i in unique(file$State)) {
        if (state == i) {
            valid_state <- TRUE
            break
        }
    }
    if (!valid_state) {
        stop("invalid state")
    }
    
    # If invalid outcome is given, then stop with message "invalid outcome"
    valid_outcome <- FALSE
    for (i in c("heart attack", "heart failure", "pneumonia")) {
        if (outcome == i) {
            valid_outcome <- TRUE
            break
        }
    }
    if (!valid_outcome) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    col <- if (outcome == "heart attack") {
        11
    } else if (outcome == "heart failure") {
        17
    } else {
        23
    }
    sub <- file[file$State == state, c(2, 7 , col)]
    sub[, 3] <- suppressWarnings(as.numeric(sub[, 3]))
    sub <- sub[complete.cases(sub), ]
    
    if (is.numeric(num)) {
        if (num > length(sub[, 3])) {
            return(NA)
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
    sub[order(sub[,3], sub[, 1]), ][rank, 1]
}