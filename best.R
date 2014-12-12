best <- function(state, outcome) {
    ## Read outcome data
    file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    # If invalid state is given, then stop with message "invalid state"
    
    # If invalid outcome is given, then stop with message "invalid outcome"
    
    
    ## Return hospital name in that state with lowest 30-day death
    col <- if (outcome == "heart attack") {
        13
    } else if (outcome == "heart failure") {
        19
    } else {
        25
    }
    sub <- file[file$State == state, c(2, 7 , col)]
    sub[, 3] <- suppressWarnings(as.numeric(sub[, 3]))
    sub[order(sub[,3], sub[, 1]), ][1, 1]
    
    ## rate
}