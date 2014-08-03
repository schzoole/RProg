rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    # constants for state, heart attack, heart failure, and pneumonia rates
    col_const <- list(HA=11, HF=17, PN=23, STATE_COL=7, HOSP_COL=2)
    out_const <- list(HA="heart attack", HF="heart failure", PN="pneumonia")
    
    min_rate  <- Inf
    hosp_name <- "zzz"
    cur_row   <- 0
    rank_row  <- Inf
    
    # assign rank we are looking for
    if (num == "best") {
        rank_row <- 1
    } else if (num == "worst") {
        rank_row <- Inf
    } else {
        rank_row = as.numeric(num)
        if (is.na(rank_row)) {
            stop("invalid num")
        }
    }
    ## Check that state and outcome are valid
    if (state %in% out_data[, col_const$STATE_COL] == FALSE) {
        stop("invalid state")
    }
    
    if ((outcome %in% out_const) == FALSE) {
        stop("invalid outcome")
    }
    
    ## match outcome to the appropriate column
    out_col <- col_const[[match(outcome, out_const)]]
    
    ## Read outcome data
    out_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    out_data[, out_col] <- as.numeric(out_data[, out_col])
    
    ## get items from state of interest only
    state_data <- out_data[out_data$State == state,]
    
    ## sort by specified outcome
    sorted_data <- state_data[order(state_data[,out_col], state_data$Hospital.Name),]
    
    # remove "Not Availables"
    clean_data <- 
        sorted_data[!na.omit(sapply(sorted_data[,out_col], is.na)),]

    # if we want "worst", make sure we index only available values
    if (is.infinite(rank_row)) {
        rank_row <- nrow(clean_data)
    }
    
    ## print top results
    # print(head(cbind(clean_data[c("Hospital.Name", "State")], clean_data[,out_col]))    )
    
    ## Return hospital name in that state with specified ranking
    hosp_name <- clean_data[rank_row,]$Hospital.Name
    
    hosp_name
    
}