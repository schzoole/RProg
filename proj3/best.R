best <- function(state, outcome) {
    # constants for state, heart attack, heart failure, and pneumonia rates
    col_const <- list(HA=11, HF=17, PN=23, STATE_COL=7, HOSP_COL=2)
    out_const <- list(HA="heart attack", HF="heart failure", PN="pneumonia")

    min_rate  <- Inf
    hosp_name <- "zzz"
    out_col   <- 1
    least_col <- 1
    cur_row   <- 0
    
    ## Read outcome data
    out_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (state %in% out_data[, col_const$STATE_COL] == FALSE) {
        stop("invalid state")
    }

    if ((outcome %in% out_const) == FALSE) {
        stop("invalid outcome")
    }
    
    # match outcome to the appropriate column
    out_col <- col_const[[match(outcome, out_const)]]
        
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    #outcome[, 11] <- as.numeric(outcome[, 11])

    for (rate in out_data[,out_col]) {
        cur_row <- cur_row + 1
        if (is.na(rate) 
            || rate == "Not Available" 
            || state != out_data[cur_row,][[col_const$STATE_COL]])
            next
        rate <- as.numeric(rate)
        if (rate <= min_rate) {
            cur_hosp_name <- out_data[cur_row,][[col_const$HOSP_COL]]
            if (rate == min_rate) {
                if (cur_hosp_name > hosp_name)
                    next
            }                
            hosp_name <- cur_hosp_name
            min_rate  <- rate
        }
    }
    hosp_name
}