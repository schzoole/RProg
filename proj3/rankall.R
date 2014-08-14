rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
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
    ## Check that outcome is valid
    
    if ((outcome %in% out_const) == FALSE) {
        stop("invalid outcome")
    }
    
    ## match outcome to the appropriate column
    out_col <- col_const[[match(outcome, out_const)]]
    
    ## Read outcome data
    out_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    out_data[, out_col] <- as.numeric(out_data[, out_col])
    
    ## sort by specified outcome
    sorted_data <- out_data[order(out_data$State, out_data[,out_col], out_data$Hospital.Name),]
    
    # remove "Not Availables"
    clean_data <- 
        sorted_data[!na.omit(sapply(sorted_data[,out_col], is.na)),]
    
    ## print top results
    # print(head(cbind(clean_data[c("Hospital.Name", "State")], clean_data[,out_col]))    )
    
    ## Return hospital name in that state with specified ranking

    cur_state <- NULL
    dd <- data.frame(hospital = character(), state = character())
    colnames(dd) <- c("hospital", "state")
    
    nr <- nrow(clean_data) 
    
    for (i in 1:nr)
    {
        if (is.null(cur_state) || cur_state != clean_data$State[i] )
        {
            cur_state <- clean_data$State[i]
            # if we want "worst", find the last hospital in this state
            if (num == "worst") {
                j <- i
                while (j <= nr && cur_state == clean_data$State[j]) {
                    j <- j + 1
                }
                rank_row <- j - i
            }
            dd <- rbind(dd, data.frame(
                                    hospital = clean_data$Hospital.Name[i+rank_row-1],
                                    state    = cur_state))
        }
    }
    
    dd
}