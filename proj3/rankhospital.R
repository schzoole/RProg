rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    # constants for state, heart attack, heart failure, and pneumonia rates
  
    options(warn=-1)
    col_const <- list(HA="Heart.Attack", HF="Heart.Failure", PN="Pneumonia")
    out_const <- list(HA="heart attack", HF="heart failure", PN="pneumonia")
    col_name <- paste("Hospital.30.Day.Death..Mortality..Rates.from." , col_const[[match(outcome, out_const)]],sep="")
    
    ## Read outcome filtered_obs
    landing_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors=FALSE )
    
    ## Check that state and outcome are valid
    if (state %in% landing_data$State == FALSE) {
      stop("invalid state")
    }
        
    
    if ((outcome %in% out_const) == FALSE) {
      stop("invalid outcome")
    }
    
    
    data <- cbind.data.frame(
      V1 = landing_data$Provider.Number, 
      V2 = landing_data$Hospital.Name, 
      V3 = landing_data$State, 
      V4 = landing_data[,col_name],
      stringsAsFactors=FALSE
    )
    #Add numeric column for evaluation
    data <- cbind(data, "V5" =as.numeric(data$V4) )
    
    #filter and rank data.fram
    filtered_obs <- subset(data, data$V3 == state & !is.na(data$V5))
    filtered_obs["rank"] <- as.numeric(NA)
    filtered_obs$rank[order(filtered_obs$V5, filtered_obs$V2)] <- 1:nrow(filtered_obs)

    ## unload landing datasets
    #rm(landing_data)
    #rm(data)
    
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
    
    # if we want "worst", make sure we index only available values
    if (is.infinite(rank_row)) {
        rank_row <- nrow(filtered_obs)
    }
    
    ## Return hospital name in that state with specified ranking
    hosp_name <- subset(filtered_obs, filtered_obs$rank==rank_row)
    
    if( length(hosp_name$V2) == 0) {
      NA
      } else {
      as.character(hosp_name$V2)
      }
    
}

test_output1 <- function() {
  output <- rankhospital("TX","heart failure", 4)
  identical(output, "DETAR HOSPITAL NAVARRO")
}

test_output2 <- function() {
  output <- rankhospital("MD", "heart attack", "worst")
  identical(output, "HARFORD MEMORIAL HOSPITAL")
}

test_output3 <- function() {
  output <- rankhospital("MN", "heart attack", 5000)
  identical(output, NA)
}

test_output1()
test_output2()
test_output3()

