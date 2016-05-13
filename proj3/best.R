best <- function(state, outcome) {

    options(warn=-1)
    # constants for state, heart attack, heart failure, and pneumonia rates
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
    filtered_obs <- subset(data, data$V3 == state)
    ## unload landing dataset
    rm(landing_data)
    rm(data)
    


    
    
    # match outcome to the appropriate column
    out_col <- col_const[[match(outcome, out_const)]]
    
    #identify min value
    min_val <- min(filtered_obs$V5,na.rm=TRUE)
    
    #return sorted order of hospitals with lowest rates    
    min_df <- subset(filtered_obs, filtered_obs$V5 == min_val & filtered_obs$V3 == state)
   # head(sort(as.character(min_df$V2), decreasing = FALSE),1)
    head(sort(as.character(min_df$V2), decreasing = FALSE),1)
}

test_output1 <- function() {
  output <- best("TX","heart attack")
  identical(output, "CYPRESS FAIRBANKS MEDICAL CENTER")
}

test_output2 <- function() {
  output <- best("TX", "heart failure")
  identical(output, "FORT DUNCAN MEDICAL CENTER")
}

test_output3 <- function() {
  output <- best("MD", "heart attack")
  identical(output, "JOHNS HOPKINS HOSPITAL, THE")
}

test_output4 <- function() {
  output <- best("MD", "pneumonia")
  identical(output, "GREATER BALTIMORE MEDICAL CENTER")
}


test_output5 <- function() {
  output <- tryCatch(stop(best("BB", "heart attack")), error = function(e) e)
  output2 <-"<simpleError in best(\"BB\", \"heart attack\"): invalid state>"
  identical(output, output2)
}

test_output6 <- function() {
  output <- tryCatch(stop(best("NY", "hert attack")), error = function(e) e)
  output2 <-"<simpleError in best(\"NY\", \"hert attack\") : invalid outcome"
  identical(output, output2)
}

test_output1()
test_output2()
test_output3()
test_output4()
test_output5()
test_output6()

              