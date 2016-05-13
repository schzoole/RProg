rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    col_const <- list(HA="Heart.Attack", HF="Heart.Failure", PN="Pneumonia")
    out_const <- list(HA="heart attack", HF="heart failure", PN="pneumonia")
    col_name <- paste("Hospital.30.Day.Death..Mortality..Rates.from." , col_const[[match(outcome, out_const)]],sep="")
    if ((outcome %in% out_const) == FALSE) {
      stop("invalid outcome")
    }
    
        
    ## Read outcome filtered_obs
    landing_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors=FALSE )
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
    filtered_obs <- as.data.table(subset(data, !is.na(data$V5)))
    #order dataset for ties.method
    filtered_obs <- filtered_obs[order(filtered_obs$V3, filtered_obs$V5, filtered_obs$V2)]
    #rank performance 
    dt<-filtered_obs[,rn:=as.double(rank(V5,ties.method = "first")), by="V3"]
    #rank worst performance
    dt<-filtered_obs[,rnworst:=as.double(rank(-rn, ties.method = "first")), by="V3"]
    
    # assign rank we are looking for
    if (num == "best") {
        rank_row <- 1
        dt_sub <- subset(dt, dt$rn==rank_row )
    } else if (num == "worst") {
        rank_row <- 1
        dt_sub <- subset(dt, dt$rnworst==rank_row )
    } else {
        rank_row = as.numeric(num)
        if (is.na(rank_row)) {
            stop("invalid num")
        }
        dt_sub <- subset(dt, dt$rn==rank_row )
    }

    
    #Derive uniques for outer join
    df_state_all <- unique(data$V3)
    #df_state_all[dt_sub$V3]
    df_state_sub <- unique(dt_sub$V3)
    #Derive missing states
    missingStates <- df_state_all[!as.data.frame(df_state_all %in% df_state_sub)]

    #union data with missing state data
    ph <- rep(0,length(missingStates))
    df_missing_state_data <- data.frame(
            V1 = ph, 
            V2 = rep(NA,length(missingStates)), 
            V3 = missingStates, 
            V4 = ph, V5 = ph,
            rn = rep(rank_row,length(missingStates)))
    df_all_state_data <- rbind(dt_sub, df_missing_state_data, fill=TRUE, stringsAsFactors = FALSE)
    
    #order and output data
    df_all_state_data[order(df_all_state_data$V3),]
    
}

#head(rankall("heart attack", 20), 10)
#tail(rankall("pneumonia", "worst"), 3)
#tail(rankall("heart failure"), 10 )
r <- rankall("heart attack", 4)
as.character(subset(r, V3 == "HI")$V2)

r <- rankall("pneumonia", "worst")
as.character(subset(r, V3 == "NJ")$V2)

r <- rankall("heart failure", 10)
as.character(subset(r, V3 == "NV")$V2)
