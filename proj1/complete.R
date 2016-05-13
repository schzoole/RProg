
getCountsByColumn_asDataFrame <- function(dataframe, colname) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases


	# Massage, format, and order data.frame.
	# TODO:  revise tapply for dynamic columns. :(
	nobs <- tapply(dataframe$ID, dataframe$ID, length)
	id <- as.numeric(rownames(nobs))
	m <- cbind(id,nobs)
	row.names(m)<-NULL
	df <- as.data.frame(m)
	
	if (head(id,1) > tail(id,1)) {
		df[rev(order(df$id)),]
	} else {
		df[order(df$id),]
	}
}


complete <- function(directory, ids){
	## endpoint for RProg course.  
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

	# Populate dataframe with fileset specified.
	fs<- getSpecDataFileNames(directory,ids)
	dataframe <- getCsv_AsDataFrame(fs)
	
	# Populate complete.cases data.frame from dataframe.
	dataframecc <- subset(dataframe, complete.cases(dataframe))
	
	getCountsByColumn_asDataFrame(dataframecc, "ID")
}


#TEST CASES
test_output1 <- function(){
	a <- complete("specdata", 1)
	b <- data.frame(id = c(1), nobs = c(117))
	identical(a,b) 
}

test_output2 <- function(){
	a <- complete("specdata",  c(2,4,8,10,12))
	b <- data.frame(id = c(2,4,8,10,12), 
	nobs = c(1041,474,192,148,96))
	identical(a,b) 
}

test_output3 <- function(){
	a <- complete("specdata", 30:25)
	b <- data.frame(id = 30:25, 
	nobs = c(932,711,475,338,586,463))
	identical(a,b) 
}

test_output4 <- function(){
	a <- complete("specdata", 3)
	b <- data.frame(id = c(3), nobs = c(243))
	identical(a,b) 
}

test_output1()
test_output2()
test_output3()
test_output4()






