getSpecDataFileNames <- function(directory, id) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return an character vector of fully qualified names.

	if (identical(NULL,directory) == TRUE) {
		directory <- getwd()
	}

    files <- character()
    # generate list of filenames
    for (i in id) {
        files <- c(files, sprintf("%s\\%03d.csv", directory, i))
    }
    files
}

getFilesFromDir <- function(directory){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## Return an character vector of fully qualified files names from the directory.

	files <- dir(directory)
}

getCsv_AsList <- function(files) {
    ## 'files' is a character vector of length n indicating fully qualifed file paths & names.
    
    ## Return an list of data retreived from files. 
    
	list <-	lapply(files, function(i) {read.csv(i)})
}

getCsv_AsDataFrame <- function(files) {
    ## 'files' is a character vector of length n indicating fully qualifed file paths & names.
    
    ## Return a data frame of data retreived from files. 

	list <-	getCsv_AsList(files)
	dataframe <- do.call("rbind", list)
}

getVector_FromDataFrame <- function(dataframe, colname, removeNA) {
	## 'dataframe' is a data.frame used as source data.
	## 'colname' is the data.frame column used as the target column.
	
	## Returns a character vector of data from the data.frame referenced column.
	
    values <- numeric()
    if (identical(removeNA,TRUE) == TRUE) {
    	rmdata = is.na(dataframe[colname])
	    values <- c(values, dataframe[colname][!rmdata])    
    } else {
	    values <- c(values, dataframe[colname])    
	}
}


getPollutantMean <- function(directory, colname, fileset){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'fileset' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## 'colname' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
	
	fs <- getSpecDataFileNames(directory, fileset)
	dataframe <- getCsv_AsDataFrame(fs)
	data <- getVector_FromDataFrame(dataframe, colname, TRUE)
	mean(data)
}

pollutantmean <- function(directory, colname, fileset){
	getPollutantMean(directory, colname, fileset)
}

pollutantmean("specdata", "sulfate", 1:10) 

#TEST CASES
test_output1 <- function() {
	a <- getPollutantMean("specdata", "sulfate", 1:10)
	b <- 4.064128
	round(a[1],6)==b[1]
}

test_output2 <- function() {
	a <- getPollutantMean("specdata", "nitrate", 70:72)
	b <- 1.732979
	round(a[1],6)==b[1]
}

test_output3 <- function() {
	a <- getPollutantMean("specdata", "nitrate", 23)
	b <- 1.280833
	round(a[1],6)==b[1]
}


test_output1()
test_output2()
test_output3()



#rm(list=ls(all=TRUE))



