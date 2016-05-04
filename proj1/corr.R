corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    id     <- 1:332
    values <- numeric()
    
    # calculate good values in each file
    #for (i in id) {
        # read in file
        file <- sprintf("%s/%03d.csv", directory, i)
        data <- read.csv(file)
        
        # find complete observations
        comp <- data[complete.cases(data),]
        if (nrow(comp) >= threshold) {
            correlation = cor(comp[2], comp[3])[1]
            values <- c(values, correlation)
        }
    }
    
    values
}

corr2 <- function (directory, threshold = 0) {

	# Populate dataframe with fileset specified.
	directory <- "C:\\Users\\Society\\Documents\\specdata"
	
	
	fs <- getSpecDataFileNames(directory, 1:332)
	dataframe <- getCsv_AsDataFrame(fs)
	
	# Populate complete.cases data.frame from dataframe.
	dataframecc <- subset(dataframe, complete.cases(dataframe))
	dataframect <- getCountsByColumn_asDataFrame(dataframecc, "ID")
	thresholds <- subset(dataframect$id, dataframect$nobs >= threshold)
 	#dataframerd <- dataframecc[dataframecc$ID %in% thresholds,]
    
    df <- numeric()
    for (i in thresholds) {
	    dataframerd <- dataframecc[dataframecc$ID ==i,]
	    x <- as.numeric(cor(dataframerd$sulfate, dataframerd$nitrate, use = "complete.obs"))
		df <- c(df, x)
		#df <- c(df, cor(dataframerd$sulfate, dataframerd$nitrate, use = "complete.obs"))
    }
    
	df
}


cr <- corr2("specdata", 150)
head(cr)
summary(cr)

cr <- corr2("specdata", 400)
head(cr)
summary(cr)



## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630


cr <- corr("specdata", 400)
head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630


cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
length(cr)
## [1] 0


cr <- corr("specdata")
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
length(cr)
## [1] 323


