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
    for (i in id) {
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
