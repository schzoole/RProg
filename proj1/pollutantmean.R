getFileList <- function(directory, id) {
    files <- character()
    # generate list of filenames
    for (i in id) {
        files <- c(files, sprintf("%s/%03d.csv", directory, i))
    }
    files
}

getPollutants <- function(files, pollutant) {
    values <- numeric()
    for (i in seq_along(files)) {
        data <- read.csv(files[i])
        bad = is.na(data[pollutant])
        values <- c(values, data[pollutant][!bad])
    }    
    values
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)

    files <- getFileList(directory, id)
    values <- getPollutants(files, pollutant)
    sprintf("%0.3f", mean(values))
    
}