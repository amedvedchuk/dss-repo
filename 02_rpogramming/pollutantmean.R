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
    
    
    ## reading variant 1
    #   readed <- list();
    #   for (i in seq_along(id)){
    #     file <- (paste(directory, "/", formatC(id[i], width = 3, flag = "0", format = "d"), ".csv", sep = ""))
    #     print(file)
    #     readed[[i]] <- read.csv(file = paste(file))
    #   }
    #   print(readed)
    
    
    ## reading variant 2
    #   files <- paste(directory, "/" ,formatC(id, width = 3, flag = "0", format = "d"), ".csv", sep = "")
    #   print(files)
    #   readed2 <- lapply(files, read.csv)
    # #   readed2 <- lapply(readed2, rbind)
    #   print(readed2)
    
    
    ## reading variant 3
    files <- paste(directory, "/" ,formatC(id, width = 3, flag = "0", format = "d"), ".csv", sep = "")
    print(files)
    readed3 <- do.call("rbind", lapply(files, read.csv))
    
    print("merged data length: ")
    print(length(readed3[,1]))
    
    print(pollutant)
    
    polmean <- mean(readed3[[pollutant]], na.rm = TRUE)
    
    print(polmean)
    
    polmean
    
}