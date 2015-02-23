

source("readspec.R")

complete <- function(directory, id = 1:332) {
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
    
    
#     files <- paste(directory, "/" ,formatC(id, width = 3, flag = "0", format = "d"), ".csv", sep = "")
#     print(files)
#     
#     readed3 <- do.call("rbind", lapply(files, read.csv))
  
  readed3 <- readspec(directory = directory, id = id)
  
    #   r[complete.cases(r),]   
    cc <- readed3[complete.cases(readed3),]
#     print(cc)
    fcc <- factor(cc[["ID"]], levels = as.character(id))
#     print(fcc)
    t <- table(fcc)
#     print(t)
    res <- data.frame( stack(t)[2], stack(t)[1])
    colnames(res) <- c("id","nobs")
    
#     print(res)
    
    res
    
# readed3
}