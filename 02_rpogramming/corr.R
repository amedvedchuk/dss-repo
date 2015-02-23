
source("readspec.R")

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    cc <- complete("specdata")
    
    neededLocIds <- subset(cc, nobs>threshold, select = "id")
    
    print(paste("neededLocIds.length = ", length(neededLocIds[,1])))
    
    neededLocIds
    
    result <- vector()
    
    if(length(neededLocIds[,1])== 0){
        print("return")
        result
    } else {
        
        
        
        #   print(neededLocIds[,1])
        
        ids <- as.integer(as.vector(neededLocIds$id))
        
        ids
        
        #   print("ids as vumecric = ")
        #   print(ids)
        #   
        
        files <- paste(directory, "/" ,formatC(ids, width = 3, flag = "0", format = "d"), ".csv", sep = "")
        print(files)
        
        #   dfForCorr <- do.call("rbind", lapply(files, read.csv))
        dfForCorr <- lapply(files, read.csv)
        
        #   dfForCorr <- lapply(dfForCorr, complete.cases(dfForCorr))
        
        #   dfForCorr <- readspec(directory, ids)
        
        #   dfForCorr <- dfForCorr[complete.cases(dfForCorr),]
        #   
        dfForCorr
        
        
        
        for (i in seq_along(dfForCorr)){
            ccl <- dfForCorr[[i]] 
            ccl <- ccl[complete.cases(ccl), ]
            #         print(ccl)
            dfForCorr[[i]] <- ccl
            result[i] <- cor(ccl$sulfate, ccl$nitrate)
        }
        #     dfForCorr
        result
    }
}
