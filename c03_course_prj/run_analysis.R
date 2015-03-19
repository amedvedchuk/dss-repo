
## Requires LaF and plyr package for readinf big Fixed Width Files 

library(LaF)
library(plyr)

# data saving parameters
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataDir <- "data"
destArchive <- paste(dataDir, "/incoming.zip", sep="")
destUnzipFolder <- paste(dataDir, "/incoming", sep="")
outputTable <- paste(dataDir, "/output.table", sep="")

# common file names
file_act_labels <- paste(destUnzipFolder, "/UCI HAR Dataset/activity_labels.txt", sep="")
file_features <- paste(destUnzipFolder, "/UCI HAR Dataset/features.txt", sep="")

# templates to access files for subsets: train, test
template_subjects <- paste(destUnzipFolder, "/UCI HAR Dataset/<subset>/subject_<subset>.txt", sep="")
template_x <- paste(destUnzipFolder, "/UCI HAR Dataset/<subset>/X_<subset>.txt", sep="")
template_y <- paste(destUnzipFolder, "/UCI HAR Dataset/<subset>/y_<subset>.txt", sep="")

downloadRawData <- function(){
    
    if(!file.exists(dataDir)){
        print(paste("download data from:", dataUrl))
        dir.create(dataDir)
        download.file(dataUrl, destArchive, method = "wget")
        print(paste("unzip files to:", destUnzipFolder))
        unzip(zipfile = destArchive, exdir = destUnzipFolder)
    } else {
        print("data already downloaded!")
    }
    
}

getNeededFeatures <- function() {
    
    features <- read.delim(file_features, header = FALSE, sep = " ")
    f_names <- as.character(features$V2)
    
    needed_var_names_indexes <- grep(pattern = "mean|std", x=f_names)
    needed_var_names <- f_names[needed_var_names_indexes]
    result <- data.frame(needed_var_names_indexes, needed_var_names)
    print(paste("obtained needed features length:", length(needed_var_names)))
    
    result
}


readRawDataSubset <- function(act_labels, f_need, subset) {
    
    resolveTemplateToFile <- function(template){
        res <- gsub("<subset>", subset, template)
        res
    }
    
    read <- function() {
        
        # read X dataset
        laf <- laf_open_fwf(resolveTemplateToFile(template_x), 
                            column_widths = rep(16, 561), column_types = rep("double", 561))
        x_set <- laf[,f_need$needed_var_names_indexes]
        names(x_set) <- f_need$needed_var_names
        close(laf)
        
        # read y dataset
        y_set <- read.csv(resolveTemplateToFile(template_y), header = FALSE)
        y_set$a_labels <- act_labels[y_set$V1,2]
        
        # read subjects dataset
        subject_train <- read.csv(resolveTemplateToFile(template_subjects), header = FALSE)
        names(subject_train) <- "subject"
        
        # combine resulting data set
        res <- data.frame(subject_train, y_set$a_labels, x_set)
        names(res)[2] <- "activity"
        print(paste("subset [",subset,"] length: ", dim(res)[1],", width: ", dim(res)[2],  sep=""))
        res
    }
    
    # call read function as a result
    read()
    
}

prepareTidyData <- function(){
    
    downloadRawData()
    
    act_labels <- read.delim(file_act_labels, header = FALSE, sep =" ")
    f_need <- getNeededFeatures()
    
    res_train <- readRawDataSubset(act_labels, f_need, "train")
    res_test <- readRawDataSubset(act_labels, f_need, "test")
    
    # merge train and test data
    tidyData <- rbind(res_test, res_train)
    
    resultData <- ddply(tr_res, .variables = .(subject, activity), colwise(mean))
    #make names human readable
    data_names <- gsub("\\.\\.\\.", ".", names(resultData))
    data_names <- gsub("\\.\\.", "", data_names)
    data_names[-(1:2)] <- paste("MEAN.",data_names[-(1:2)], sep="")
    names(resultData) <- data_names
    
    write.table(res, file = outputTable, row.name=FALSE)
    print(paste("output data set saved to:", outputTable, " dimensions: ", dim(resultData)[1], dim(resultData)[2]))
    resultData
}

resultData <- prepareTidyData()
