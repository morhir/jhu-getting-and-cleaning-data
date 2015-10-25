run_analysis <- function (dataDir = "UCI HAR Dataset"){
        
        #for using group by 
        library(dplyr)
        
        
        dataDir <- paste("./", dataDir, sep = '')
        ##getting activity names
        fileName <- paste(dataDir, "activity_labels.txt", sep = '/')
        colNames <- c("ID", "NAME")
        activityLabels <- read.table(fileName, col.names = colNames)
        
        ##getting variable names
        colNames <- c("ID", "NAME")
        fileName <- paste(dataDir, "features.txt", sep = '/')
        features <- read.table(fileName, col.names = colNames)
        
        ##Extract only the measurements on the mean and standard deviation 
        ##for each measurement. 
        is_mean_or_std <- function(x){ 
                grepl('mean()', x, fixed = TRUE) | grepl('std()', x, fixed = TRUE)
        }
        mainFeatures <- features[sapply(features$NAME, is_mean_or_std), ]
        
        ##Function for getting data
        get_data <- function(type){
                file_name <- function(fileName) {
                        fileName <- paste(fileName, type, sep = '_')  
                        fileName <- paste(fileName, 'txt', sep = '.')
                        fileName <- paste(dataDir, type, fileName, sep = '/')
                }
                
                #reading activity
                colNames <- "activityID"
                Y <- read.table(file_name('Y'), col.names = colNames)
                
                #naming activities
                Y$activityID <- sapply(Y$activityID, f <- function(x){
                                        activityLabels$NAME[activityLabels$ID == x]})
                Y <- rename(Y, activity = activityID)
                        
                #subject
                colNames <- "subject"
                subject <- read.table(file_name('subject'), col.names = colNames)
                
                #reading data
                colNames <- as.array(features$NAME)
                X <- read.table(file_name('X'), col.names = colNames)
                
                #selecting mean, std variables 
                X <- X[mainFeatures$ID]
                
                #combining data
                data_train <- cbind(subject, Y, X)
        }
        #get test data
        data_test <- get_data('test')
        
        #get train data
        data_train <- get_data('train')
        
        #combine data
        data <- rbind(data_test, data_train)
        
        #group data
        grData <- group_by(data, subject, activity)
        grData <- summarise_each(grData, funs(mean))
        
        #write result into the file
        write.table(grData, file = 'grouped_data.txt', row.name=FALSE)
}