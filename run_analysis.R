library(dplyr)
library(plyr)
library(qdap)
library(data.table)

##import data files into R
setwd("~/Dropbox/Coursera/UCI HAR Dataset")
features <- as.matrix(read.table('features.txt'))[,2] ##variable labels (561)
train <- read.table("~/Dropbox/Coursera/UCI HAR Dataset/train/X_train.txt", col.names = features) ##training data set
subjtrain <- as.vector(read.table("~/Dropbox/Coursera/UCI HAR Dataset/train/subject_train.txt") [,1]) ##subject number for each training row
trainlabels <- as.vector(read.table("~/Dropbox/Coursera/UCI HAR Dataset/train/y_train.txt")[,1]) ##activity number for each training row
activitylabels <- as.vector(read.table("activity_labels.txt")[,2]) ##legend for activity numbers
test <- read.table("~/Dropbox/Coursera/UCI HAR Dataset/test/X_test.txt", col.names = features) ##test data set
subjtest <- as.vector(read.table("~/Dropbox/Coursera/UCI HAR Dataset/test/subject_test.txt")[,1]) ##subject number for each test row
testlabels <- as.vector(read.table("~/Dropbox/Coursera/UCI HAR Dataset/test/y_test.txt")[,1]) ##activity number for each test row

##add subject and activity columns to train and test datasets
train$subject <- subjtrain ##column coding for subject number in training set
train$activity <- trainlabels ##column coding for activity in training set
test$subject <- subjtest ##column coding for subject number in test set
test$activity <- testlabels ##column coding for activity in test set 

##Step 1: Combine training and test data sets
completedata <- rbind(train, test)

##Step 2: Extract only the measurements on the mean and standard deviation for each measurement
meansandsds <- cbind(select(completedata, subject, activity, contains(".mean.."), contains(".std..")))

##Step 3: Use descriptive activity names to name the activities in the data set
meansandsds$activity <- mgsub(1:6,activitylabels,meansandsds$activity) ##relabel activity numbers with descriptions

##Step 4: Appropriately label the data set with descriptive variable names
descriptive_columns <- function(columns = names(meansandsds)) {
    columns <- gsub("^t", "time", columns)
    columns <- gsub("^f", "fouriertransformed", columns)
    columns <- gsub("Acc", "accelerometry", columns)
    columns <- gsub("Gyro", "gyroscopy", columns)
    columns <- gsub("Mag", "magnitude", columns)
    columns <- gsub("\\.", "", columns)
    columns <- gsub("Body+", "body", columns)
    columns <- gsub("bodybody", "body", columns)
    columns <- tolower(columns)
    return (columns)
    
}
names(meansandsds) <- descriptive_columns()

##Step 5: Create a second, independent tidy data set with the average of each variable for each activity and each subject.
tidymeans <- aggregate(meansandsds, by=list(meansandsds$subject,meansandsds$activity), FUN=mean, na.rm=TRUE)
tidymeans <- select(tidymeans, -subject, -activity)
tidymeans <- select(tidymeans, subject = Group.1, activity = Group.2, timebodyaccelerometrymeanx:fouriertransformedbodygyroscopyjerkmagnitudemean) 

##Save meansandsds & tidymeans dataframes as csv files
write.csv(meansandsds, file = "meansandsds.csv",row.names = FALSE)
write.csv(tidymeans, file = "tidymeans.csv",row.names = FALSE)