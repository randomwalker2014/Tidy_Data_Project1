####################################################################
# This R Script collects,combines,transforms and cleans up raw 
# accelerometer and gyroscope data set and generates a tidy aggregated 
# data file 
####################################################################

## Read training and test measurement data files 
trainData <- read.table("./train/X_train.txt",header=FALSE)
testData <- read.table("./test/X_test.txt",header=FALSE)

## Read subject related training and test data files
subjectTrainData <- read.table("./train/subject_train.txt",header=FALSE)
subjectTestData <- read.table("./test/subject_test.txt",header=FALSE)

## Assign friendly column names
colnames(subjectTrainData) <- c("subject.id")
colnames(subjectTestData) <- c("subject.id")

## Read activity related Training and Test data
activityTrainData <- read.table("./train/y_train.txt",header=FALSE)
activityTestData <- read.table("./test/y_test.txt",header=FALSE)

## Assign friendly column names
colnames(activityTrainData) <- c("activity.id")
colnames(activityTestData) <- c("activity.id")

## Read activity label data
activityLabelData <- read.table("./activity_labels.txt")
colnames(activityLabelData) <- c("activity.id","activity.label")

## Read feature data
featureData <- read.table("./features.txt",header=FALSE)
colnames(featureData) <- c("feature.id","feature.name")

## Combine subject and actvity Test and Training data sets by columns
subject_activity_train <- cbind(subjectTrainData,activityTrainData)
subject_activity_test <- cbind(subjectTestData,activityTestData)

## Combine subject_actvity Test and Training data sets from previous step by rows
subject_activity_merged <- rbind(subject_activity_train,subject_activity_test)

## Assign the descriptive activity labels 
subject_activity_merged$activity.label <- activityLabelData[subject_activity_merged$activity.id,"activity.label"]

## Combine the measurement data by rows and create a single data set
mergedData <- rbind(trainData,testData)

## Assign column names to the merged data set using the feature data set
colnames(mergedData) <- featureData[,"feature.name"]

## Retain data only columns that are related to mean or standard-deviation (exclude the rest)
mergedDataSubset_meanStd <- mergedData[,which( (grepl("mean",colnames(mergedData),ignore.case=FALSE) | 
                                                grepl("std",colnames(mergedData),ignore.case=FALSE)) &
                                               !grepl("meanFreq",colnames(mergedData),ignore.case=FALSE))]

## Additional cleanup of the column names by stripping off enclosing brackets and dashes
colnames(mergedDataSubset_meanStd) <- sapply(colnames(mergedDataSubset_meanStd),
                                             function(x) gsub("[()]", "", x, ignore.case = TRUE))
colnames(mergedDataSubset_meanStd) <- sapply(colnames(mergedDataSubset_meanStd),
                                             function(x) gsub("[-]", ".", x, ignore.case = TRUE))

## Combine the subject_activity dataset with the measurement dataset by columns
mergedDataSubset_meanStd <- cbind(subject_activity_merged, mergedDataSubset_meanStd)

## Generate the tidy data set by applying an aggregate function that groups by subject.id,activity.id and activity.label 
## and calculates the mean for all other measurement columns

tidyDataSet <- aggregate(. ~ subject.id + activity.id + activity.label,data=mergedDataSubset_meanStd,FUN=mean,na.rm =T)
rownames(tidyDataSet) <- NULL

## Write the tidy data set to a tab delimited text file
write.table(tidyDataSet, file = "tidydata.txt", sep = "\t", col.names = colnames(tidyDataSet))

