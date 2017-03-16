## data cleaning project
## The purpose of this project is to demonstrate your ability to collect, 
## work with, and clean a data set. The goal is to prepare tidy data that 
## can be used for later analysis. You will be graded by your peers on a 
## series of yes/no questions related to the project. You will be required 
## to submit: 1) a tidy data set as described below, 
## 2) a link to a Github repository with your script for performing the 
## analysis, and 
## 3) a code book that describes the variables, the data, and any 
## transformations or work that you performed to clean up the data called 
## CodeBook.md. You should also include a README.md in the repo with your 
## scripts. This repo explains how all of the scripts work and how they are 
## connected.

## One of the most exciting areas in all of data science right now is 
## wearable computing - see for example this article . Companies like 
## Fitbit, Nike, and Jawbone Up are racing to develop the most advanced 
## algorithms to attract new users. The data linked to from the course 
## website represent data collected from the accelerometers from the 
## Samsung Galaxy S smartphone. A full description is available at the 
## site where the data was obtained:
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## IMPORT

library(data.table)
library(dplyr)

## LOADING DATA:

dataURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download.file(dataURL, 'projectData.zip') 
unzip ('projectData.zip', exdir = "./") # UCI HAR dataset

## run_analysis.R does the following.
## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for 
## each measurement.
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names.

datadir <- 'UCI HAR Dataset/'
test_datadir <- 'UCI HAR Dataset/test/'
train_datadir <- 'UCI HAR Dataset/train/'

## interested only in mean and std features
features <- data.table(read.table(paste0(datadir, 'features.txt')))
colnames(features)  = c('featureID','featuresName');

activity_labels <- data.table(read.table(paste0(datadir, 'activity_labels.txt')))
colnames(activity_labels)  = c('activityID','activityType');

features_of_interest <- which(
    (grepl('-mean()', tolower(features$featuresName)) & 
        !grepl("-meanfreq()", tolower(features$featuresName))) |
        grepl("-std()", tolower(features$featuresName)))

## loading data
trainingSet <- data.table(read.table(paste0(train_datadir, 'X_train.txt')))
## features x training/subject
trainingLabels <- read.table(paste0(train_datadir, 'Y_train.txt'))
trainingSubj <- read.table(paste0(train_datadir, 'subject_train.txt'))

testingSet <- data.table(read.table(paste0(test_datadir, 'X_test.txt')))
testingLabels <- read.table(paste0(test_datadir, 'Y_test.txt'))
testingSubj <- read.table(paste0(test_datadir, 'subject_test.txt'))

## combining
trainingSet <- mutate(trainingSet,activityID = trainingLabels$'V1',
                      subject = trainingSubj$'V1')
testingSet <- mutate(testingSet,activityID = testingLabels$'V1',
                     subject = testingSubj$'V1')

dataSet <- rbind(trainingSet,testingSet)
names(dataSet) = c(as.character(features$featuresName),'activityID','subject')

## including only the features of interest
dataSet <- dataSet[,c(features_of_interest,length(dataSet)-1,length(dataSet))]

##
## From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

second_dataset <- aggregate(dataSet[,c(1:length(features_of_interest))], 
                            by = list(subjID = dataSet$subject,
                                      activityID = dataSet$activityID),
                            mean)

second_dataset <- merge(second_dataset,activity_labels,by = "activityID")
dataSet <- merge(dataSet,activity_labels,by = "activityID")

write.table(second_dataset,"tidy_dataset.txt",
            sep = ",", 
            col.names = colnames(second_dataset)
            )



