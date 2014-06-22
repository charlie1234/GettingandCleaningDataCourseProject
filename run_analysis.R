## Presentation
# This file contains the script requested in order to complete the course project
# of "Getting and Cleaning Data" course from Coursera. According to the 
# instructions, I will perform several  tasks on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# The tasks I will perform to this collection of data are:

# 1. Merging the training and the test sets to create one data set

# 2. Extracting only the measurements on the mean and standard deviation for each measurement

# 3. Using descriptive activity names to name the activities in the data set

# 4. Appropriately label the data set with descriptive activity names 

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject 

###### Task 1: merge the training and the test sets to create one data set

setwd("~/GettingandCleaningDataCourseProject/")

## Read data from files

trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
dim(trainData) # 7352*561
head(trainData)



trainLabel <- read.table("UCI HAR Dataset/train/y_train.txt")
table(trainLabel)

trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")

testData <- read.table("UCI HAR Dataset/test/X_test.txt")
dim(testData) # 2947*561


testLabel <- read.table("UCI HAR Dataset/test/y_test.txt") 
table(testLabel) 


testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")

## Merge trainData and testData 

joinData <- rbind(trainData, testData)

dim(joinData) # 10299*561

## Merge trainLabel and testLabel

joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) # 10299*1

## Merge trainSubject and testSubject

joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) # 10299*1

###### Task 2: extract only the measurements on the mean and standard 
# deviation for each measurement

features <- read.table("UCI HAR Dataset/features.txt")
dim(features)  # 561*2
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
joinData <- joinData[, meanStdIndices]
dim(joinData) # 10299*66
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

###### Task 3: uses descriptive activity names to name the activities in 
# the data set

activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

###### Task 4: Appropriately labels the data set with descriptive activity 
# names


names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) # 10299*68

## Write out the first data set
write.table(cleanedData, "merged_data.txt") 

###### Task 5:  Create a second, independent tidy data set with the average of 
# each variable for each activity and each subject

subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)

## Write out the second data set

write.table(result, "tidydata.txt") 

## Read in the new tidy data set

data <- read.table("tidydata.txt")

head(data)

###### END OF THE SCRIPT