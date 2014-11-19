##      Getting & Cleaning Data Course Projects
##      Brian Ostrovsky

##      Data Source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

##      This R script called run_analysis.R that does the following. 
##      1. Merges the training and the test sets to create one data set.
##      2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##      3. Uses descriptive activity names to name the activities in the data set
##      4. Appropriately labels the data set with descriptive variable names. 
##      5. From the data set in step 4, creates a second, independent tidy data set with the 
##         average of each variable for each activity and each subject.


##      1. Merges the training and the test sets to create one data set.

## Load activity names and features data files
activity_labels <- read.table("./activity_labels.txt", quote="\"")
features <- read.table("./features.txt", quote="\"")


## Load train data files - full path "~/Desktop/Getting & Cleaning Data/UCI HAR Dataset/train/"
subject_train <- read.table("./train/subject_train.txt", quote="\"")
X_train <- read.table("./train/X_train.txt", quote="\"")
y_train <- read.table("./train/y_train.txt", quote="\"")

## Create column names in train data set
colnames(activity_labels)  = c('activityId','activityType')
colnames(subject_train)  = "subjectId"
colnames(X_train)        = features[,2]
colnames(y_train)        = "activityId"

## Merges y_train, X_train and subject_train into a single combinedTrainData dataset
combinedTrainData  <- cbind(y_train, X_train, subject_train)

## Load test data files - full path "~/Desktop/Getting & Cleaning Data/UCI HAR Dataset/test/"
subject_test <- read.table("./test/subject_test.txt", quote="\"")
X_test <- read.table("./test/X_test.txt", quote="\"")
y_test <- read.table("./test/y_test.txt", quote="\"")

## Create column names in test data set
colnames(subject_test) = "subjectId"
colnames(X_test)       = features[,2]
colnames(y_test)       = "activityId"

## Merges y_train, X_train and subjectTrain into a single combinedTrainData dataset
combinedTestData  <- cbind(y_test, X_test, subject_test)

## Combines training and test data sets into allData
allData  <- rbind(combinedTrainData, combinedTestData)

##      2. Extracts only the measurements on the mean and standard deviation for each measurement. 
colNames  = colnames(allData)
subsetData = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))


##      3. Uses descriptive activity names to name the activities in the data set
allData = allData[subsetData==TRUE]
allData = merge(allData,activity_labels,by='activityId',all.x=TRUE)


##      4. Appropriately labels the data set with descriptive variable names. 
columnNamesList  = colnames(allData)
for (i in 1:length(columnNamesList)) 
{
        columnNamesList[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columnNamesList[i])
        columnNamesList[i] = gsub("^(t)","Time",columnNamesList[i])
        columnNamesList[i] = gsub("^(f)","Frequency",columnNamesList[i])
        columnNamesList[i] = gsub("\\()","",columnNamesList[i])
}
colnames(allData) = columnNamesList

##      5. From the data set in step 4, creates a second, independent tidy data set with the 
##         average of each variable for each activity and each subject.
meanData = aggregate(allData, by= list(allData$activityId,allData$subjectId),FUN=mean, na.rm=TRUE)
meanData$activityType  <- NULL 
meanData$subjectId  <- NULL
write.table(meanData, './meanData.txt',row.names=TRUE,sep='\t')
