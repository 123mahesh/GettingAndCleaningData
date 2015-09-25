###############################################################
# DOWNLOAD THE REQUIRED DATA                                  #
# LOAD THE LIBRARY                                            #
###############################################################

library(data.table)
library(dplyr)

#if there is no folder called 'data' in working directory create it
if(!dir.exists("./data"))
  {dir.create("./data")}

#data source 
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#download the zip fiel in Dataset.zip
download.file(Url,destfile="./data/Dataset.zip")

#unzip the file in 'data' folder
unzip(zipfile="./data/Dataset.zip",exdir="./data")

###############################################################
# Read data in variables                                      #
###############################################################

#Read Supporting Metadata
featureNames <- read.table("./data/UCI HAR Dataset/features.txt")
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Read training data
subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Read test data
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header = FALSE)



###############################################################
# PART 1                                                      #
# MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET#
###############################################################

#Combine the respective data in training and test data sets corresponding to 
#subject, activity and features. The results are stored in subject, activity and features.
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Columns in the features data set can be named from the metadata in featureNames
colnames(features) <- t(featureNames[2])

#Merge the data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

###############################################################
# PART 2                                                      #
# Extracts only the measurements on the mean and standard     #
# deviation for each measurement.                             #
###############################################################

#Extract the column indices that have either mean or std in them.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

#Add activity and subject columns to the list and look at the dimension of completeData
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

#create extractedData with the selected columns in requiredColumns
extractedData <- completeData[,requiredColumns]

###############################################################
# PART 3                                                      #
# Uses descriptive activity names to name the activities in   #
# the data set                                                #
###############################################################

#activity field in extractedData is originally of numeric type. 
#change its type to character so that it can accept activity names.
extractedData$Activity <- as.character(extractedData$Activity)

#The activity names are taken from metadata activityLabels
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

#factor the activity variable, once the activity names are updated.
extractedData$Activity <- as.factor(extractedData$Activity)

###############################################################
# PART 4                                                      #
# Appropriately labels the data set with descriptive variable #
# names.                                                      #
###############################################################


#Acc can be replaced with Accelerometer
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))

#Gyro can be replaced with Gyroscope
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))

#BodyBody can be replaced with Body
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))

#Mag can be replaced with Magnitude
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))

#Character f can be replaced with Frequency
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))

#Character t can be replaced with Time
names(extractedData)<-gsub("^t", "Time", names(extractedData))

#Character tBody can be replaced with TimeBody
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))

#Word 'Mean' to represent mean
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)

#Word 'STD' to represent standard deviation
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)

#Word 'Frequency' to represent frenquency
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)

#Word 'Angle' to represent angle
names(extractedData)<-gsub("angle", "Angle", names(extractedData))

#Word 'Gravity' to represent gravity
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

###############################################################
# PART 5                                                      #
# From the data set in step 4, creates a second, independent  #
#tidy data set with the average of each variable for each     #
#activity and each subject.                                   #                       
###############################################################

# set Subject as a factor variable
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#create tidyData as a data set with average for each activity and subject
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

#order the enties in tidyData and write it into data file Tidy.txt
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
