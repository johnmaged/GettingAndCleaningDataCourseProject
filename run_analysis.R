## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 

# "UCI HAR Dataset" is put in the same R working directory
dataCurDir <- "./UCI HAR Dataset"

# Function to create (test or train) dataset
prepareData <- function(myData) {
  # data folder path
  dataDir <- paste(dataCurDir, "/", myData, sep="")
  
  # measure names
  measures <- read.table(paste(dataCurDir, "/features.txt", sep=""))
  names(measures) <- c("measureID", "measureName")
  
  # indices of measurements on the mean and standard deviation for each measurement
  tmeasure <- grep("mean|std", measures$measureName)
  
  # subject IDs
  subject <- read.table(paste(dataDir, "/subject_", myData, ".txt", sep=""))
  names(subject) <- c("subjectID")
  
  # activity IDs
  a_data <- read.table(paste(dataDir, "/y_", myData, ".txt", sep=""))
  # activity labels
  a_names <- read.table(paste(dataCurDir, "/activity_labels.txt", sep=""))
  # merge activity IDs and labels
  a_data <- merge(a_data, a_names)
  names(a_data) <- c("activityID", "activityName")
  
  # data of target measurements
  tm_data <- read.table(paste(dataDir, "/x_", myData, ".txt", sep=""))
  tm_data.subset <- tm_data[, tmeasure]
  names(tm_data.subset) <- measures$measureName[tmeasure]
  
  # return a tidy dataset
  data.frame(subject, a_data, tm_data.subset)
}

# tidy dataset
train.df <- prepareData("train")
test.df <- prepareData("test")
data.df <- rbind(train.df, test.df)


## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

library(reshape2)
vars <- c("activityID", "activityName", "subjectID")
measure.vars <- setdiff(colnames(data.df), vars)
data.melt <- melt(data.df, id=vars, measure.vars=measure.vars)
data.df2 <- dcast(data.melt, activityName + subjectID ~ variable, mean)

write.table(data.df2, file="tidydata.txt", row.name=FALSE)
