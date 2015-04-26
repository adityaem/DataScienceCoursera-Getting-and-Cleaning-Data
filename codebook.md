##Script Overview: The process is broken up in to five key components.
##Step 1: Set the location of the data
##Step 2: Read --> Classify --> Merge the multiple data source files into a single file with the key dimensions of 
##Step 2: Activity, Subjects and Features with all related measures
##Step 3: Extracts only the measurements on the mean and standard deviation for each measurement and Review
##Step 4:Appropriately labels the data set with descriptive variable names
##Step 5: Aggregate the and subset the data to create the tidy data output
##Step 6: Generate the codebook

##Step 1 : Set the location of the data/ review available files
setwd("C:/coursera")
path_rf <- file.path("./" , "UCI HAR Dataset")

##Step 2: Based on file list reviewed in step 1 get the data for the relevant files for the project
##Step 2.a : Load the data into the 6 data groups - This is the lowest level of grouping
dataActivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)
dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
dataSubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)
dataFeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)

##Step 2b: Merge the subgroups created in Step 2 into the key dimensions

dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataActivity<- rbind(dataActivityTrain, dataActivityTest)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)

##Step 2c: Now give the data groups created valid names to clearly describe them
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)
names(dataFeatures)<- dataFeaturesNames$V2

##Step 2d: Create a single Merged Data Set for Analysis
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)

##Step 3: Extracts only the measurements on the mean and standard deviation for each measurement and Review
subdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
Data<-subset(Data,select=selectedNames)

##Step 4:Appropriately labels the data set with descriptive variable names
activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"),header = FALSE)
head(Data$activity,30)
str(path_rf)
str(activityLabels)
Data$activity <- as.character(Data$activity)
Data$activity[Data$activity == 1] <- "Walking"
Data$activity[Data$activity == 2] <- "Walking Upstairs"
Data$activity[Data$activity == 3] <- "Walking Downstairs"
Data$activity[Data$activity == 4] <- "Sitting"
Data$activity[Data$activity == 5] <- "Standing"
Data$activity[Data$activity == 6] <- "Laying"
Data$activity <- as.factor(Data$activity)
names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

##Step 5: Aggregate the and subset the data to create the tidy data output
library(plyr);
Data2<-aggregate(. ~subject + activity, Data, mean)
Data2<-Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)

##Step 6: Generate the codebook
code = readLines('CourseProjectCleaningData.R')
writeLines(code,"codebook.Rmd")
library(knitr)
knit2html("codebook.Rmd")

