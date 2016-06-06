library(reshape2)
library(dplyr)

rm(list=ls())


filename <- "getdata_dataset.zip" 
## Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists(filename)) { 
  unzip(filename) 
}

 
#Load activity metadata.
activitymeta <- read.table(".\\UCI HAR Dataset\\activity_labels.txt")
#Apply variable names
colnames(activitymeta) <- c("activityid","activity")
#Load activity associated with each observation.
activity_train <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt")
activity_test <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt")
#Add environment variable {train,test}
activity_train <- cbind(environment="train",activity_train,stringsAsFactors=FALSE)
activity_test <- cbind(environment="test",activity_test,stringsAsFactors=FALSE)
#Union test and training datasets 
dfactivity <- rbind(activity_test,activity_train)
#Apply variable names
colnames(dfactivity) <- c("environment","activityid")
#Garbage collect
rm(activity_test,activity_train)


#Load subject associated with each observation.
subject_train <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt")
subject_test <-  read.table(".\\UCI HAR Dataset\\test\\subject_test.txt")
#Add environment variable {train,test}
subject_train <- cbind(environment="train",subject_train,stringsAsFactors=FALSE)
subject_test <- cbind(environment="test",subject_test,stringsAsFactors=FALSE)
#Union test and training datasets 
dfsubject <- rbind(subject_test,subject_train)
#Apply variable names
colnames(dfsubject) <- c("environment","subjectid")
#Garbage collect
rm(subject_train,subject_test)



#Load & apply observations metadata.
observation_meta <- read.table(".\\UCI HAR Dataset\\features.txt")
colnames(observation_meta) <- c("id","observation")
v<-tolower(as.character(observation_meta$observation))
v<-c("environment", v)
#Load observations.
observation_train <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt")
observation_test <-  read.table(".\\UCI HAR Dataset\\test\\X_test.txt")
#Add environment variable {train,test}
observation_train <- cbind(environment="train",observation_train,stringsAsFactors=FALSE)
observation_test <- cbind(environment="test",observation_test,stringsAsFactors=FALSE)
#Union test and training datasets 
dfobservation <- rbind(observation_test,observation_train)
#Apply variable names
colnames(dfobservation) <- v
#Garbage collect
rm(observation_meta,observation_train,observation_test,v)


#Union training and test 
dfobservation <- cbind(dfactivity, dfsubject, dfobservation)
#Normalize dataset with melt
x <- melt(data=dfobservation, id.vars=1:5, variable_name="variable")  #altered name featuredetail to variable


#Split observation / feature into attributes 
#(not required, but useful for additional analysis)
out1 <- strsplit(as.character(x$variable),"-")
out1 <- do.call(rbind, out1)
colnames(out1) <- c("key","calculation","axis")
out1 <- as.data.frame(out1)


out2 <- as.list(substring(out1$key, first = 1, last = 1))
out2 <- do.call(rbind, out2)
out2 <- as.data.frame(out2)
colnames(out2) <- c("signal")

out3 <- as.list(substring(out1$key, first = 2))
out3 <- do.call(rbind, out3)
out3 <- as.data.frame(out3)
colnames(out3) <- c("feature")

x <- cbind(out3, out2, out1, x)

#Extract only the measurements on the mean and standard deviation for each measurement.
data <- x[x$calculation %in% c("mean()","std()"),-c(3,8,10)]
data <- merge(data, activitymeta, id="activityid")

#Reorder columns for dataset readability
data <- data[,c(1,10,7,6,3,2,5,4,8,9)]
data[!(data$axis %in% c("z","y","x")), 7] <- NA
data[!(data$signal %in% c("f","t")), 5] <- NA
#Garbage collect
rm(activitymeta,out1, out2, out3)


#From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.
group_data <- group_by(data, activity, subjectid)


write.table(summarize(group_data,mean(value)), "observationsummary.txt", row.name=TRUE)




