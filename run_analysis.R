#load libraries
library(plyr) 
library(data.table) 
library(dplyr)

#path to dataset
data_path=file.path("./data","UCI HAR Dataset")
data_path_test=file.path(data_path,"test")
data_path_train=file.path(data_path,"train")
#Read Activity files 
data_activity_test <- read.table(file.path(data_path_test, "y_test.txt"),header=FALSE)
data_activity_train <- read.table(file.path(data_path_train,"y_train.txt"),header=FALSE)

#Read Subject files
data_subject_test <- read.table(file.path(data_path_test,"subject_test.txt"),header=FALSE)
data_subject_train <- read.table(file.path(data_path_train,"subject_train.txt"),header=FALSE)

#Read Features files
data_features_test <- read.table(file.path(data_path_test,"X_test.txt"),header=FALSE)
data_features_train <- read.table(file.path(data_path_train,"X_train.txt"),header=FALSE)


#Task 1 : Merges the training and the test sets to create one data set.
message(" 1) Merge the training and the test sets to create one data set. ")

data_subject <- rbind(data_subject_train,data_subject_test)
data_activty <- rbind(data_activity_train,data_activity_test)
data_features <- rbind(data_features_train,data_features_test)

names(data_subject)<-c("subject")
names(data_activty)<- c("activity")
data_features_names <- read.table(file.path(data_path, "features.txt"),head=FALSE)
names(data_features)<- data_features_names$V2


#final data
final_data <- cbind(data_subject,data_activty,data_features)

#Task 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
message("2) Extract only the measurements on the mean and standard deviation for each measurement. ")
# grep feature names with mean() or std()
sub_data_features_names<-data_features_names$V2[grep("mean\\(\\)|std\\(\\)", data_features_names$V2)]

#get the required selection of feature names
selected_names <- c("subject","activity",as.character(sub_data_features_names))

# final data with mean and std
final_data <- subset(final_data,select=selected_names)

#Task 3: Uses descriptive activity names to name the activities in the data set
message("3) Use descriptive activity names to name the activities in the data set")
#Read the activity labels
activity_labels <- read.table(file.path(data_path,"activity_labels.txt"),header=FALSE)

activity.ID = 1
# for loop to replace activity labels
for (act_label in activity_labels$V2) {
  final_data$activity <- gsub(activity.ID, act_label, final_data$activity)
  activity.ID <- activity.ID + 1
}


#Task 4: Appropriately labels the data set with descriptive variable names. 
message("4) Appropriately label the data set with descriptive variable names. ")

names(final_data)<-gsub("^t", "time", names(final_data))
names(final_data)<-gsub("^f", "frequency", names(final_data))
names(final_data)<-gsub("Acc", "Accelerometer", names(final_data))
names(final_data)<-gsub("Gyro", "Gyroscope", names(final_data))
names(final_data)<-gsub("Mag", "Magnitude", names(final_data))
names(final_data)<-gsub("BodyBody", "Body", names(final_data))

#Task 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

message("5) From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject. ")
tidy_data<-aggregate(. ~subject + activity, final_data, mean)
tidy_data<-tidy_data[order(tidy_data$subject,tidy_data$activity),]
write.table(tidy_data, file = "tidydata.txt",row.name=FALSE)
