## Getting-and-Cleaning-Data-Course-Project


# step 1: Merges the training and the test sets to create one data set.


setwd("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")


#load feature and activity data 
features <- read.table("./features.txt") 
activity_labels <- read.table("./activity_labels.txt") 
 

#load training data 
subject_train <- read.table("./train/subject_train.txt", header=F) 
X_train <- read.table("./train/X_train.txt", header=F) 
Y_train <- read.table("./train/Y_train.txt", header=F) 
 

#labels
colnames(activity_labels) <- c("ActivityID","ActivityType") 
colnames(subject_train) <- "SubjectID" 
colnames(Y_train) <- "ActivityID" 
colnames(X_train) <- features[,2] 


#combine training data 
training_data <- cbind(subject_train,Y_train,X_train) 
 
#load testing data 
subject_test <- read.table("./test/subject_test.txt", header=F) 
X_test <- read.table("./test/X_test.txt", header=F) 
Y_test <- read.table("./test/Y_test.txt", header=F) 
 

#labels
colnames(activity_labels) <- c("ActivityID","ActivityType") 
colnames(subject_test) <- "SubjectID" 
colnames(Y_test) <- "ActivityID" 
colnames(X_test) <- features[,2] 
 

#combine testing data 
testing_data <- cbind(subject_test,Y_test,X_test)


combined_data <- rbind(training_data,testing_data) 


column_names <- colnames(combined_data) 


# step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

mean_std_data <- combined_data[c(1,2,grep("*-mean*",column_names),grep("*-std*",column_names))]


# step 3: Uses descriptive activity names to name the activities in the data set

data_descr_activity <- merge(mean_std_data,activity_labels,by='ActivityID',all.x=TRUE);


# step 4: Appropriately labels the data set with descriptive variable names.

column_names <- colnames(data_descr_activity) 


for (i in 1:length(column_names))  
			 { 
			   column_names[i] = gsub("\\()","",column_names[i]) 
			   column_names[i] = gsub("-std$","StdDev",column_names[i]) 
			   column_names[i] = gsub("-mean","Mean",column_names[i]) 
			   column_names[i] = gsub("^(t)","time",column_names[i]) 
			   column_names[i] = gsub("^(f)","freq",column_names[i]) 
			   column_names[i] = gsub("([Gg]ravity)","Gravity",column_names[i]) 
			   column_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",column_names[i]) 
			   column_names[i] = gsub("[Gg]yro","Gyro",column_names[i]) 
			   column_names[i] = gsub("AccMag","AccMagnitude",column_names[i]) 
			   column_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",column_names[i]) 
			   column_names[i] = gsub("JerkMag","JerkMagnitude",column_names[i]) 
			   column_names[i] = gsub("GyroMag","GyroMagnitude",column_names[i]) 
			 } 
 

colnames(data_descr_activity) <- column_names 



# step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Create a new table, finalDataNoActivityType without the activityType column 
data_new <- data_descr_activity[,names(data_descr_activity) != 'ActivityType']; 
 

# Summarizing the data table to include just the mean of each variable for each activity and each subject 
tidyData <- aggregate(data_new[,3:length(names(data_new))],by=list(ActivityID=data_new$ActivityID,SubjectID = data_new$SubjectID),mean)
 

tidyData <- merge(tidyData,activity_labels,by='ActivityID',all.x=TRUE)
 
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t') 



