library(data.table)
setwd("/Users/ArandaJaegar/Desktop/Coursera/Getting_and_cleaning_data/Week\4/Cleanning_data_project")

#Features
features <- read.csv("./UCI HAR Dataset/features.txt", header = FALSE, sep = ' ')
features <- as.character(features[,2])
#Train Data
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.csv("./UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = ' ')
subject_train <- read.csv("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = ' ')
data_train <- data.frame(subject_train, y_train, x_train)
names(data_train) <- c(c('subject','activity'),features)
#Test Data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.csv("./UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = ' ')
subject_test <- read.csv("./UCI HAR Datasettest/subject_test.txt", header = FALSE, sep = ' ')
data_test <- data.frame(subject_test, y_test, x_test)
names(data_test) <- c(c('subject','activity'),features)

#1)Merges the training and the test sets to create one data set.
complete_data <- rbind(data_train, data_test)

#2)Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_std <- grep('mean|std', features)
data_sub <- complete_data[,c(1,2,mean_std + 2)]

#3)Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
activity_labels <- as.character(activity_labels[,2])
data_sub[,2] <- activity_labels[data_sub[,2]]

#4)Appropriately labels the data set with descriptive variable names. 
new_names <- names(data_sub)
new_names <- gsub("[(][)]","", new_names)
new_names <- gsub("^t","TimeDomain_", new_names)
new_names <- gsub("^f","FrequencyDomain_", new_names)
new_names <- gsub("Acc","Accelerometer", new_names)
new_names <- gsub("Gyro","Gyroscope", new_names)
new_names <- gsub("Mag","Magnitude", new_names)
new_names <- gsub("-mean-","_Mean_", new_names)
new_names <- gsub("-std-","_StandardDeviation_", new_names)
new_names <- gsub("-","_", new_names)
names(data_sub) <- new_names

#5)From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Tidy_data <- aggregate(data_sub[,3:81], by = list(activity = data_sub$activity, subject = data_sub$subject), FUN = mean)
write.table(x = Tidy_data, file = "tidy_data.txt", row.names = FALSE)

