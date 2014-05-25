# 5/25/2014 Coursera/Data Science/Getting and Cleaning Data/Course Project
# Script for processing Human Activity Recognition Using Smartphones Dataset
# Script will attempt to load data from "UCI HAR Dataset/" folder. If your dataset is in diferent path, 
# you can use argument <data_path> and specify path to dataset.
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

### Package dependencies check
if (!require("data.table")) {install.packages("data.table") require("data.table")}
if (!require("reshape2")) {install.packages("reshape2") require("reshape2")}

# Function to load data and merge it with labels. Can be used both with test/train
load_data <- function(x_file, y_file, subj_file, features, activity) {
    # Saving labels that we need for processing
    k_features <- grepl("mean|std", features)
    
    # Loading whole data file
    x_dat <- read.table(x_file)
    names(x_dat) = features
    
    # Filtering only mean and std columns
    x_dat = x_dat[,k_features]
    
    # Reading activities data
    y_dat <- read.table(y_file)
    y_dat[,2] = activity[y_dat[,1]]
    names(y_dat) = c("Activity_ID", "Activity_Label")
    
    # Reading subject IDs data
    subject <- read.table(subj_file)
    names(subject) = "subject"
    
    # Linking all columns toghether
    cbind(as.data.table(subject), y_dat, x_dat)
}

# Main function for processing dataset
run <- function(data_path = "UCI HAR Dataset/") {
    # Loading data column names for later use
    features <- read.table(paste(data_path,"features.txt", sep="")) [,2]
    # Loading names of activities
    activity <- read.table(paste(data_path,"activity_labels.txt", sep="")) [,2]
    
    # Loading test and train data using internal function
    test_data = load_data(x_file = paste(data_path,"test/X_test.txt", sep=""), y_file = paste(data_path,"test/y_test.txt", sep=""), subj_file = paste(data_path,"test/subject_test.txt", sep=""), features, activity)
    trai_data = load_data(x_file = paste(data_path,"train/X_train.txt", sep=""), y_file = paste(data_path,"train/y_train.txt", sep=""), subj_file = paste(data_path,"train/subject_train.txt", sep=""), features, activity)
    
    # Merging test and train data
    dat = rbind(test_data, trai_data)
    
    # Aggregating data grouping by id_labels
    id_labels   = c("subject", "Activity_ID", "Activity_Label")
    data_labels = setdiff(colnames(dat), id_labels)
    redata      = melt(dat, id = id_labels, measure.vars = data_labels)
    # Applying mean function to dataset
    tidy_data   = dcast(redata, subject + Activity_Label ~ variable, mean)
    
    write.table(tidy_data, file = paste(data_path,"tidy_data.txt", sep=""))
}

# Start of processing 
run()
