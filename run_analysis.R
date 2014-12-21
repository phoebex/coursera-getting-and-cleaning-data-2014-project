
run_analysis <- function() {


	library(dplyr)

	##################################################################
	# 0.1 Set up file names and file paths
	##################################################################


	data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	data_dir_path <- "./data"
	data_path <- file.path(data_dir_path, "dataset.zip")
	dataset_path <- file.path("./UCI HAR Dataset")

	training_set_path <- file.path(dataset_path, "train")
	test_set_path <- file.path(dataset_path, "test")

	test_features_path <- file.path(test_set_path, "X_test.txt")
	training_features_path <- file.path(training_set_path, "X_train.txt")

	test_labels_path <- file.path(test_set_path, "y_test.txt")
	training_labels_path <- file.path(training_set_path, "y_train.txt")

	training_subject_path <- file.path(training_set_path, "subject_train.txt")
	test_subject_path <- file.path(test_set_path, "subject_test.txt")

	feature_names_path <- file.path(dataset_path, "features.txt");
	activity_labels_path <- file.path(dataset_path, "activity_labels.txt");


	##################################################################
	# 0.1 Downloads and extracts data
	##################################################################

	# Create data directory if it does not exist
	if(!file.exists(data_dir_path)) {
		dir.create(file.path(data_dir_path))
	}

	# If data file does not exist, download it
	if(!file.exists(data_path)) {
		print("Download dataset file")
		
		if(Sys.info()["sysname"] == "Windows") {
			download.file(data_url, destfile = data_path)
		} else {
			download.file(data_url, destfile = data_path, method = "curl")
		}

		print("Data file downloaded")

	} else {
	  
	  print("Data file exists")
	}

	# Extract data from compressed file
	if(!file.exists(dataset_path)) {

		unzip(data_path)
		print("Data file unzipped")
	}


	#######################################################################
	# 1. Merges the training and the test sets to create one data set.
	#######################################################################

	# Read data into data frames
	features_names <- read.table(feature_names_path, colClasses = "character", stringsAsFactors = FALSE)
	activity_label_map <- read.table(activity_labels_path, stringsAsFactors = FALSE, col.names = c("labelInt", "labelString"))
	test_features <- read.table(test_features_path, colClasses = "numeric", col.names = features_names[, 2], check.names = TRUE)
	train_features <- read.table(training_features_path, colClasses = "numeric", col.names = features_names[, 2], check.names = TRUE)
	test_labels <- read.table(test_labels_path, colClasses = "integer", col.names = c("Activity"))
	train_labels <- read.table(training_labels_path, colClasses = "integer", col.names = c("Activity"))
	subject_test <- read.table(test_subject_path, colClasses = "integer", col.names = c("Subject"))
	subject_train <- read.table(training_subject_path, colClasses = "integer", col.names = c("Subject"))

	# Merge training and test data together
	features_table <- rbind(test_features, train_features)
	activities_table <- rbind(test_labels, train_labels)
	subject_table <- rbind(subject_test, subject_train)


	################################################################################
	# 3. Uses descriptive activity names to name the activities in the data set
	################################################################################

	# rename activities
	activities_table <- mutate(tbl_df(activities_table), Activity = activity_label_map[Activity, 2])


	#################################################################################################
	# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
	#################################################################################################

	features_mean_std <- features_table[, grep("mean\\(|std\\(", features_names[, 2])]


	###################################################################
	# 4. Appropriately labels the data set with descriptive names.
	###################################################################

	# merge datasets
	merged_dataframe <- cbind(subject_table, activities_table, features_mean_std)

	# create column names vector
	column_names_merged <- c("Subject", "Activity", features_names[grep("mean\\(|std\\(", features_names[, 2]), 2])

	# remove parentheses from names
	column_names_merged <- gsub("\\(\\)", "", column_names_merged)

	# replace column names with more descriptive ones
	names(merged_dataframe) <- column_names_merged


	####################################################################################################################################################
	# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject from the data set in step 4
	####################################################################################################################################################

	column_means <- summarise_each(group_by(merged_dataframe, Subject, Activity), funs(mean))
	write.table(column_means, "tidy_data.txt", row.name=FALSE, sep="\t")
	column_means
}






