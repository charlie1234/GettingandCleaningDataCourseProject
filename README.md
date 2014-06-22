Getting and Cleaning Data Course Project
========================================
This file describes how run_analysis.R script works.

* First, unzip the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip in a folder named "UCI HAR Dataset". Make sure the folder "UCI HAR Dataset" and the run_analysis.R script are both in the current working directory.

* Second, use source("run_analysis.R") command in RStudio.

* Third, you will find two output files are generated in the current working directory:

- merged_data.txt (7.9 Mb): it contains a data frame called cleanedData with 10299*68 dimension.

- tidydata.txt (220 Kb): it contains a data frame called result with 180*68 dimension.

* Finally, use data <- read.table("tidydata.txt") command to read the file. Since there is a requirement to get the average of each variable for each activity and each subject, and there are 6 activities in total and 30 subjects in total, we have 180 rows with all combinations for each of the 66 features. 
