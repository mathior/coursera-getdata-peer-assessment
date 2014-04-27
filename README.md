Coursera "Getting and Cleaning Data" Peer Assessment
====================================================

Scripts and data for the peer assessment in the coursera course "Getting and Cleaning Data".

Task given
----------
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

You should create one R script called run_analysis.R that does the following.  
1. Merges the training and the test sets to create one data set.  
2. Extracts only the measurements on the mean and standard deviation for each measurement.  
3. Uses descriptive activity names to name the activities in the data set  
4. Appropriately labels the data set with descriptive activity names.  
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Original dataset
----------------
The "Human Activity Recognition Using Smartphones Data Set" is described here:  
<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>.  
An archive containing the data was provided here:  
<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>.  
In order to work correctly the run_analysis.R script has to be located inside the directory obtained by unpacking this archive.



Files
-----
  - run_analysis.R  
    The script performing the given task.
  - README.md  
    This file.
  - CodeBook.md  
    A description of the variables, data and transformation the sript does.


