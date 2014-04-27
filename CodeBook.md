coursera-getdata-peer-assessment
================================

Datasets
--------
  1. human-activity-recognition-\<timestamp\>.txt  
     Contains all columns with mean and standard deviation variables from the
     original [https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](Human Activity Recognition Using Smartphones Data Set) as well as subject id and activity label.
  2. human-activity-recognition-average-\<timestamp\>.txt  
     Contains the averaged measurements for mean and standard deviation variables,
     average is computed by subject and activity.

Both datasets are actually in csv format (but the coursera uploader didn't
accept files ending in .csv).

These datasets are modified versions of the [https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](Human Activity Recognition Using Smartphones Data Set) (a description can be found here: <http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>). The readme text of the original dataset is included as a reference at the bottom of this file.


### Description of the modifications as performed by the run_analysis.R script
  - Measurements from the test and train data set are combined into one data set.
    The result is a table with 2947 (test) + 7352 (train) = 10299 rows and 561
    columns.
  - The feature names are loaded and set as column names.
  - The dataset is reduced to just the columns containing measurements for mean
    and standard deviation variables (86 columns).
  - The subject ids from the test and train data set are loaded and added as a
    new column named "subjectId".
  - The activity labels from the test and train data set are loaded and added as
    a new column named "activity".
  - The column names are normalized to account for problems that occured during
    writing and reading csv files. The normalization replaces all of the following
    characters '(),-' with underscores '_', then reduces multiple underscores
    to one and stripping trailing underscores.
  - The resulting dataset is available in the R workspace as a data frame named
    "harDF", a serialized version in csv format is human-activity-recognition-\<timestamp\>.txt.
  - A second data frame with the average of each variable for each activity and
    each subject is computed and available in the R workspace named "harAvgDF",
    a serialized version in csv format is human-activity-recognition-average-\<timestamp\>.txt.

Both datasets have the same column (feature) names. A description of the features as given in the features_info.txt file of the original dataset is included at the end of this file as a reference.

The 86 selected mean and standard deviation features for both datasets are:

    tBodyAcc_mean_X
    tBodyAcc_mean_Y
    tBodyAcc_mean_Z
    tBodyAcc_std_X
    tBodyAcc_std_Y
    tBodyAcc_std_Z
    tGravityAcc_mean_X
    tGravityAcc_mean_Y
    tGravityAcc_mean_Z
    tGravityAcc_std_X
    tGravityAcc_std_Y
    tGravityAcc_std_Z
    tBodyAccJerk_mean_X
    tBodyAccJerk_mean_Y
    tBodyAccJerk_mean_Z
    tBodyAccJerk_std_X
    tBodyAccJerk_std_Y
    tBodyAccJerk_std_Z
    tBodyGyro_mean_X
    tBodyGyro_mean_Y
    tBodyGyro_mean_Z
    tBodyGyro_std_X
    tBodyGyro_std_Y
    tBodyGyro_std_Z
    tBodyGyroJerk_mean_X
    tBodyGyroJerk_mean_Y
    tBodyGyroJerk_mean_Z
    tBodyGyroJerk_std_X
    tBodyGyroJerk_std_Y
    tBodyGyroJerk_std_Z
    tBodyAccMag_mean
    tBodyAccMag_std
    tGravityAccMag_mean
    tGravityAccMag_std
    tBodyAccJerkMag_mean
    tBodyAccJerkMag_std
    tBodyGyroMag_mean
    tBodyGyroMag_std
    tBodyGyroJerkMag_mean
    tBodyGyroJerkMag_std
    fBodyAcc_mean_X
    fBodyAcc_mean_Y
    fBodyAcc_mean_Z
    fBodyAcc_std_X
    fBodyAcc_std_Y
    fBodyAcc_std_Z
    fBodyAcc_meanFreq_X
    fBodyAcc_meanFreq_Y
    fBodyAcc_meanFreq_Z
    fBodyAccJerk_mean_X
    fBodyAccJerk_mean_Y
    fBodyAccJerk_mean_Z
    fBodyAccJerk_std_X
    fBodyAccJerk_std_Y
    fBodyAccJerk_std_Z
    fBodyAccJerk_meanFreq_X
    fBodyAccJerk_meanFreq_Y
    fBodyAccJerk_meanFreq_Z
    fBodyGyro_mean_X
    fBodyGyro_mean_Y
    fBodyGyro_mean_Z
    fBodyGyro_std_X
    fBodyGyro_std_Y
    fBodyGyro_std_Z
    fBodyGyro_meanFreq_X
    fBodyGyro_meanFreq_Y
    fBodyGyro_meanFreq_Z
    fBodyAccMag_mean
    fBodyAccMag_std
    fBodyAccMag_meanFreq
    fBodyBodyAccJerkMag_mean
    fBodyBodyAccJerkMag_std
    fBodyBodyAccJerkMag_meanFreq
    fBodyBodyGyroMag_mean
    fBodyBodyGyroMag_std
    fBodyBodyGyroMag_meanFreq
    fBodyBodyGyroJerkMag_mean
    fBodyBodyGyroJerkMag_std
    fBodyBodyGyroJerkMag_meanFreq
    angle_tBodyAccMean_gravity
    angle_tBodyAccJerkMean_gravityMean
    angle_tBodyGyroMean_gravityMean
    angle_tBodyGyroJerkMean_gravityMean
    angle_X_gravityMean
    angle_Y_gravityMean
    angle_Z_gravityMean

The format of all these features is numerical.

The additional columns are:

    subjectId
Numerical subject id.

    activity
Activity label as Factor.





README.txt from the original dataset
====================================

## Human Activity Recognition Using Smartphones Dataset
Version 1.0

Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws


The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:


- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

## The dataset includes the following files:

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

## Notes: 
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

## License:
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.




features_info.txt from the original dataset
===========================================

## Feature Selection 

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

    tBodyAcc-XYZ
    tGravityAcc-XYZ
    tBodyAccJerk-XYZ
    tBodyGyro-XYZ
    tBodyGyroJerk-XYZ
    tBodyAccMag
    tGravityAccMag
    tBodyAccJerkMag
    tBodyGyroMag
    tBodyGyroJerkMag
    fBodyAcc-XYZ
    fBodyAccJerk-XYZ
    fBodyGyro-XYZ
    fBodyAccMag
    fBodyAccJerkMag
    fBodyGyroMag
    fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

    mean(): Mean value
    std(): Standard deviation
    mad(): Median absolute deviation 
    max(): Largest value in array
    min(): Smallest value in array
    sma(): Signal magnitude area
    energy(): Energy measure. Sum of the squares divided by the number of values. 
    iqr(): Interquartile range 
    entropy(): Signal entropy
    arCoeff(): Autorregresion coefficients with Burg order equal to 4
    correlation(): correlation coefficient between two signals
    maxInds(): index of the frequency component with largest magnitude
    meanFreq(): Weighted average of the frequency components to obtain a mean frequency
    skewness(): skewness of the frequency domain signal 
    kurtosis(): kurtosis of the frequency domain signal 
    bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
    angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

    gravityMean
    tBodyAccMean
    tBodyAccJerkMean
    tBodyGyroMean
    tBodyGyroJerkMean

