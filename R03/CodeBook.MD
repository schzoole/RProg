# Getting and Cleaning Data Project 

Stephanie Hudson

#Study Details 
(Human Activity Recognition Using Smartphones Data Set )
## Description
Additional information about the variables, data and transformations used in the course project for the Johns Hopkins Getting and Cleaning Data course.

## Source Data
A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
[The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

## Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Attribute Information
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

#Project Details 

## Data Sources (relevant to this effort)
- features.txt:          metadata variable headings for observations.  
- activity_labels.txt:   metadata for activity.
- subject_train.txt:     subject observation id's  for the training environment.
- x_train.txt            raw observation value for the training environment.
- y_train.txt            raw activityid  for the training environment.
- subject_test.txt       subject observation id's  for the test environment.
- x_test.txt             raw observation value for the training environment.
- y_test.txt             subject observation id's  for the test environment.


##Data Model
For the above data sources and points, integration & normalization of data was required into the following model.
data.frame':	observation:
 -$ activityid : int         attribute indicating the integer activity id.  values {1,2,3,4,5,6}
 -$ activity   : Factor      attribute indicating the string activity type. values {"LAYING","SITTING",..}
 -$ subjectid  : int         attribute indicating human participating subject id
 -$ environment: chr         attribute indicating the study environment; value {test or train}  
 -$ signal     : Facactor    attribute indicating signal type; t (time) or f (frequency)
 -$ feature    : Factor      attribute indicating observation feature; 
 -$ axis       : Factor      attribute indicating the axis plane; x, y or z
 -$ calculation: Factor      attribute indicating what calculation was measured.
 -$ variable   : Factor      staging attribute indicating the pre normalized string.value  signal-feature-calculation-axis*
                            *pattern applies to 95% of domain values.
 -$ value      : num         measure indicating observation value, range from 0 to 1 
                            Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                            0.2572  0.2703  0.2748  0.2728  0.2755  0.2860 



## Section 1. Stage Data
Load & integrate the training and the test sets for each set of files including; activity, subject, and feature.  Apply metadata to dataframes.

## Section 2. Merge Data 
Merge activity, subject, and observations into a single datasest.  Begin to normalize the observations with melt().  

## Section 3. Normalize 
Normalize observations.variable with split() from pre normalized string.value  signal-feature-calculation-axis.

## Section 4. Reduce 
Reduce observations for calucations mean() and std().   Reorder columns for readability. 

## Section 5.  Group 
Group dataframe using activity and subject as partitions.  

## Section 6.  Write to file 
Write group by output to file. 

