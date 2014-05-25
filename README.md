## Getting and Cleaning Human Activity Recognition Data Project
* * *
### Introduction

Experiments were carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, the research team at Smartlab (Non Linear Complex Systems Laboratory, Italy) captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz.

The obtained dataset was randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.Each subject was observed and measurements were recorded across multiple time windows, a vector of features was obtained by calculating variables from the time and frequency domain.

For each record following information was provided in the dataset:

* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject

* * *

### Objective

The objective of this project was to tidy up the publicly available raw data set, aggregate data and generate a clean data set that can be used for further analysis and studies. 

The following related artifacts are included in this repo project

* run_analysis.R - R script for cleaning the raw data files and generating a clean file

* CodeBook.md -    A reference file describing the variables in the tidy data set and the transformations applied to the raw data for generating the tidy data set.

* tidydata.txt -   Tidy data set containing the aggregated data that was generated by the run.analysis.R script

