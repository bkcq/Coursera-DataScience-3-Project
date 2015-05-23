# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
# The goal is to prepare tidy data that can be used for later analysis. 
# You will be graded by your peers on a series of yes/no questions related to the project.
# You will be required to submit: 
# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the analysis, and 
# 3) a code book that describes the variables, the data, and any transformations or work that you 
# performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts.
# This repo explains how all of the scripts work and how they are connected.  

# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
# Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
# The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
# A full description is available at the site where the data was obtained: 
  
#    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

# Here are the data for the project: 
  
#    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.#
#
# Good luck!



#Course 3 Assignment
run_analysis<-function(){
    require(reshape2)
    require(tidyr)
    require(dplyr)
    #get column names for the vector of training data
    trColNames<-read.table("UCI HAR Dataset/features.txt")
    #get the training dataset, assign colnames as we get it.
    trData<-read.table("UCI HAR Dataset/train/X_train.txt",sep="" ,
                       col.names=trColNames[,2])
    #get the training set activity codes
    trActivity<-read.table("UCI HAR Dataset/train/y_train.txt",sep="")
    #get the list of activities
    nameActivity<-read.table("UCI HAR Dataset/activity_labels.txt", sep="")
    #map activitycode and activities (temporary dataframe)
    z<-merge(trActivity,nameActivity)
    #assign names to the new frames columns
    colnames(z)<- c("ActivityCode","Activity")
    #bind to training set
    trData<-cbind(trData,z)
    #get the subject ID's 
    trSubj<-read.table("UCI HAR Dataset/train/subject_train.txt")
    #label column
    colnames(trSubj)<-"SubjectID"
    #bind to training data
    trData<-cbind(trData,trSubj)
    #get test dataset, assign colnames as we get it.
    testData<-read.table("UCI HAR Dataset/test/X_test.txt",sep="" ,
                       col.names=trColNames[,2])
    #get the test set activity codes
    testActivity<-read.table("UCI HAR Dataset/test/y_test.txt",sep="")
    
    #map activitycode and activities (temporary dataframe)
    z<-merge(testActivity,nameActivity)
    #assign names to the new frames columns
    colnames(z)<- c("ActivityCode","Activity")
    #bind to test set
    testData<-cbind(testData,z)
    #get the subject ID's 
    testSubj<-read.table("UCI HAR Dataset/test/subject_test.txt")
    #label column
    colnames(testSubj)<-"SubjectID"
    #bind to test data
    testData<-cbind(testData,testSubj)
    #get full dataset
    fullData<-rbind(trData,testData)
    #free structures no longer required.
    rm(testData)
    rm(nameActivity)
    rm(testActivity)
    rm(trActivity)
    rm(trColNames)
    rm(trData)
    rm(z)
    rm(trSubj)
    rm(testSubj)
    #this just leaves fullData.  We are only interested in the mean and stdev information
    #list of column ID's we are interested in
    y<-c(1:6,41:46,81:86,121:126,161:166,201,202,214,215,227,228,240,241,253,254
         ,266:271,294:296,345:350,373:375,424:429,452:454,503,504,529,530,542,543,
         552,556:564)
    #Note that 562-564 are the handles (ActivityCode Activity and SubjectID)
    
    trimmedData<-fullData[,y]
    trimmedData<-subset(trimmedData,select=-c(ActivityCode))
    trimmedData<-trimmedData%>%group_by(Activity,SubjectID)%>%summarise_each(funs(average=mean))
    write.table(trimmedData,file="Run_Analysis.txt",row.name=FALSE)
    #Now need to get the Means of Each column(1:80) by factors (Activity/SubjectID)
  # finalDF<-melt(setDT(trimmedData), id.vars = c("Activity","SubjectID"),
  #               measure.vars = 1:80)[, mean(value), by = c("Activity","SubjectID")]
   
  
}