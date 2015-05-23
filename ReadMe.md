---
title: "ReadMe"
output: html_document
---
The included Run_analysis.R script takes the UCI Machine Learning Dataset for <i>Human Activity Recognition Using Smartphones Data Set</i> from <a href="https://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"> here </a> and calculates the average value of all 'mean' and 'standard deviation' measurements against the activity type and subject.


The first steps are to load the required libraries, and read in the first part of the dataset assumed to be extracted in a folder in the working directory:

``` r 
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
```

We have loaded the training set, and assigned the 'activity_labels' to the appropriate activity.  We have also matched the 'SubjectID''s  to the data.

Next we essentially do the same with the test data set, merge the two data sets into one, and then clean up after ourselves.
<i> (my wife just rolled her eyes....)</i>

``` r
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
```

While it is possible to use gsub to extract columns with 'mean' and 'stdev' in their name, I thought I'd do it explicitly, just for the pain.

After 'trimming' the Data, we group it by activity and subject, and then summarise each column, and write the resulting table to file.

``` r
  #this just leaves fullData.  We are only interested in the mean and stdev information
    #list of column ID's we are interested in
    #The long and stupid way
    y<-c(1:6,41:46,81:86,121:126,161:166,201,202,214,215,227,228,240,241,253,254
         ,266:271,294:296,345:350,373:375,424:429,452:454,503,504,529,530,542,543,
         552,556:564)
    #Note that 562-564 are the handles (ActivityCode Activity and SubjectID), and ActivityCode is redundant.
    
    trimmedData<-fullData[,y]
    trimmedData<-subset(trimmedData,select=-c(ActivityCode)) #remove the redundant column.
    trimmedData<-trimmedData%>%group_by(Activity,SubjectID)%>%summarise_each(funs(average=mean)) #summarise each column by Activity and SubjectID
    write.table(trimmedData,file="Run_Analysis.txt",row.name=FALSE)  #and..... we're done.
   
  
}
```