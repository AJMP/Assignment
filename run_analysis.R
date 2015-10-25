#Coursera | Getting and Cleaning Data |Course Project

#The project is divided in 5 steps, listed below:
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##################################################################################################################################################

#cleaning the environment and setting the worlplace
list <- ls()
rm(list)
setwd("/Users/andrejmp/Desktop/My Docs/Programing/R/Cleaning Data/Course Project")

#1. Merges the training and the test sets to create one data set.
# Read in the data from TRAIN files
features_DF <- read.table("./Downloads/UCI HAR Dataset/features.txt", header = FALSE)
activities_DF <- read.table("./Downloads/UCI HAR Dataset/activity_labels.txt", header = FALSE)
subjectTrain_DF <- read.table('./Downloads/UCI HAR Dataset/train/subject_train.txt', header = FALSE) 
xTrain_DF <- read.table('./Downloads/UCI HAR Dataset/train/x_train.txt', header = FALSE)
yTrain_DF <- read.table('./Downloads/UCI HAR Dataset/train/y_train.txt', header = FALSE)

# Assign collum names for every collum
colnames(activities_DF)  = c('Activity_Id','Activity_Typology')
colnames(subjectTrain_DF)  = "Subject_Id"

colnames(xTrain_DF)        = features_DF[,2] 
colnames(yTrain_DF)        = "Activity_Id"

# Create the final TRAIN set by merging yTrain, subjectTrain, and xTrain
train_DF <- cbind(yTrain_DF,subjectTrain_DF,xTrain_DF)

# Read in the data from TEST files
subjectTest_DF <- read.table('./Downloads/UCI HAR Dataset/test/subject_test.txt', header = FALSE) 
xTest_DF <- read.table('./Downloads/UCI HAR Dataset/test/x_test.txt', header = FALSE)
yTest_DF <- read.table('./Downloads/UCI HAR Dataset/test/y_test.txt', header = FALSE)

# Assign column names to the TEST data imported above
colnames(subjectTest_DF) = "Subject_Id"
colnames(xTest_DF)       = features_DF[,2] 
colnames(yTest_DF)       = "Activity_Id"

# Create the final test set by merging the xTest, yTest and subjectTest data
test_DF <- cbind(yTest_DF,subjectTest_DF,xTest_DF)

# CREATE THE BIG DATASET
MyData <- rbind(train_DF,test_DF);

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# Create a vector with name of collum so it can be created a vector identyfing the mean and standard deviatio collums 
CNames_Vector <- c(1:8, 43:48, 83:88, 123:128, 163:168, 203:204, 216:217, 229:230, 255:256, 268:273, 296:298, 347:352, 375:377, 426:431, 454:457, 505:506, 515, 518, 528, 531:532, 541, 544:545, 554, 557:563)

# Subseting "MyData" with only MEAN and STD collums
Mydata2 <- MyData[,CNames_Vector]
dim(Mydata2)

#3. Uses descriptive activity names to name the activities in the data set
# MAPPING the type of activity (walking, etc...) with the activity type ID
MyData3 <- merge(Mydata2,activities_DF,by='Activity_Id',all.x=TRUE);

# Set up new collum Names on MYDATA3
CNames_Mydata3 <- colnames(MyData3)

#4. Appropriately labels the data set with descriptive variable names. 
for (i in 1:length(CNames_Mydata3)) {
  CNames_Mydata3[i] = gsub("\\()","",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("-std$","StdDev",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("-mean","Mean",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("^(t)","Time",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("^(f)","Frequency",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("([Gg]ravity)","Gravity",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("[Gg]yro","Gyro",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("AccMag","Acc_Magnitude",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("([Bb]odyaccjerkmag)","Body_Acc_Jerk_Magnitude",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("JerkMag","Jerk_Magnitude",CNames_Mydata3[i])
  CNames_Mydata3[i] = gsub("GyroMag","Gyro_Magnitude",CNames_Mydata3[i])
}
# Rename collum names of MYDATA3
CNames_Mydata3 <- colnames(MyData3)

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Create a new table, NoAT_DF without the activityType column so that it can be summarized next
NoAT_DF <- MyData3[,names(MyData3) != 'activityType']

# Summarizing by including the mean function for each collum and row
tidyData  <- aggregate(NoAT_DF[,names(NoAT_DF) != c('Activity_Id','Subject_Id')],by=list(Activity_Id=NoAT_DF$Activity_Id,Subject_Id = NoAT_DF$Subject_Id),mean)

# Complete the file tidyData
tidyData <- merge(tidyData,activities_DF,by='Activity_Id',all.x=TRUE)

# Export the tidyData set in the way indicated on course assignment 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
