#Read the raw data assuming the dir UCI HAR Dataset is unzipped and in the working directory
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

#Merge the test and training data
X <- rbind(X_test,X_train)
y <- rbind(y_test,y_train)
subject <- rbind(subject_test,subject_train)

#Remove unnecessary variables
rm(X_test,X_train,y_test,y_train,subject_test,subject_train)

#Create a mask for only those features with mean() or std() in the name
meanMask <-  agrep("mean()",as.character(features[,2]))
stdMask <-  agrep("std()",as.character(features[,2]))

#Eliminate data that does not fit the masks we just created
X <- X[, sort(c(meanMask,stdMask))]

#Change the column names to match those given by features file and more descriptive subject title
colnames(X) <- as.character(features[sort(c(meanMask,stdMask)),2])
colnames(subject) <- "subject"

#Combine X, y, and subjects into one clean unified data set
activity <- activity_labels[y[,1],2]
ActivityData <- cbind(subject,activity,X)

#Remove unnecessary variables
rm(features,X,y,subject,activity_labels,meanMask,stdMask,activity)

#Initialize a dataframe to store mean values we will delete this row at the end
MeanActivityData <- ActivityData[1,]

#Create a list of unique subjects, cycle through that list for each activity, then calc the mean of each column for that subject/activity pair
subjects <- sort(unique(ActivityData$subject))
for (i in subjects)
{
  activites <- unique(ActivityData[ActivityData$subject==i,2])
  for(j in activites)
  {
    subset <- ActivityData[((ActivityData$subject==i)&(ActivityData$activity==j)),]
    MeanActivityData <- rbind(MeanActivityData,c(i,j,colMeans(subset[,3:68])))
  }
}

#Delete dummy first row
MeanActivityData <- MeanActivityData[-1,]

#Remove unnecessary variables
rm(subset, i, j, subjects, activites)

#Write out the file to the working directory
write.table(MeanActivityData,file="MeanActivityData.txt",row.names=FALSE)

