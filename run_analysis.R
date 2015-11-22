library("dplyr")
library("tidyr")

# download
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
data <- download.file(url, "dataset.zip")
unzip("dataset.zip")

# read
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt")
x.train <- read.table("UCI HAR Dataset/train/X_train.txt")
x.test <- read.table("UCI HAR Dataset/test/X_test.txt")
y.train <- read.table("UCI HAR Dataset/train/Y_train.txt")
y.test <- read.table("UCI HAR Dataset/test/Y_test.txt")
features <- read.table("UCI HAR Dataset/features.txt")
activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt") [, -1]


# Merges the training and the test sets to create one data set.
colnames(x.train) <- features$V2
colnames(x.test) <- features$V2

train <- data.frame('subject' = subject.train$V1, 'activity' = y.train$V1, as.matrix(x.train))
test <- data.frame('subject' = subject.test$V1, 'activity' = y.test$V1, as.matrix(x.test))

data <- arrange(rbind(train, test), subject)
 

# Extracts only the measurements on the mean and standard deviation for each measurement. 
data.sub <- select(data, subject, activity, matches("\\.std\\.|\\.mean\\.")) 

# Uses descriptive activity names to name the activities in the data set
data.sub <- mutate(data.sub, activity = activity.labels[activity])

# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data.sub <- data.sub %>% 
        gather(name, value, -subject, -activity) %>% 
        mutate(name = gsub("^t", "time.", name)) %>%
        mutate(name = gsub("^f", "frequency.", name)) %>%
        mutate(name = gsub("Acc", "Accelerometer.", name)) %>%
        mutate(name = gsub("Gyro", "Gyroscope.", name)) %>%
        mutate(name = gsub("BodyBody", "Body", name)) %>%
        mutate(name = gsub("Body", "Body.", name)) %>%
        mutate(name = gsub("Gravity", "Gravity.", name)) %>%
        separate(name, c("magnitude", "acceleration", "device", "jerk", "estimate", "directions"), sep = "\\.", extra = "merge") %>%
        mutate(directions = gsub("\\.", "", directions)) %>%
        mutate(directions = ifelse(directions == "", "norm", directions)) %>%
        mutate(jerk = grepl("Jerk", jerk)) 
    
    
write.table(data.sub, file = "tidy.data.txt", row.names = F)     
   
    








