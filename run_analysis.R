## Merges the training and the test sets to create one data set

train_set_dir <- ".\\UCI HAR Dataset\\train\\"
test_set_dir <- ".\\UCI HAR Dataset\\test\\"

part1 <- read.table(paste(train_set_dir,"X_train.txt", sep = "", collapse = NULL))
part2 <- read.table(paste(test_set_dir,"X_test.txt", sep = "", collapse = NULL))
x <- rbind(part1, part2)

part1 <- read.table(paste(train_set_dir,"y_train.txt", sep = "", collapse = NULL))
part2 <- read.table(paste(test_set_dir,"y_test.txt", sep = "", collapse = NULL))
y <- rbind(part1, part2)

part1 <- read.table(paste(train_set_dir,"subject_train.txt", sep = "", collapse = NULL))
part2 <- read.table(paste(test_set_dir,"subject_test.txt", sep = "", collapse = NULL))
s <- rbind(part1, part2)

## Extracts only the measurements on the mean and standard deviation for each measurement.
## read file with labels
features <- read.table(".\\UCI HAR Dataset\\features.txt")

#find column Nos.
features_mean <- grep("-mean\\(\\)", features[, 2])
features_std <- grep("-std\\(\\)", features[, 2])
labels <- c(features_mean,features_std)

#assign labels
x <- x[, labels]
names(x) <- features[labels, 2]
#delete "()" in labels
names(x) <- gsub("\\(|\\)", "", names(x))

## Uses descriptive activity names to name the activities in the data set
activity <- read.table(".\\UCI HAR Dataset\\activity_labels.txt")
y[,1] = activity[y[,1], 2]


## Appropriately labels the data set with descriptive variable names.
names(y) <- "activity"
names(s) <- "subject"


## From the data set in step 4, creates a second, independent tidy data set
## with the average of each variable for each activity and each subject
tidy_dataset <- cbind(s, y, x)

u_s <- unique(s)[,1]
len_s = length(u_s)
len_a = length(activity[,1])
numCols = dim(tidy_dataset)[2]
result = tidy_dataset[1:(len_s*len_a), ]
row = 1
for (s in 1:len_s) {
        for (a in 1:len_a) {
                result[row, 1] = u_s[s]
                result[row, 2] = activity[a, 2]
                tmp <- tidy_dataset[tidy_dataset$subject==s & tidy_dataset$activity==activity[a, 2], ]
                result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}
write.table(result, ".\\UCI HAR Dataset\\tidy_dataset_plus_avg.txt")
write.table(result, ".\\UCI HAR Dataset\\tidy_dataset_plus_avg_no_row_names.txt",row.name=FALSE)