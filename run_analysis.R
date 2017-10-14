library(reshape)
library(data.table)

##Get current working directory
path <- getwd()

##Get labelSet and featureSet
labelSet <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt"), col.names = c("classlabelSet", "activityName"))
featureSet <- fread(file.path(path, "UCI HAR Dataset/features.txt"), col.names = c("index", "featureNames"))

#Only need mean and std features
targetFeatures <- grep("(mean|std)\\(\\)", featureSet[, featureNames])

##Get the measureFeatures we are interested in
measureFeatures <- featureSet[targetFeatures, featureNames]

##Strip off the brackets in the measurement names
measureFeatures <- gsub('[()]', '', measureFeatures)

##Get training data
trainingDataX <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, targetFeatures, with = FALSE]

data.table::setnames(trainingDataX, colnames(trainingDataX), measureFeatures)

##Get training data
trainingActivities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt"), col.names = c("ActivityName"))
trainingSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt"), col.names = c("SubjectNum"))

##Combine data with acitivies and subjects
trainingData <- cbind(trainingSubjects, trainingActivities, trainingDataX)

##Get test data X
textDataX <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, targetFeatures, with = FALSE]

##Set measurement names
data.table::setnames(textDataX, colnames(textDataX), measureFeatures)

##Get test data
testActivities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt"), col.names = c("ActivityName"))
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt"), col.names = c("SubjectNum"))

##Combine data with acitivies and subjects
testData <- cbind(testSubjects, testActivities, textDataX)

##Combine training data and test data
bindedData <- rbind(trainingData, testData)

##Replace ActivityName number with ActivityName names
bindedData[["ActivityName"]] <- factor(bindedData[, ActivityName], levels = labelSet[["classlabelSet"]], labels = labelSet[["activityName"]])
##Make subject number factors
bindedData[["SubjectNum"]] <- as.factor(bindedData[, SubjectNum])

##Calculate the average of each acvitiby and each subject
bindedData <- reshape2::melt(data = bindedData, id = c("SubjectNum", "ActivityName"))
bindedData <- reshape2::dcast(data = bindedData, SubjectNum + ActivityName ~ variable, fun.aggregate = mean)

data.table::fwrite(x = bindedData, file = "cleanData.txt", quote = FALSE, row.name=FALSE)