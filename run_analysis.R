#You should create one R script called run_analysis.R that does the following. 
# used the blog post (https://thoughtfulbloke.wordpress.com/2015/09/09/getting-and-cleaning-the-assignment/), 
#several Stack Overflow answers, and several RPub documents to complete this 
#(not to mention the numerous, ad-hoc Google searches and trawling the MOOC DF)! 

#load referenced packages
library(dplyr) #for rename(), select(), merge(), group_by(), arrange()


#`1.  Merges the training and the test sets to create one data set.

#dowload zip file if not yet
if (!file.exists("dataset.zip")){
  zipURL  <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(zipURL, method = "curl", destfile = "./Dataset.zip")
  unzip(zipfile = "./Dataset.zip", exdir = "./Dataset")# unzip file 
} 
#list.files("./") #check if filr's present

#load X_rain.txt
trainX = read.table("Dataset/UCI HAR Dataset/train/X_train.txt")
#dim(trainX) #7352 561
#length(complete.cases(trainX)) #7352:  check for NAs

#load X_test.txt
testX = read.table("Dataset/UCI HAR Dataset/test/X_test.txt")
#dim(testX) #2947 561
#length(complete.cases(testX)) #2947:  check for NAs
#View(testX)

#combine train and test sets for X
combinedX <- rbind(trainX, testX)
#dim(combinedX) #10299 561
#length(complete.cases(combinedX)) #10249:  check for NAs
#sapply(combinedX, as.numeric) #make sure everything is numeric for compuatations
#head(combinedX)
#View(CominedX)

#get feature list
featuresX = read.table("Dataset/UCI HAR Dataset/features.txt")
#dim(featuresX) #561 2
#featuresX[, 2]
featuresXNames <- as.list(unlist(featuresX[, 2]))
#class(featuresXNames)
#length(featuresXNames)
featuresXNames <- lapply(featuresXNames, as.character)
#featuresXNames
#View(featuresXNames)

#set feature names
colnames(combinedX)  <- featuresXNames
#head(combinedX)
#View(combinedX)

# load y_train.txt
trainY = read.table("Dataset/UCI HAR Dataset/train/y_train.txt")
#dim(trainY) #7352 1
#length(complete.cases(trainY)) #7352:  check for NAs

#load y_test.txt
testY = read.table("Dataset/UCI HAR Dataset/test/y_test.txt")
#dim(testY) #2947 1
#length(complete.cases(testY)) #2947:  check for NAs

#combine train and test sets for y
combinedY <- rbind(trainY, testY)
#dim(combinedY) #10299 1
#length(complete.cases(combinedY)) #10299:  check for NAs
#sapply(combinedY, as.numeric)  #make sure everything is numeric for compuatations
#which(is.na(combinedY))
#View(combinedY)
#class(combinedY$V1)
#View(combinedY[is.na(combinedY$V1)])

#load subjects from subjects_train.txxt
trainSubject = read.table("Dataset/UCI HAR Dataset/train/subject_train.txt")
#dim(trainSubject) #7352 1
#length(complete.cases(trainSubject)) #7352:  check for NAs

#load subjects from subjects_test.txxt
testSubject = read.table("Dataset/UCI HAR Dataset/test/subject_test.txt")
#dim(testSubject) #2947 1
#length(complete.cases(testSubject)) #2947:  check for NAs

#combine to get all subjects
combinedSubject <- rbind(trainSubject, testSubject)
#dim(combinedSubject) #10299 1
#length(complete.cases(combinedSubject)) #10299:  check for NAs
#sapply(combinedSubject, as.numeric) #make sure everything is numeric for compuatations
#length(unique(unlist(combinedSubject)))# 30:  assumed that all users 1-30 are represented
combinedSubject <- rename(combinedSubject, subject = V1)  
#colnames(combinedSubject)
#head(combinedSubject)
  
#2.  Extracts only the measuremnents on the mean and standard deviation 
#    for each measurement. 

#get the means
dfMeans <- combinedX[grepl("mean",colnames(combinedX))]
#dim(dfMeans) #10299 46
#View(dfMeans)

#get the standard deviationss
dfSDs <- combinedX[grepl("std",colnames(combinedX))]
#dim(dfSDs) #10299 33
#View(dfSDs)


#3.  Uses descriptive activity names to name the activities in the data set
#posted to DF

#load activity labbels
activity <- read.table("Dataset/UCI HAR Dataset/activity_labels.txt")
#dim(activity) #6 2
#View(activity)

#prepare for 'updating'
activity <- rename(activity, activity = V1)  
activity <- rename(activity, activityLabel = V2)
#colnames(activity)

#View(datasetSubset)
dfActivity <- sapply(combinedY, function(x) activity$activityLabel[match(x, activity$activity)])
#View(dfAcivity)


#4.  Appropriately labels the data set with descriptive variable names. 
#posted to DF
#Also used in Step 3 for activity columns

#for features (and descriptions) for reference for script and codebook see "features.txt" and "features_info.txt"
combinedXRenamed <- cbind(dfMeans, dfSDs)

#use regular expressions for matching and replacing column names to make them human readable
names(combinedXRenamed) <- gsub("^t", "Time", names(combinedXRenamed)) #replace leading 't' with time
names(combinedXRenamed) <- gsub("^f", "Transform", names(combinedXRenamed)) #replace leading "t" with transform 'short'for Fast Fourier Transform (FFT) 
names(combinedXRenamed) <- gsub("Acc", "Accelorometer", names(combinedXRenamed)) #Spell Out "Acc-
names(combinedXRenamed) <- gsub("Acc-", "Accelorometer", names(combinedXRenamed)) #Spell Out "Acc-
names(combinedXRenamed) <- gsub("Gyro", "Gyroscope", names(combinedXRenamed)) #Spell Out "Gyro-
names(combinedXRenamed) <- gsub("Gyro-", "Gyroscope", names(combinedXRenamed)) #Spell Out "Gyro-
names(combinedXRenamed) <- gsub("mean\\(\\)", "MeanValue", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("std\\(\\)", "StandardDeviation", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("mad\\(\\)", "MedianValue", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("max\\(\\)", "MaximumValue", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- sub("min\\(\\)", "MinimumValue", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- ssub("sma\\(\\)", "SimpleMagitudeArea", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("energy\\(\\)", "EnergyMeasure", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("iqr\\(\\)", "InterquartileRange", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("entropy\\(\\)", "SignalEntropy", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("arCoeff\\(\\)", "AutoreggressionCoefficients", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("correlation\\(\\)", "CorrelationCoefficient", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("maxInds\\(\\)", "IndexOfMaximumValue", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("meanFreq\\(\\)", "WeightedAverageOfFrequency", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("skewness\\(\\)", "SignalSkewness", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("kurtosis\\(\\)", "SignalKurtosis", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("bandsEnergy\\(\\)", "FrequencyBands", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("angle\\(\\)", "VectorAngle", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("Jerk-", "Shake", names(combinedXRenamed)) #'English-like translation'
names(combinedXRenamed) <- gsub("Mag", "Magnitude", names(combinedXRenamed)) #spell out "mag"
names(combinedXRenamed) <- gsub("-X", "X-coordinate", names(combinedXRenamed)) # 'improve readability'
names(combinedXRenamed) <- gsub("-Y", "Y-coordinate", names(combinedXRenamed)) #'improve readability'
names(combinedXRenamed) <- gsub("-Z", "Z-coordinate", names(combinedXRenamed)) #'improve readability'
names(combinedXRenamed) <- gsub("-coordinate,", "", names(combinedXRenamed))#for arcoeffs: 1-4
names(combinedXRenamed) <- gsub("-XY", "X and Y", names(combinedXRenamed)) #for correlation readabilty
names(combinedXRenamed) <- gsub("-XZ", "X and Z", names(combinedXRenamed)) #for correlation readabilty
names(combinedXRenamed) <- gsub("-YZ", "Y and Z", names(combinedXRenamed)) #for correlation readabilty
names(combinedXRenamed) <- gsub("BodyBody", "Body", names(combinedXRenamed)) #to correct reapeted words observed through visual inspection

#dim(combinedXRenamed)#`10299 79 `
#View(combinedXRenamed)

#esure labelling of activuity column
colnames (dfActivity) [1] <- "activity"



#5.  From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.

#combine all to get an 'abbreviated' dataset
datasetSubset <- cbind(combinedSubject, dfActivity, combinedXRenamed)
#dim(datasetSubset) #10299 81: 1 _ 1 + 79
#View(datasetSubset)

select(datasetSubset, subject, activity)
tidydata <- datasetSubset %>%  
            group_by(subject, activity) %>% 
            arrange(subject, activity) %>%
            summarise_all(funs(mean))
#dim(tidydata) #180 81:  30*6 1+1+ 46 +33
#View(tidydata)  

#wite tidy dataset to file
write.table(tidydata, "TidyData.txt", row.name=FALSE)
#str(tidydata) #compare to what was written

