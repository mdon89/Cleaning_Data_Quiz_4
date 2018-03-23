#Set working directory
setwd("C:/Users/mdon/datasciencecoursera/Cleaning data quiz 4/UCI HAR Dataset)

#list file names
filenames <- list.files()[c(5,6,9,10,11,12)]


#read in all test documents and assign to a dataframe
List <- lapply(filenames, read.table, fill = TRUE)

#Assign names to data frames
newnames <- gsub(".txt", "",filenames)
names(List) <- newnames

#Unlist List in Global environment
list2env(List, .GlobalEnv)

#Merge datasets with cbind for data rames with the same number or rows 

Mergeddata <- cbind(subject_test, y_test, X_test)

Mergeddatatrain <- cbind(subject_train, y_train, X_train)

#rbind two dataframes above to give one data source

df1 <- rbind(Mergeddata, Meregeddatatrain)

#Change first 2 column names

colnames(df1)[1] <- "Subject"
colnames(df1)[2] <- "Y"

#Add column headings to rest of df1 dataframe from features file
df1columnheadings <- features$V2

#Convert column headings to character
df1columnheadings <- as.character(df1columnheadings)

colnames(df1)[3:563] <- df1columnheadings

#Find which columns are mean or standard deviation so heading contains mean() or std()
std <- grep("std()", colnames(df1), fixed = TRUE)
mean <- grep("mean()", colnames(df1), fixed = TRUE)

#subset df1 on mean and std columns
meanstddf1 <- df1[,c(1,2,mean,std)]

#read in activity labels document
activity_labels <- read.table("activity_labels.txt")

#assign labels to corresponding numbers in column Y of dataframe. Lookup Y values and match to activities to create new vector of activities

newlabel <- as.character(sapply(meanstddf1$Y, function(x) activity_labels$V2[match(x, activity_labels$V1)]))

#Substitute numbers with names of activities

meanstddf1$Y <- newlabel

#Replace column name Y with "activity names"
colnames(meanstddf1)[2] <- "Activity_Names"

#Rename column variables so they are more understandable. Replace t with time and f with freq

renamet <- gsub("^t", "time",colnames(meanstddf1)[3:68])
renametf <- gsub("^f", "freq",renamet)
colnames(meanstddf1)[3:68] <- renametf

#create 2nd data frame with tidy data. One mean per subject and activity of each variable. Group by Activity name and subject then take mean of variables
res = meanstddf1 %>% group_by(Subject, Activity_Names) %>%
+     summarise_each(funs(mean(.)))

