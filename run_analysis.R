library(plyr)

### helper functions
## trim removes leading and trailing whitespace from character vectors
trim <- function (x) { gsub("^\\s+|\\s+$", "", x) }

## load a text file and return a list of character vectors
loadTextFile <- function(fileName) {
    con <- file(fileName, open="r")
    lines <- readLines(con)
    close(con)
    trim(lines)
}

## As read.csv gets a little confused by some characters in column names ("(), and -")
## -- even if quoted -- this is a little helper function to normalize the names
## so that written and read cvs is identical
normalizeNames <- function(x) {
    tmp <- gsub("[(),-]", "_", x)
    tmp <- gsub("_{2,}", "_", tmp)
    gsub("_+$", "", tmp)
}

### do the actual work
prepareDataSet <- function(verbose=TRUE) {
    
    ## 1: Merge the training and the test sets to create one data set
    
    if (verbose) { message("loading feature data") }
    xTestLines <- loadTextFile("test/X_test.txt")
    xTrainLines <- loadTextFile("train/X_train.txt")
    
    ## extend test data with train data, note that the order in further appends
    ## must match!
    xLines <- append(xTestLines, xTrainLines)
    
    ## split the lines into feature values and cast to numeric
    tab <- strsplit(xLines, " +")
    tab <- lapply(tab, as.numeric)
    
    ## quick check, the documentation said there are 561 features
    if (verbose) {
        message("check features complete (all rows have 561 feature values)")
    }
    stopifnot(all(sapply(tab, length) == 561))
    
    ## convert to dataframe
    if (verbose) { message("convert to dataframe") }
    xDF <- do.call(rbind.data.frame, tab)
    
    ## check: the dataframe must have 2947 (test) + 7352 (train) = 10299 rows
    ## and 561 columns
    if (verbose) {
        message("checking dataframe conversion worked (expecting 10299 observations of 561 features)")
    }
    stopifnot(all(dim(xDF) == c(10299, 561)))
    
    ## assign names
    if (verbose) { message("loading and assigning feature names (i.e. column headers)") }
    lines <- loadTextFile("features.txt")
    lines <- strsplit(lines, " +")
    features <- sapply(lines, "[[", 2)
    names(xDF) <- features
    
    ## 2: Extract only the measurements on the mean and standard deviation for each measurement.
    if (verbose) { message("selecting columns with mean or std measurements") }
    selection <- grepl("mean", names(xDF), ignore.case=TRUE) | grepl("std", names(xDF), ignore.case=TRUE)
    xDF <- xDF[,selection]
    
    ## load subject ids and append a new column
    if (verbose) { message("loading subject ids") }
    subjectTestLines <- loadTextFile("test/subject_test.txt")
    subjectTrainLines <- loadTextFile("train/subject_train.txt")
    subjectLines <- append(subjectTestLines, subjectTrainLines)
    subjectId <- as.numeric(subjectLines)
    DF <- cbind(xDF, subjectId)
    
    ## load and prepare activity labels
    if (verbose) { message("loading activity data") }
    yTestLines <- loadTextFile("test/y_test.txt")
    yTrainLines <- loadTextFile("train/y_train.txt")
    yLines <- append(yTestLines, yTrainLines)
    activityNum <- as.numeric(yLines)
    
    ## 3: Use descriptive activity names to name the activities in the data set
    ## load and prepare activity label names
    activityLines <- loadTextFile("activity_labels.txt")
    activityLines <- strsplit(activityLines, " +")
    activityNames <- sapply(activityLines, "[[", 2)
    
    ## build a factor, preserve the order
    activityF <- factor(activityNames, levels=activityNames)
    
    ## 4: Appropriately label the data set with descriptive activity names.
    ## append a column with activity labels
    DF$activity <- activityF[activityNum]
    
    ## normalize names and return the prepared dataset
    names(DF) <- normalizeNames(names(DF))
    DF
}

## 5: Create a second, independent tidy data set with the average of each
##    variable for each activity and each subject.
computeAverage <- function(DF) {
    
    ## prepare the dataframe for split-apply-combine
    ## in order to use ddply all columns to split onto must be numeric
    ## columns 1:87 are the measurements and the subjectId,
    ##   column 88 is the activity label as factor
    tmp <- DF[,1:87]
    tmp$activity <- as.numeric(DF$activity)
    
    ## save activity levels to reapply later
    activityL <- levels(DF$activity)
    activityF <- factor(activityL, levels=activityL)
    
    ## split by subjectId and activity, apply colMeans and recombine into dataframe
    avg <- ddply(tmp, .(subjectId, activity), colMeans)
    
    ## reapply descriptive activity label
    avg$activity <- activityF[avg$activity]
    
    avg
}

## call the functions
harDF <- prepareDataSet()
harAvgDF <- computeAverage(harDF)

saveDatasets <- function() {
    now <- format(Sys.time(), "%Y-%m-%d:%H-%M-%S")
    harFn <- paste("human-activity-recognition-", now, ".csv", sep="")
    harAvgFn <- paste("human-activity-recognition-average-", now, ".csv", sep="")
    write.csv(harDF, harFn, row.names=FALSE)
    write.csv(harAvgDF, harAvgFn, row.names=FALSE)
}

## uncomment to save the datasets to csv files
saveDatasets()

