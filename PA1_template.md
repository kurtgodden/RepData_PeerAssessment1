# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
#Data file is here in my local repo clone:
# /Users/Kurt/DataScience/datasciencecoursera/RepData_PeerAssessment1

#create new working directory for this project
wd <- getwd() 
if (!file.exists("./Project1")) {dir.create("./Project1")}
newWD <- paste(wd,"/Project1", sep="")
setwd(newWD)

#get source data file from local repo clone
fileURL <- "~/DataScience/datasciencecoursera/RepData_PeerAssessment1/activity.zip"

#unzip file and save to new working dir
unzip(fileURL, exdir = newWD, overwrite=TRUE)

#read data
activityDataRaw <- read.csv("activity.csv")
dim(activityDataRaw) # This is to confirm we have the expected data
```

```
## [1] 17568     3
```

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
