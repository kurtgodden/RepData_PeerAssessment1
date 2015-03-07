################################################################################
#######################  Project One: Repr Res   ###############################
################################################################################
# Loading and preprocessing the data
#
# Show any code that is needed to
# 1. Load the data (i.e. read.csv() )
# 2. Process/transform the data (if necessary) into a format suitable for your analysis

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
# > dim(activityDataRaw)
# [1] 17568     3
# 