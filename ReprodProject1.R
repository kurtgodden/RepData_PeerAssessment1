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
dim(activityDataRaw) # This is to confirm we have the expected data
# [1] 17568     3
# 
#####################################################
####### Part 1 regarding mean steps per day   #######
#####################################################
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset. 
# 1. Calculate the total number of steps taken per day
# 2. If you do not understand the difference between a histogram and a barplot, 
#    research the difference between them. Make a histogram of the total number 
#   of steps taken each day
# 3. Calculate and report the mean and median of the total number of steps taken per day

# 1. Calculate the total number of steps taken per day
# I decided simply to use base graphics for this task. Nothing fancy.
dailySteps <- tapply(activityDataRaw$steps, activityDataRaw$date, sum,
                     na.rm = TRUE) #we were told to ignore NAs for this part

# 2. Make a histogram of the total number of steps taken each day
hist(dailySteps, main="Histogram of Daily Step Totals", 
     xlab="Total Daily Steps", col="lightblue", ylim=c(0, 30))

# 3. Calculate and report the mean and median 
# of the total number of steps taken per day
meanDailySteps <- mean(dailySteps)
medianDailySteps <- median(dailySteps)

mmPlot <-
    barplot(c(meanDailySteps, medianDailySteps), 
            main="Daily Steps: Comparison of Central Measures",
            ylim=c(0, 12000), names.arg=c("Mean Daily Steps", "Median Daily Steps"),
            ylab="Number of Daily Steps", col=c("lightblue", "pink"))
# Now add the y values as text
text(y=c(meanDailySteps, medianDailySteps), 
     x=mmPlot, 
     pos=3, #Place data labels above the bars
     labels=as.character(c(round(meanDailySteps, digits=1), medianDailySteps)))

# Notice in plot that the median is almost 1041 steps more than the mean.

#################################################################
####### Part 2 regarding average daily activity pattern   #######
#################################################################

# What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l" ) of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all days (y-axis)
# 2. Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?

# Question 1 is slightly confusing, but here is my interpretaion of it:
# He is asking us to compute the average number of steps taken for each 5-min
# interval.  For example, there are 288 5-minute samples in a 
# day, so we take the average value across possibly 61 days (if no NAs)
# for each of those 288 samples and that is one data point to be plotted, or
# 288 data points in all.

# Question 1
intervalMeanSteps <- tapply(activityDataRaw$steps, activityDataRaw$interval, 
                     mean, na.rm = TRUE)
plot(intervalMeanSteps, type="l", 
     main="Mean Steps of 288 5-minute Samples\n(missing data omitted)",
     xlab="5-minute Interval", ylab="Mean Steps across 61 Days", col="blue",
     xlim=c(0, 300))

# Question 2
# intervalMeanSteps has length 288 and each value is the mean of the
# steps for that interval across all days:
# > length(intervalMeanSteps)
# [1] 288
# > head(intervalMeanSteps)
# 0         5        10        15        20        25 
# 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396 

# So now we just have to find which of those intervals has the highest value.
# The highest value is:
# > max(intervalMeanSteps)
# [1] 206.1698

# So let's find out the index of that interval
indexOfmaxMeanStepInterval <- which(intervalMeanSteps==max(intervalMeanSteps))
# > indexOfmaxMeanStepInterval
# 835 
# 104 
# So the 104th element of intervalMeanSteps refers to interval 835
# Looking at the plot, this appears correct since the highest point looks
# slightly above 200 on the y-axis and slightly higher than 100 on the x-axis.

# That is our answer, but to double-check its correctness:

intervalMeanSteps[indexOfmaxMeanStepInterval[[1]]]

# > intervalMeanSteps[indexOfmaxMeanStepInterval[[1]]]
# 835 
# 206.1698

# i.e. this is correct.
# So the answer to question 2 is that interval 835, on average, has the 
# highest number of steps: 206.1698.

#################################################################
############## Part 3 on Imputing Missing Values   ############## 
#################################################################

# Note that there are a number of days/intervals where there are missing values 
# (coded as NA ). The presence of missing days may introduce bias into some 
# calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with NA s)
# Doing the same thing as shown below, but on the 'date' and 'interval'
# vars, shows a sum of 0 in both cases, so we know that all of the NA
# values are in the 'steps' variable.

missingSteps <- is.na(activityDataRaw$steps)
sum(missingSteps) #This shows our answer, which is 2304
#  [1] 2304

# 2.  Devise a strategy for filling in all of the missing values in the dataset. 
# 3.  Create a new dataset that is equal to the original dataset but with the 
#     missing data filled in.
# I decided to round the mean for each 5-min interval to the nearest whole number
# e.g. for interval 1840, the mean is 85.33962, so the round() of that is 85,
# which comes from round(intervalMeanSteps[["1840"]])

# get row indices of raw data with missing steps
indicesOfNA <- which(missingSteps, arr.ind=TRUE)

# Define function on data frame and indices into that df
# to replace the NA vals in the 'steps' var with the round of the steps
# in the same interval using 'intervalMeanSteps', previously computed.
fillInNA <- function(df, indices) {
    # replace each NA in 'steps' var with the nearest whole number
    # from the corresponding interval previously computed in intervalMeanSteps
    for (i in indices) {
        intervalOfNA <- df[i, "interval"]
        #next line references intervalMeanSteps as a global
        df$steps[i] <- round(intervalMeanSteps[[as.character(intervalOfNA)]])
    }
    df
}

activityDataFilled <- fillInNA(activityDataRaw, indicesOfNA)

# 4. Make a histogram of the total number of steps taken each day and Calculate 
#    and report the mean and median total number of steps taken per day. 
#    Do these values differ from the estimates from the first part of the assignment? 
#    What is the impact of imputing missing data on the estimates 
#    of the total daily number of steps?

dailyStepsFilled <- tapply(activityDataFilled$steps, activityDataFilled$date, 
                           sum) 

#Make a histogram of the total number of steps taken each day
hist(dailyStepsFilled, main="Histogram of Daily Step Totals (filled NAs)", 
     xlab="Total Daily Steps (filled NAs)", col="blue", ylim=c(0, 40))

#Instructions: Calculate and report the mean and median 
#of the total number of steps taken per day
meanDailyStepsFilled <- mean(dailyStepsFilled)
medianDailyStepsFilled <- median(dailyStepsFilled)

mmPlotFilled <-
    barplot(c(meanDailyStepsFilled, medianDailyStepsFilled), 
            main="Daily Steps (filled NAs): Comparison of Central Measures",
            ylim=c(0, 12000), 
            names.arg=c("Mean Daily Steps", "Median Daily Steps"),
            ylab="Number of Daily Steps", col=c("blue", "red"))
# Now add the y values as text
text(y=c(meanDailyStepsFilled, medianDailyStepsFilled), 
     x=mmPlotFilled, 
     pos=3, #Place data labels above the bars
     labels=as.character(c(round(meanDailyStepsFilled, digits=1), 
                           medianDailySteps)))

# Now notice how, with filled NA data, the mean and median are nearly identical

#Obviously, imputing the missing values indeed makes a huge difference
#in the graph compared to the raw numbers.  With imputed values, 
#the mean and median are nearly identical, whereas with the raw data
#the difference was almost 1041 steps.  
#And also the shape of the 2 histograms is VERY different in raw vs. imputed,
#as is the absolute height of the bars (which would be expected).

#################################################################
################# Part 4 on Activity Patterns   ################# 
#################################################################

# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset 
# with the filled-in missing values for this part.
# 1. Create a new factor variable in the dataset with two levels – “weekday” and 
#    “weekend” indicating whether a given date is a weekday or weekend day.
# 2. Make a panel plot containing a time series plot (i.e. type = "l" ) 
#    of the 5-minute interval (x-axis) and the average number of steps taken, 
#    averaged across all weekday days or weekend days (y-axis). See the README file 
#    in the GitHub repository to see an example of what this plot should look 
#    like using simulated data.

isWeekend <- function(dateAsCharacter) {
                weekdays(as.Date(dateAsCharacter)) %in% c("Saturday", "Sunday")
                }
is.weekend <- isWeekend(activityDataFilled$date)

is.weekend[is.weekend==TRUE] <- "weekend"
is.weekend[is.weekend==FALSE] <- "weekday"
        
activityDataFilled$TypeOfDay <- as.factor(is.weekend)

# Now make the panel plot and I'll use ggplot2

library(ggplot2)

tsPlot <- ggplot(activityDataFilled, aes(interval, steps))
tsPlot + facet_grid(TypeOfDay ~ .) +
    xlab("Interval") +
    ylab("Number of steps") +
    ggtitle("Time Series Comparison of Mean Steps: Weekday vs. Weekend") +
    stat_summary(fun.y="mean", geom=c("line"), color="blue")

