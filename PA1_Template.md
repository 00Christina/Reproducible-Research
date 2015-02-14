Activity Monitoring Data: October-November 2012
===============================================

This document seeks to analyze data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The source for this data can be downloaded from [this url location](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip), or found separately in the GitHub repository titled ["activity.zip"](https://github.com/00Christina/RepData_PeerAssessment1/blob/master/activity.zip).

The variables included in this dataset are:

-steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
-date: The date on which the measurement was taken in YYYY-MM-DD format
-interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

The following R code reads the data and processes it into a form that is suitable for analysis.


```r
activity <- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

We would like to answer the question: What is the mean number of steps taken per day? To do this, the following R code will make a histogram of the total number of steps taken each day, and then report the mean and median total number of steps per day.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
library(ggplot2)
library(scales)

by_date <- group_by(activity, date)
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'activity' not found
```

```r
sum_date <- summarize(by_date, sum_steps = sum(steps))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'by_date' not found
```

```r
sum_date$date <- as.Date(sum_date$date)
```

```
## Error in as.Date(sum_date$date): object 'sum_date' not found
```

```r
ggplot(sum_date, aes(x = date, y= sum_steps)) + geom_bar(stat = "identity", position= "dodge") + ylab("Step Count") + ggtitle("Steps by Date, October-November 2012") + scale_x_date(breaks = pretty_breaks())
```

```
## Error in ggplot(sum_date, aes(x = date, y = sum_steps)): object 'sum_date' not found
```

```r
mean_step <- mean(sum_date$sum_steps, na.rm=TRUE)
```

```
## Error in mean(sum_date$sum_steps, na.rm = TRUE): object 'sum_date' not found
```

```r
median_step <- median(sum_date$sum_steps, na.rm=TRUE)
```

```
## Error in median(sum_date$sum_steps, na.rm = TRUE): object 'sum_date' not found
```

```r
mean_median <- merge(mean_step, median_step)
```

```
## Error in merge(mean_step, median_step): object 'mean_step' not found
```

```r
colnames(mean_median) <- c("Mean", "Median")
```

```
## Error in colnames(mean_median) <- c("Mean", "Median"): object 'mean_median' not found
```

```r
print(mean_median)
```

```
## Error in print(mean_median): object 'mean_median' not found
```
Next question is: What is the average daily activity pattern? To answer this, the following R code makes a time series plot of the mean number of steps taken across all days by interval, and then finds the maximum number of steps taken in one interval over that time span.


```r
steps_interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
plot(steps_interval, type = "l", main = "Time Series by 5-Minute Interval")
```

```
## Error in plot(steps_interval, type = "l", main = "Time Series by 5-Minute Interval"): object 'steps_interval' not found
```

```r
max_interval <- steps_interval$interval[which.max(steps_interval$steps)]
```

```
## Error in eval(expr, envir, enclos): object 'steps_interval' not found
```

```r
print(max_interval)
```

```
## Error in print(max_interval): object 'max_interval' not found
```

Following that, we'd like to impute the missing values that may otherwise affect summarization and analysis of the data. First, we find the total number of rows missing the data, and then use the means of the 5-minute intervals to fill in the missing data. Afterward, we make a histogram with the adjusted data. Again, we report the mean and median values to see if they have changed much by the imputation.


```r
sum(is.na(activity))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
activity <- merge(activity, steps_interval, by = "interval", suffixes = c("", 
    ".y"))
```

```
## Error in merge(activity, steps_interval, by = "interval", suffixes = c("", : object 'activity' not found
```

```r
NAs <- is.na(activity$steps)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
activity$steps[NAs] <- activity$steps.y[NAs]
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
activity <- activity[, c(1:3)]
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
steps_date <- aggregate(steps ~ date, data = activity, FUN = sum)
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
barplot(steps_date$steps, main = "Adjusted Steps by Date, October-November 2012", names.arg = steps_date$date, xlab = "date", ylab = "steps")
```

```
## Error in barplot(steps_date$steps, main = "Adjusted Steps by Date, October-November 2012", : object 'steps_date' not found
```

```r
mean_steps <- mean(steps_date$steps)
```

```
## Error in mean(steps_date$steps): object 'steps_date' not found
```

```r
median_steps <- median(steps_date$steps)
```

```
## Error in median(steps_date$steps): object 'steps_date' not found
```

```r
mean_medians <- merge(mean_steps, median_steps)
```

```
## Error in merge(mean_steps, median_steps): object 'mean_steps' not found
```

```r
colnames(mean_medians) <- c("Mean", "Median")
```

```
## Error in colnames(mean_medians) <- c("Mean", "Median"): object 'mean_medians' not found
```

```r
print(mean_medians)
```

```
## Error in print(mean_medians): object 'mean_medians' not found
```

The final question we'd like to address is: Are there differences in activity patterns between weekdays and weekends? First, we'll create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Then, we'll make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
dayofweek <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$dayofweek <- as.factor(sapply(activity$date, dayofweek))
```

```
## Error in lapply(X = X, FUN = FUN, ...): object 'activity' not found
```

```r
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps_type <- aggregate(steps ~ interval, data = activity, subset = activity$dayofweek == 
        type, FUN = mean)
    plot(steps_type, type = "l", main = type)
}
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

Looking at the two plots, it appears that there is more activity on the weekends than during the weekday.
