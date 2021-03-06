---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

1. Load the data (i.e. read.csv())


```r
unzip("activity.zip", overwrite = TRUE)
data <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your
analysis

```r
data$date <- as.Date(as.character(strptime(data$date, "%F")))
data$interval <- as.factor(data$interval)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
daily_steps <- aggregate(data$steps~data$date, FUN=sum)
names(daily_steps) <- c("date", "steps.sum")
hist(daily_steps$steps.sum, xlab = "Total number of steps by date")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2. Calculate and report the mean and median total number of steps taken
per day


```r
summary(daily_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)


```r
interval_steps <- aggregate(data$steps~data$interval, FUN=mean)
names(interval_steps) <- c("interval", "steps.mean")
plot(interval_steps, type = "l", xlab = "5-Minute interval identifier", ylab = "Average steps across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?


```r
interval_steps[interval_steps$steps.mean==max(interval_steps$steps.mean),]
```

```
##     interval steps.mean
## 104      835   206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
## A simple_impute function is created that takes a set that needs to be imputed and a set with averages per 5-minute interval that are used to impute the NAs in the former.
simple_impute <- function(to_impute, averages) {
   size <- nrow(to_impute)
   changed <- 0
   ## for each record in the set to be imputed
   
   for (i in 1:size){ ##print(paste("Cheking row ", i, " of ", size))
      
      if(is.na(to_impute[i,]$steps)){ ##print(paste("NA? ", is.na(to_impute[i,]$steps)))
         
         ## Change NA with average value for corresponding 5-minute time interval across all days
         to_impute[i,]$steps <- averages[averages$interval==to_impute[i,]$interval,]$steps
         changed <- changed + 1
      }
   }
   print(paste("NAs imputed: ", changed))
   ## Returned iputed set
   to_impute
}
```

3. Create a new dataset that is equal to the original dataset but with the
missing data filled in.

```r
imputed <- data.frame(data)
imputed <- simple_impute(imputed, interval_steps)
```

```
## [1] "NAs imputed:  2304"
```

4. a. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. 


```r
imputed_daily <- aggregate(imputed$steps~imputed$date, FUN=mean)
names(imputed_daily) <- c("date", "steps")
hist(imputed_daily$steps, xlab = "Mean of daily steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
summary(imputed_daily$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1424 34.0938 37.3826 37.3826 44.4826 73.5903
```

b. Do these values differ from the estimates from the first part of the assignment?

```r
summary(daily_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```
Yes, there are slight differences. After imputation the median is slightly higher, the first quartile is a bit higher and the third quartile a bit lower.

c. What is the impact of imputing missing data on the estimates of the total
daily number of steps?
The distribution changes slightly towards lower values.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday”
and “weekend” indicating whether a given date is a weekday or weekend
day.

```r
library(chron)
imputed$daytype <- ifelse(is.weekend(imputed$date), "weekend", "weekday")
imputed$daytype <- as.factor(imputed$daytype)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis)

```r
daytype_steps <- aggregate(steps~interval+daytype, data = imputed, FUN=mean)
names(daytype_steps) <- c("interval", "daytype", "steps")
daytype_steps$interval <- factor(daytype_steps$interval)
library(lattice)
## The following plotting code creates a perfect plot in R Studio, but knitr does not output the x axis correctly so I plot to a png to demonstrate
png(filename="correct_daytype_steps_plot.png")
xyplot(steps~interval|daytype, daytype_steps, type = "l", layout = c(1,2), ylab = "Average steps by day", scales=list(y=list(at= seq(0, 250, 25), limits=c(0,250)),x=list(at= seq(0, 2400, 200), limits=c(0,2400))))
dev.off()
```

```
## png 
##   2
```

```r
## Calling this through knitr still gives errors, but in R Studio works perfectly.

## The best I can get out of knitr, but with x axis missing (when I leave out x list completely it gets an 'overcrowded' x axis)
xyplot(steps~interval|daytype, daytype_steps, type = "l", layout = c(1,2), ylab = "Average steps by day", scales=list(y=list(at= seq(0, 250, 25), limits=c(0,250)),x=list(at= seq(0, 2400, 200))))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
