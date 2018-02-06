## Loading and preprocessing the data

data <- read.csv("activity.csv")
data$date <- as.Date(as.character(strptime(data$date, "%F")))
data$interval <- as.factor(data$interval)

## What is mean total number of steps taken per day?

daily_steps <- aggregate(data$steps~data$date, FUN=sum)
names(daily_steps) <- c("date", "steps.sum")

## What is the average daily activity pattern?

interval_steps <- aggregate(data$steps~data$interval, FUN=mean)
names(interval_steps) <- c("interval", "steps.mean")

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

imputed <- data.frame(data)   
imputed <- simple_impute(imputed, interval_steps)

imputed_daily <- aggregate(imputed$steps~imputed$date, FUN=sum)
names(imputed_daily) <- c("date", "steps.sum")

## Are there differences in activity patterns between weekdays and weekends?

library(chron)
imputed$daytype <- ifelse(is.weekend(imputed$date), "weekend", "weekday")
imputed$daytype <- as.factor(imputed$daytype)

daytype_steps <- aggregate(steps~interval+daytype, data = imputed, FUN=mean)
names(daytype_steps) <- c("interval", "daytype", "steps")
daytype_steps$interval <- as.numeric(levels(daytype_steps$interval))

## The following plotting code creates a perfect plot in R Studio, but knitr does not output the x axis correctly so I plot to a png to demonstrate
## library(lattice)
## png(filename="correct_daytype_steps_plot.png")
## xyplot(steps~interval|daytype, daytype_steps, type = "l", layout = c(1,2), ylab = "Average steps by day", scales=list(y=list(at= seq(0, 250, 25), limits=c(0,250)),x=list(at= seq(0, 2400, 200), limits=c(0,2400))))
## dev.off()
## Calling this through knitr still gives errors, but in R Studio works perfectly.
## The best I can get out of knitr, but with x axis missing (when I leave out x list completely it gets an 'overcrowded' x axis)
## xyplot(steps~interval|daytype, daytype_steps, type = "l", layout = c(1,2), ylab = "Average steps by day", xlab = "5-Minute interval identifier", scales=list(y=list(at= seq(0, 250, 25), limits=c(0,250)),x=list(at= seq(0, 2400, 200))))