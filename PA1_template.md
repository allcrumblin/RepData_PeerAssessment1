# Programm Assignment 1 - Reproducible Research



## Loading and Preprocessing the data

First we read in the data. We add a weekday column for further analysis later to be able to distinguish between weekdays and weekends.


```r
data <- read.csv('activity.csv', header=TRUE)
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- as.factor(data$interval)
data$weekday <- weekdays(data$date, abbreviate=FALSE)
```

## What is mean total number of steps taken per day?

Here we calculate the total number of steps taken each day and create a histogram showing the result.


```r
library(ggplot2)

stepsperday <- aggregate(steps ~ date, data, sum)
colnames(stepsperday) <- c("date","steps")
qplot(steps, data=stepsperday, binwidth=1000, main= 'Total steps each day')
```

![](ProgrammingAssignment_files/figure-html/steps per day-1.png)<!-- -->

Next we calculate the mean and median of the steps per day


```r
steps_mean   <- mean(stepsperday$steps, na.rm=TRUE)
steps_median <- median(stepsperday$steps, na.rm=TRUE)
```

## What is the average daily activity pattern?

Here we create a time series plot showing the average number of steps taken in each interval.


```r
stepsperinterval <- aggregate(steps ~ interval, data, mean)
colnames(stepsperinterval) <- c('Interval','AveSteps')
qplot(as.integer(Interval), AveSteps, data=stepsperinterval, xlab='Interval', ylab='Average Steps', main='Average Steps in dependence of the interval', geom='line')
```

![](ProgrammingAssignment_files/figure-html/Average Steps taken in each interval-1.png)<!-- -->

Next we calculate the interval with the most average steps.


```r
stepsperinterval[which.max(stepsperinterval$AveSteps),]
```

```
##     Interval AveSteps
## 104      835 206.1698
```

## Input missing values

First, lets find the total number of missing values.


```r
sum(is.na(data))
```

```
## [1] 2304
```

By first inspection we realized that the missing values are located in the steps column. We decided to fill the NA values with the average number of steps taken in the corresponding interval.


```r
fillna <- function(data, meanperint) {
  na_index <- which(is.na(data$steps))
  na_replace <- unlist(lapply(na_index, FUN=function(idx){
    interval = data[idx,]$interval
    round(meanperint[meanperint$Interval == interval,]$AveSteps)
  }))
  fill_steps <- data$steps
  fill_steps[na_index] <- na_replace
  fill_steps
}

data2 <- data
data2$steps <- fillna(data, stepsperinterval)
```

Next we check if all NAs have been replaced.


```r
sum(is.na(data2))
```

```
## [1] 0
```

We then create histogram of Total Steps per day with filled NA values


```r
stepsperday2 <- aggregate(steps ~ date, data2, sum)
colnames(stepsperday) <- c("date","steps")
qplot(steps, data=stepsperday, binwidth=1000, main= 'Total steps each day')
```

![](ProgrammingAssignment_files/figure-html/Histogram with filled data-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

We now use the column created while processing the data. By this column we can assign to each day a daytype.


```r
daytype <- function(weekday) {
    if (weekday %in% c('Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag'))
        return("weekday")
    else 
        return("weekend")
}
data2$daytype <- sapply(data2$weekday, daytype)
```

Next we calculate and plot the average steps taken in each interval for weekdays and weekend.


```r
stepsperinterval2 <- aggregate(steps ~ interval + daytype, data2, mean)
qplot(as.integer(interval), steps, data=stepsperinterval2, xlab='Interval', ylab='Average Steps', facets=daytype ~ ., main='Average Steps in dependence of the interval', geom='line')
```

![](ProgrammingAssignment_files/figure-html/getting Average Steps per interval for weekdays and weekends-1.png)<!-- -->
