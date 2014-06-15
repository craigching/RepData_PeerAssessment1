# Reproducible Research: Peer Assessment 1
### Abstract

In this assignment we are analyzing the walking activity of a single person over the course of two months.  The analysis will show when the person was active and when they were not.

## Loading and preprocessing the data

Pre-process the data by converting the "steps" column to numeric which will be useful later and add a day type indicating whether the given day (indicated by the "date" column) is a week day or a weekend day.


```r
library(data.table)
library(ggplot2)

setwd("/Users/cching/Coursera/Reproducible Research/RepData_PeerAssessment1")

if (!file.exists("activity.csv")) unzip("activity.zip")

data <- data.table(read.csv("activity.csv", stringsAsFactors = FALSE))
data <- data[, steps := as.numeric(steps)]
data <- data[, daytype := ifelse(weekdays(as.Date(date)) == "Saturday" | weekdays(as.Date(date)) == "Sunday", "weekend", "weekday")]

options(scipen = 1, digits = 7)
```

## What is mean total number of steps taken per day?


```r
totals <- data[, list(steps = sum(steps, na.rm = TRUE)), by = date]
totals_mean <- totals[, mean(steps)]
totals_median <- totals[, median(steps)]
ggplot(totals, aes(x = steps)) +
    geom_histogram(binwidth = 5000, colour = "black", fill = "steelblue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### The mean total steps is 9354.2295082 and the median is 10395

## What is the average daily activity pattern?


```r
avgs <- data[, list(mean = mean(steps, na.rm = TRUE)), by = interval]
max_avgs <- avgs[avgs[, mean == max(mean)], ]
qplot(interval, mean, data = avgs, geom = "line")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

### The interval with the maximum average number of steps is 835 with a value of 206.1698113

## Imputing missing values


```r
# Compare complete.cases with is.na(steps), if the same
# then all NA are in steps
sum(!complete.cases(data))
```

```
## [1] 2304
```

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
nas <- data[, is.na(steps)]
sum(nas)
```

```
## [1] 2304
```

```r
for (i in 1:nrow(data)) {
    
    if (is.na(data[i]$steps)) {
        data[i]$steps = avgs[interval == data[i]$interval]$mean
    }
}

totals <- data[, list(sum = sum(steps, na.rm = TRUE)), by = date]
totals_mean <- mean(totals$sum)
totals_median <- median(totals$sum)
ggplot(totals, aes(x = sum)) +
    geom_histogram(binwidth = 5000, colour = "black", fill = "steelblue")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

### The mean total steps is 10766.1886792 and the median is 10766.1886792

## Are there differences in activity patterns between weekdays and weekends?

```r
avgs <- data[, list(mean = mean(steps, na.rm = TRUE)), by = list(interval, daytype)]
max_avgs <- avgs[avgs[, mean == max(mean)], ]
qplot(interval, mean, data = avgs, facets = . ~ daytype, geom = c("line", "smooth"), method = "loess") +
    facet_wrap(~ daytype, nrow = 2, ncol = 1)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
