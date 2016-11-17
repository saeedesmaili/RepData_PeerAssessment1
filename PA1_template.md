---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Reproducible Research Project 1
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
Load the data

```r
library(ggplot2)
library(plyr)
data <- read.csv("RepData_PeerAssessment1/activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'RepData_PeerAssessment1/
## activity.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```
Process the data into a format suitable for analysis

```r
data$day <- weekdays(as.Date(data$date))
data$DateTime<- as.POSIXct(data$date, format="%Y-%m-%d")

cleanData <- data[!is.na(data$steps),]
```
## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day

```r
sumData <- aggregate(data$steps ~ data$date, FUN = sum )
colnames(sumData)<- c("Date", "Steps")
```
Make a histogram of the total number of steps taken each day

```r
hist(sumData$Steps, breaks = 10, xlab = "Steps", main = "Total Steps per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
Calculate and report the mean and median of the total number of steps taken per day

```r
as.integer(mean(sumData$Steps))
```

```
## [1] 10766
```

```r
as.integer(median(sumData$Steps))
```

```
## [1] 10765
```
## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalData <- ddply(cleanData, .(interval), summarize, Avg = mean(steps))

g <- ggplot(intervalData, aes(x = interval, y = Avg), xlab = "Interval", ylab = "Avg Steps")
g + geom_line() + ggtitle("Avg Steps per Interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maximumSteps <- max(intervalData$Avg)
intervalData[intervalData$Avg == maximumSteps, 1]
```

```
## [1] 835
```
## Imputing missing values
Calculate and report the total number of missing values in the dataset

```r
nrow(data[is.na(data$steps),])
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset

```r
avgData <- ddply(cleanData, .(interval, day), summarize, Avg = mean(steps))
naData <- data[is.na(data$steps),]
dataNew <- merge(naData, avgData, by = c("interval", "day"))
```
Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
dataNew2 <- dataNew[,c(6,4,1,2,5)]
colnames(dataNew2) <- c("steps", "date", "interval", "day", "DateTime")
mergedData <- rbind(cleanData, dataNew2)
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sumData2 <- aggregate(mergedData$steps ~ mergedData$date, FUN = sum)
colnames(sumData2) <- c("Date", "Steps")
as.integer(mean(sumData2$Steps))
```

```
## [1] 10821
```

```r
as.integer(median(sumData2$Steps))
```

```
## [1] 11015
```

```r
hist(sumData2$Steps, breaks = 10, xlab = "Steps", main = "Total Steps per Day (Fixes NAs)", col = "Black")
hist(sumData$Steps, breaks = 10, xlab = "Steps", main = "Total Steps per Day (Fixes NAs)", col = "Grey", add = T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
mergedData$dayCat <- ifelse(mergedData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```
Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
intervalData2 <- ddply(mergedData, .(interval, dayCat), summarize, Avg = mean(steps))
xyplot(Avg ~ interval | dayCat, data = intervalData2, type = "l", layout = c(1,2),
       main = "Average Steps per Interval (Day)",
       ylab = "Average Steps", xlab = "Interval")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

