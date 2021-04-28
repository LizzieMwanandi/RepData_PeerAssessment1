
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of
two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute
intervals each day.
=========================================================

# DOWNLOADING AND READING DATA

Dataset for [ACTIVITY] [1]

[1]:  https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip "ACTIVITY"



```r
activity_URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

download.file(activity_URL,"./Data2021/Activity.zip")
unzip("./Data2021/Activity.zip", exdir = "./Data2021/Activity")
Activity <- read.table("./Data2021/Activity/activity.csv", header = TRUE, sep = "," , na.strings = TRUE)
```

## EXPLORING DATA

```r
head(Activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
dim(Activity)
```

```
## [1] 17568     3
```

# CHANGING FORMAT OF THE DATE AND STEPS

```r
Activity$date <- as.Date(Activity$date)
Activity$steps <-  as.numeric(Activity$steps)
```

```
## Warning: NAs introduced by coercion
```

1.HISTOGRAM OF STEPS TAKEN PER DAY

a.Finding the total number of steps taken per day

```r
library(dplyr)

stepsperday <- Activity %>%

        group_by(date) %>%
        
        summarise(sumsteps = sum(steps,na.rm = TRUE))

head(stepsperday,10)
```

```
## # A tibble: 10 x 2
##    date       sumsteps
##    <date>        <dbl>
##  1 2012-10-01        0
##  2 2012-10-02      126
##  3 2012-10-03    11352
##  4 2012-10-04    12116
##  5 2012-10-05    13294
##  6 2012-10-06    15420
##  7 2012-10-07    11015
##  8 2012-10-08        0
##  9 2012-10-09    12811
## 10 2012-10-10     9900
```

b. Finding the mean and median



```r
meansteps <-  mean(stepsperday$sumsteps)

median <- median(stepsperday$sumsteps)
```
c. Plottingthe Histogram

```r
hist(stepsperday$sumsteps, main ="STEPS PER DAY", col = "Orange", xlab = "Number of Steps"  )
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
2. FINDING THE MEAN AND MEDIAN OF STEPS TAKEN PER DAY


b. Finding the mean and median

```r
meansteps <-  mean(stepsperday$sumsteps)

median <- median(stepsperday$sumsteps)
```

3.STEPS PER INTERVAL

a. Finding the Average daily activity pattern


```r
StepsperInterval <- Activity %>%
        
        group_by(interval) %>%
        
        summarise(meansteps = mean(steps, na.rm = TRUE))
head(StepsperInterval,10)
```

```
## # A tibble: 10 x 2
##    interval meansteps
##       <int>     <dbl>
##  1        0    1.72  
##  2        5    0.340 
##  3       10    0.132 
##  4       15    0.151 
##  5       20    0.0755
##  6       25    2.09  
##  7       30    0.528 
##  8       35    0.868 
##  9       40    0     
## 10       45    1.47
```
b. Plotting the graph



```r
library(ggplot2)

plot(StepsperInterval$meansteps ~ StepsperInterval$interval, main ="STEPS PER INTERVAL",type = "l", col = "red",xlab = "5 minutes Interval", ylab = "Average Number of Steps" )
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
c. Average steps for the 5 Minutes Interval


```r
print(paste("Interval containing the most steps on average: ",StepsperInterval$interval[which.max(StepsperInterval$meansteps)]))
```

```
## [1] "Interval containing the most steps on average:  835"
```

```r
print(paste("Average steps for that interval: ",round(max(StepsperInterval$meansteps),digits=2)))
```

```
## [1] "Average steps for that interval:  206.17"
```


5. MISSING VALUES


```r
print(paste("The total number of rows with NA is: ",sum(is.na(Activity$steps))))
```

```
## [1] "The total number of rows with NA is:  2304"
```

```r
activityNoNA <- Activity  
for (i in 1:nrow(Activity)){
        if(is.na(Activity$steps[i])){
                activityNoNA$steps[i]<- StepsperInterval$meansteps[activityNoNA$interval[i] == StepsperInterval$interval]
        }
}


head(activityNoNA,10)
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsperday <- activityNoNA %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 
head(stepsperday,10)
```

```
## # A tibble: 10 x 2
##    date       sumsteps
##    <date>        <dbl>
##  1 2012-10-01   10766.
##  2 2012-10-02     126 
##  3 2012-10-03   11352 
##  4 2012-10-04   12116 
##  5 2012-10-05   13294 
##  6 2012-10-06   15420 
##  7 2012-10-07   11015 
##  8 2012-10-08   10766.
##  9 2012-10-09   12811 
## 10 2012-10-10    9900
```

```r
hist(stepsperday$sumsteps, main = "Histogram of Daily Steps", 
     col="blue", xlab="Steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from
the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
meanPostNA <- round(mean(stepsperday$sumsteps), digits = 2)
medianPostNA <- round(median(stepsperday$sumsteps), digits = 2)

print(paste("The mean is: ", mean(meanPostNA)))
```

```
## [1] "The mean is:  10766.19"
```

```r
print(paste("The median is: ", median(medianPostNA)))
```

```
## [1] "The median is:  10766.19"
```

```r
print(paste("The mean is: ", mean(meanPostNA)))
```

```
## [1] "The mean is:  10766.19"
```

COMPARING


```r
NACompare <- data.frame(mean = c(meansteps,meanPostNA),median=c(median,medianPostNA))
rownames(NACompare) <- c("Pre NA Transformation", "Post NA Transformation")
print(NACompare)
```

```
##                            mean   median
## Pre NA Transformation   9354.23 10395.00
## Post NA Transformation 10766.19 10766.19
```
5. Are there differences in activity patterns between weekdays and     weekends?

a. Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activityDoW <- activityNoNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)
```

b.Make a panel plot containing a time series plot (i.e. type=“l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
        group_by(interval) %>%
        summarize(steps = mean(steps)) 
activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)
```

c. Plotting the Graph


```r
g <- ggplot (wkdayWkend, aes (interval, steps))
g + geom_line() + facet_grid (day~.) + 
        theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
        labs(y = "Number of Steps") + labs(x = "Interval") + 
        ggtitle("Average Number of Steps - Weekday vs. Weekend") + 
        theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

knit2html() 

