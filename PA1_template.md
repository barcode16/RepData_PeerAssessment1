# Reproducible Research: Peer Assessment 1

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web site:

* Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

We will start by clearing the environment ready for our new analysis, loading the data, and taking a quick look at the loaded dataset


```r
rm(list=ls())
activity<-read.csv("C:/Users/Mike/Desktop/Coursera materials/R Working Directory/activity.csv", header = TRUE)
head(activity)
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

## What is mean total number of steps taken per day?

##### Make a histogram of the total number of steps taken each day

Next we will summarise the number of steps taking for each day using the tapply() function, and use the summarised data to produce a histogram. tapply() works by adding up the values in x (in this case, the "steps" variable) within each subgroup y (in this case, the "date" variable)


```r
dailysteps<-tapply(activity$steps, activity$date, sum)
hist(dailysteps, breaks = 12
    , col ="lightsteelblue2"
    , main = "Histogram of Daily Steps"
    , xlab = "Number of Daily Steps"
    , ylab = "Freqency"
    , las = 1
    )
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

##### Calculate and report the mean and median total number of steps taken per day

And now we will take a look at the mean and the median total number of steps taken per day using the mean() and median() functions.


```r
mean(dailysteps, na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(dailysteps, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

###### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

We can take a look at the daily pattern of activity by creating a time series plot of the 5-minute intervals and the average number of steps taken across all days. We start by using tapply() again, this time to add up the values in the "steps" variable (x) within each 5-minute interval subgroup (y). We use the head() function to take a look at the new variable created, and then use the plot() function to create the time series plot. 


```r
int.avg.steps<-tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
head(int.avg.steps)
```

```
##       0       5      10      15      20      25 
## 1.71698 0.33962 0.13208 0.15094 0.07547 2.09434
```

```r
plot(int.avg.steps
     , type="l"
     , xlab ="5-minute interval"
     , main="Time Series Plot of No of Steps by 5-Minute Interval"
     , ylab="Average Number of Steps"
     , col = "grey32"
      )
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

###### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

We can subset the data using the which.max() function to identify the 5-minute interval which, on average across all the days, contyains the maximum number of steps. The code will return the name of the 5-minute interval and the average number of steps per day for that interval. 


```r
int.avg.steps[which.max(int.avg.steps)]
```

```
##   835 
## 206.2
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

###### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


###### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

To address the potential bias of missing values, we can replace the Na values with the mean for that particular 5-minute interval. 

We will create a function called mean.f(). This function uses the replace() function to replace an NA value for x with the mean of x excluding the NA values. We will use this function in the next section to create a new dataset.


```r
mean.f<-function(x) replace (x, is.na(x), mean(x, na.rm=TRUE))
```

###### Create a new dataset that is equal to the original dataset but with the missing data filled in.

We load the plyr package to take advantage of the ddply function. For each "interval" subset of the "activity" dataframe, the ddply function applies the mean.f function to the "steps" variable. We then assign the results to a new variable (activityNoNA), and once again use tapply() to get the sum of the steps taken each day. 


```r
library(plyr)
activitynoNA<-ddply(activity, ~ interval, transform, steps = mean.f(steps))
dailystepsnoNA<-tapply(activitynoNA$steps, activitynoNA$date, sum)
```

###### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

We can now create a histogram to show the number of daily steps, and use the mean() and median() functions to show the new mean and median, which we can compare with the mean and median originally calculated. 


```r
hist(dailystepsnoNA
     , breaks=12
     , col ="lightsteelblue2"
     , main = "Histogram of Daily Steps"
     , xlab = "Number of Daily Steps"
     , ylab = "Freqency"
     , las = 1)
```

![plot of chunk unnamed-chunk-8](./PA1_template_files/figure-html/unnamed-chunk-8.png) 

```r
mean(dailystepsnoNA)
```

```
## [1] 10766
```

```r
median(dailystepsnoNA)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

###### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

To create a new factor variable indicating whether the date is a weekday or a weekend, we can create a new function "daytype()", using the weekdays() function. This function will return the result either "weekday" or "weekend", dependent on the output from the weekdays() function. We then use sapply() to assign the returned value to a new "day" variable within the activitynoNA dataframe. Finally, we use the summary() function to inspect the dataframe. 


```r
daytype<-function(date){
  day<-weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
      return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
      return("weekend")
}
activitynoNA$date<-as.Date(activitynoNA$date)
activitynoNA$day<-sapply(activitynoNA$date, FUN = daytype)
activitynoNA$day<-as.factor(activitynoNA$day)
summary(activitynoNA)
```

```
##      steps            date               interval         day       
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0   weekday:12960  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589   weekend: 4608  
##  Median :  0.0   Median :2012-10-31   Median :1178                  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178                  
##  3rd Qu.: 27.0   3rd Qu.:2012-11-15   3rd Qu.:1766                  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355
```

###### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Here we use the aggregate() function to aggregate the average number of steps by interval an day, and then create two new dataframes, one of the weekend observations and the other of the weekday observations. We can then produce a separate line plot for each of the two dataframes to compare the differences for weekdays and weekends. 


```r
averages<-aggregate(steps ~ interval + day, data = activitynoNA, mean)
weekday<-averages[averages$day=="weekday", c(1,3)]
weekend<-averages[averages$day=="weekend", c(1,3)]
par(mfrow=c(2,1), mar=c(4,4,2,1))
plot(weekday, type="l", ylim=c(0,230), xlab= "Weekday 5-Minute Intervals", ylab="Average Steps")
plot(weekend, type="l", ylim=c(0,230), xlab= "Weekend 5-Minute Intervals", ylab="Average Steps")
```

![plot of chunk unnamed-chunk-10](./PA1_template_files/figure-html/unnamed-chunk-10.png) 



