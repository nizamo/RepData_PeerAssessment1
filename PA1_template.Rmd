---
title: "Peer Assessment 1_Reproducible Research"
author: "Nizamo"
date: "Saturday, September 19, 2015"
output: html_document
---

# Reproducible Research - Peer Assessment 1

## Loading and preprocessing the data
##### 1. Load the data (i.e. read.csv())
```{r}
file<-"E:/BigDataNizam/Module5/repdata-data-activity/activity.csv" 

Data_activity<- read.csv(file, header=TRUE, sep=",")
head(Data_activity)

echo=TRUE
library(lattice)

```


##### 2. Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
Data_activity$date <-as.Date(Data_activity$date,"%Y-%m-%d")
```


## What is mean total number of steps taken per day?
```{r}
TotalSteps <-aggregate(steps ~ date, data =Data_activity, sum, na.rm=TRUE)
```

##### 1. Make a histogram of the total number of steps taken each day
```{r}

hist(TotalSteps$steps, main ="Total Steps per Day", xlab="Day", col="red")

```


##### 2. Calculate and report the mean and median total number of steps taken per day
```{r}
mean(TotalSteps$steps)
median(TotalSteps$steps)

```

-----

## What is the average daily activity pattern?
```{r}
time_series<- tapply(Data_activity$steps, Data_activity$interval, mean, na.rm=TRUE)


```

##### 1. Make a time series plot
```{r}
plot(row.names(time_series), time_series, type = "l", xlab= "5 min interval",
     ylab="Average across all Days", main= "Average number of steps taken",
     col="red")
```

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
maximum_interval<- which.max(time_series)
names(maximum_interval)
```


----

## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset 
```{r}

activity_NA <- sum(is.na(Data_activity))
activity_NA


```


##### 2. Devise a strategy for filling in all of the missing values in the dataset.

```{r}
Steps_Average<- aggregate(steps ~ interval, data = Data_activity, FUN = mean)
FillNA<- numeric()
for (i in 1 :nrow(Data_activity)) {
  obs<- Data_activity[i,]
  if (is.na(obs$steps)){
    steps <-subset(Steps_Average, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  FillNA <-c(FillNA, steps)
}

```


##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
new_DataActivity <- Data_activity
new_DataActivity$steps <- FillNA

```


##### 4. Make a histogram of the total number of steps taken each day 
```{r}
TotalSteps2 <- aggregate(steps ~ date, data = new_DataActivity, sum, na.rm= TRUE)
hist(TotalSteps2$steps, main="Total Steps by Day", xlab= "Day", col="red")

```

##### Calculate and report the mean and median total number of steps taken per day. 
```{r}
mean(TotalSteps2$steps)

median(TotalSteps2$steps)
```

##### State the impact of imputing missing data to the estimates of the total daily number of steps?
##### The mean is still the same but the median have a little bit increase.


-------

## Are there differences in activity patterns between weekdays and weekends?
##### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```{r}
day <- weekdays(new_DataActivity$date)
daylevel <- vector()
for (i in 1: nrow(new_DataActivity)) {
  if (day[i] == "Saturday"){
    daylevel[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    daylevel[i] <-"Weekend"
  } else {
    daylevel[i]= "Weekday"
  }
}
new_DataActivity$daylevel <- daylevel
new_DataActivity$daylevel <- factor(new_DataActivity$daylevel)

StepsByDay <- aggregate (steps ~ interval + daylevel, data = new_DataActivity, mean)
names(StepsByDay)<- c("interval", "daylevel", "steps")

```

##### 2. Make a panel plot containing a time series plot

```{r}
xyplot (steps ~ interval|daylevel, StepsByDay, type = "l",layout =c(1,2),
        xlab ="Interval", ylab ="Average Number of Steps")

```

##### From the time series plot, we can conclude that on weekend, there are more movement activities happened compare on weekday.
