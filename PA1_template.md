# Reproducible Research: Peer Assessment 1


### Loading and peaking into the data


```r
unzip(zipfile = "activity.zip")
raw_data <- read.csv("activity.csv")
```

### Processing: removing NA from data


```r
good <- complete.cases(raw_data)
data <- raw_data[good,] 

head(data)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```


### Calculate the total number of steps taken per day


```r
steps_per_day <- aggregate(data$steps, by=list(day = data$date), "sum")
head(steps_per_day)
```

```
##          day     x
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```


### Make a histogram of the total number of steps taken each day


```r
hist(steps_per_day$x, main="Histogram", xlab="Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Calculate and report the mean and median of the total number of steps taken per day

#### Mean 


```r
mean(steps_per_day$x)
```

```
## [1] 10766.19
```

#### Median 


```r
median(steps_per_day$x)
```

```
## [1] 10765
```


### What is the average daily activity pattern?

 
Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
require(graphics)

activity <- aggregate( formula = steps~interval, 
           data = data,
           FUN = mean)

#peak into the data
head(activity)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
#show plot
plot(activity , type="l") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#get the active interval
active_interval <- activity[which.max(activity$steps),]

#print the interval
active_interval
```

```
##     interval    steps
## 104      835 206.1698
```

```r
#get the hour
hr <- active_interval / 60

#this could need converting into hh:mm, but I guess for purpose of homework, this is enough
hr
```

```
##     interval    steps
## 104 13.91667 3.436164
```

### Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
total_na <- sum(is.na(raw_data$steps))

total_na
```

```
## [1] 2304
```

###  Fill the missing values data


```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps)) 
        filled <- c(steps) else filled <-  activity$steps[ which(activity$interval == interval )]
    return(filled)
}

new_data <- raw_data

new_data$steps <- mapply(fill.value, new_data$steps, new_data$interval)

new_activity <- aggregate( formula = new_data$steps~new_data$interval, 
           data = new_data,
           FUN = mean)

plot(activity, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
plot(new_activity, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

### Are there differences in activity patterns between weekdays and weekends?


```r
add_week_flag <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("midweek") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}

new_data$date <- as.Date(new_data$date)

new_data$day <- sapply(new_data$date, FUN = add_week_flag)

## I haven't completed the full R course yet, there has to be a better way to do this.
midweek_data <- subset(new_data, day=="midweek")

weekend_data <- subset(new_data, day=="weekend")


##head(weekend_data , 5000L)
 
activity_midweek <- aggregate( formula = steps~interval, 
           data= midweek_data,
           FUN = mean)

plot(activity_midweek , type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
activity_weekend <- aggregate( formula = steps~interval, 
           data= weekend_data,
           FUN = mean)

plot(activity_weekend, type = "l" )
```

![](PA1_template_files/figure-html/unnamed-chunk-11-2.png)<!-- -->
