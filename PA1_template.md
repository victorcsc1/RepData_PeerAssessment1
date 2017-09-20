# PA1_template
Victor Coelho  
11 de setembro de 2017  



```r
knitr::opts_chunk$set(echo = TRUE)
```
#First we are going to load the packages required to process the data and load the data.

```r
library(plyr)
library(ggplot2)
library(knitr)
library(markdown)
library(rmarkdown)
setwd("~/reproducible research")
dataset <- read.csv("activity.csv")
```
## What is mean total number of steps taken per day?

```r
steps <- aggregate(steps ~ date, dataset, sum)
hist(steps$steps, main = "Total Steps of each Day", col="red", xlab="number of steps", ylim=c(0,30))
```

![](PA1_template_files/figure-html/activity data-1.png)<!-- -->

```r
mean(steps$steps)
```

```
## [1] 10766.19
```

```r
median(steps$steps)
```

```
## [1] 10765
```
#Average of daily activity pattern? which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avsteps <- aggregate(steps ~ interval, dataset, mean)
ggplot(avsteps, aes(x=interval, y=steps))+ geom_line() + xlab("Interval") + ylab("Average of Steps") + ggtitle("Average steps per Interval")
```

![](PA1_template_files/figure-html/activity process-1.png)<!-- -->

```r
avsteps[which.max(avsteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
#104 day, max of 835 steps
```
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(dataset$steps))
```

```
## [1] 2304
```

```r
#2304
```
#Devise a strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in. 

```r
nvalue <- dataset
nvalue$steps <- ifelse(is.na(nvalue$steps), round(avsteps$steps[match(nvalue$interval, avsteps$interval)],0), nvalue$steps)
tnvalue <- data.frame(steps=nvalue$steps, interval=nvalue$interval, date=nvalue$date)
steps2 <- aggregate(steps ~ date, tnvalue, sum)
sum(is.na(steps2$steps))
```

```
## [1] 0
```
#Plot a histogram with the no-na value and report mean and median

```r
hist(steps2$steps, main = "Total Steps of each Day, with no NA-value", col="green", xlab="Steps", ylim=c(0,40))
```

![](PA1_template_files/figure-html/activi-1.png)<!-- -->

```r
mean(steps2$steps, na.rm=TRUE)
```

```
## [1] 10765.64
```

```r
median(steps2$steps, na.rm=TRUE)
```

```
## [1] 10762
```

```r
#The values do not had significant changes.Despite the increase of the median values as the na-values was set with mean value from the previous dataset
```
#Check if differences exist in activity patterns between weekdays and weekends

```r
steps3 <- nvalue
steps3$date <- as.Date(nvalue$date, format = "%Y-%m-%d")
steps3$week <- weekdays(steps3$date)
steps3$daytype <- ifelse(steps3$week== 'saturday' | steps3$week== 'sunday', 'weekend', 'weekday')
```
#Two time-series plot of 5minutes interval

```r
Fplot <- aggregate(steps ~ interval+daytype, data=steps3, FUN=mean, na.action=na.omit)
Fplot$time <- Fplot$interval/100
r <- ggplot(Fplot, aes(time, steps)) + geom_line() + ggtitle("Average steps per time interval: weekdays vs. weekends") + xlab("Time") + ylab("Steps")
ggplot(Fplot, aes(time, steps)) + geom_line() + ggtitle("Average steps per time interval: weekdays vs. weekends") + xlab("Time") + ylab("Steps")
```

![](PA1_template_files/figure-html/set-1.png)<!-- -->

```r
ggplot(Fplot, aes(time, steps)) + geom_line() + ggtitle("Average steps per time interval: weekdays vs. weekends") + xlab("Time") + ylab("Steps") + facet_grid(daytype ~.)
```

![](PA1_template_files/figure-html/set-2.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
