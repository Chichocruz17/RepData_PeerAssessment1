---
title: "Reproducible Research Project Assignment 1"
author: "Luis Carlos Cruz Huertas"
date: "3/4/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```{r}

require(readr)
require(plyr)
require(tidyr)
require(dplyr)
require(ggplot2)
require(mice)
set.seed(1010)

```

## Data Set Read for processing Personal Movement



```{r dataset, echo=TRUE}
Xactivity <- read.csv("activity.csv")
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{r Xactivity, echo=TRUE}

NAelimination <- Xactivity[!is.na(Xactivity$steps),]

Xactivity$day <- weekdays(as.Date(Xactivity$date))
Xactivity$TimeDate <- as.POSIXct(Xactivity$date, format="%Y-%m-%d")
```

## Calculation of the total number of steps taken on a day. 

```{r}

newTable <- aggregate(Xactivity$steps ~ Xactivity$date, FUN = sum, )
colnames(newTable) <- c("Date", "Steps")

##Chart Histogram 

hist(newTable$Steps, breaks=5, xlab ="Steps", main = "Steps per day") 
```
## Mean and median number of steps taken each day 
```{r}

## Mean
as.integer(mean(newTable$Steps))

## Median 

as.integer(median(newTable$Steps))
```

## Time series plot of the average number of steps taken 

```{r}
interval20 <- ddply(NAelimination, .(interval), summarize, Avg = mean(steps)) 

## Line Plot 

average_daily_activity <- aggregate(Xactivity$steps, by=list(Xactivity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")
plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col="red", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```
## 5-Minute Interval 

```{r}
average_daily_activity[which.max(average_daily_activity$mean), ]$interval
```
## Code to describe and show a strategy for imputing missing data 

Calculate the Total number of missing values 

NA Calculation 
```{r}
nrow(Xactivity[is.na(Xactivity$steps), ])
```     
Strategy to fill in Empty missing steps values is to use the mean/median for that day, or the mean for that 5-minute interval.

```{r}
missing_steps <- average_daily_activity$mean[match(Xactivity$interval, average_daily_activity$interval)]

activity_missing <- transform(Xactivity, steps = ifelse(is.na(Xactivity$steps), yes = missing_steps, no = Xactivity$steps))
total_steps_missing <- aggregate(steps ~ date, activity_missing, sum)
names(total_steps_missing) <- c("date", "daily_steps") 
```
##Histogram of the total number of steps taken each day after missing values are imputed
```{r}

hist(total_steps_missing$daily_steps, col = "red", xlab = "Steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
```

## Calculation of the new median and new mean 
```{r}
mean(total_steps_missing$daily_steps)
median(total_steps_missing$daily_steps)
```
##Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r}
Xactivity$date <- as.Date(strptime(Xactivity$date, format="%Y-%m-%d"))
Xactivity$datetype <- sapply(Xactivity$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
```
## Panel to plot and activity series 

```{r}
activitydate <- aggregate(steps~interval + datetype, Xactivity, mean, na.rm = TRUE)
plot<- ggplot(activitydate, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```
