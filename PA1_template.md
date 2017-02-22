---
title: "Week2-Assignment"
author: "Ken Gu"
date: "2017-Feb-21"
output: html_document 
---

The Week 2 Assignment

Part 1: Loading and preprocessing the data


```r
library(dplyr)
library(ggplot2)
library(grid)

# read data from csv
#setwd("~/Documents/datascience/reproduciable/week2")
rawData <- read.csv("activity.csv", header = TRUE, sep=",")
```

Part 2: What is mean total number of steps taken per day?


```r
rawData2 <- mutate(rawData, date = as.Date(rawData$date, format = "%Y-%m-%d"))
groupby1<- group_by(rawData2, date)
totalNumSteps <- summarise(groupby1, totalnumberOfSteps=sum(steps, na.rm=TRUE))
```
Graph: the total number of steps taken each day

```r
ggplot(totalNumSteps, aes(totalnumberOfSteps)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
The mean number of steps taken each day reported.

```r
meanSteps <- mean(totalNumSteps$totalnumberOfSteps)
meanSteps
```

```
## [1] 9354.23
```
The mean number of steps taken each day reported.

```r
medianSteps <- median(totalNumSteps$totalnumberOfSteps)
medianSteps
```

```
## [1] 10395
```

Part 3: What is the average daily activity pattern?
Graph: time series plot of the average number of steps taken vs. the 5-minute intervals?

```r
groupby3<- group_by(rawData2, interval)
avgAP <- summarise(groupby3, totalnumberOfSteps=mean(steps, na.rm=TRUE))
maxnx <- subset(avgAP, avgAP[,2]==max(avgAP[,2]))[,1]
maxny <- subset(avgAP, avgAP[,2]==max(avgAP[,2]))[,2]
ggplot(avgAP, aes(interval, totalnumberOfSteps)) + geom_line(linetype=1) + geom_point(aes(x=maxnx, y=maxny), colour="red", stat="identity") + geom_vline(xintercept = as.numeric(maxnx), colour="red") 
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
The maximum number of steps:

```r
maxny
```

```
## # A tibble: 1 × 1
##   totalnumberOfSteps
##                <dbl>
## 1           206.1698
```
Part 4: Imputing missing values
Code Strategy: Use complete.case function to filter out all NA values

```r
# use mean for that day
rawData <- rawData[complete.cases(rawData[,1], rawData[,2], rawData[,3]),]
missV1 <- is.na(rawData[,1])
length(missV1[missV1==TRUE])
```

```
## [1] 0
```

```r
missV2 <- is.na(rawData[,2])
length(missV2[missV2==TRUE])
```

```
## [1] 0
```

```r
missV3 <- is.na(rawData[,3])
length(missV3[missV3==TRUE])
```

```
## [1] 0
```

```r
rawData4 <- rawData
for (i in 1:length(rawData4))  {
  if (is.na(rawData4[i,1])==TRUE) {
    tmp <- subset(data4, date==as.Date(rawData4[i,2], format = "%Y-%m-%d" ))
    rawData4[i,1] <- tmp[,1]
  }
}
groupby4<- group_by(rawData4, date)
totalNumSteps4 <- summarise(groupby1, totalnumberOfSteps=sum(steps, na.rm=TRUE))
```
The graph:  the total number of steps taken each day after missing values were imputed

```r
ggplot(totalNumSteps4, aes(totalnumberOfSteps)) + geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
meanSteps4 <- mean(totalNumSteps4$totalnumberOfSteps)
meanSteps4
```

```
## [1] 9354.23
```

```r
medianSteps4 <- median(totalNumSteps4$totalnumberOfSteps)
medianSteps4
```

```
## [1] 10395
```

Part 5: Are there differences in activity patterns between weekdays and weekends?


```r
rawData5 <- mutate(rawData, date = as.Date(rawData$date, format = "%Y-%m-%d"))

rawData5 <- mutate(rawData5, dayType="Weekday")
for (i in 1:nrow(rawData5))  {
  tmp <- rawData5[i,2]

  if (weekdays(tmp)=="星期六") {
    rawData5[i,4] <- "Weekend"
  }
    if (weekdays(tmp)=="星期日") {
    rawData5[i,4] <- "Weekend"
  }
}
rawDataWD5 <- filter(rawData5, dayType=="Weekday")
groupbyWD5<- group_by(rawDataWD5, interval)
dataWD5 <- summarise(groupbyWD5, totalnumberOfSteps=mean(steps, na.rm=TRUE))
dataWD5 <- mutate(dataWD5, dType="Weekday")
rawDataWK5 <- filter(rawData5, dayType=="Weekend")
groupbyWK5<- group_by(rawDataWK5, interval)
dataWK5 <- summarise(groupbyWK5, totalnumberOfSteps=mean(steps, na.rm=TRUE))
dataWK5 <- mutate(dataWK5, dType="Weekend")
data5 <- rbind(dataWD5, dataWK5)
```
Graph: panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
g5 <- ggplot(data5, aes(interval, totalnumberOfSteps)) + geom_line(linetype=1, aes(color=dType))  + facet_wrap(~ dType, scales = "free", nrow=2, ncol=1)
print(g5)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)


