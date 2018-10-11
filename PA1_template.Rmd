---
title: "Movement Activity Analysis from Personal Devices"
author: "Adam Moreno"
date: "October 10, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

##1 Reading in and processing activity data
```{r process}

#1 Reading in and processing activity data

coursework <- "C:\\Users\\morenoa\\Documents\\Coursera - Johns Hopkins\\Reproducible Research\\Assignment 1"
setwd(coursework)
activity <- read.csv("activity.csv")

#CONVERT DATE COLUMN TO POSIX & REMOVE NAs
activity$date <- as.POSIXct(activity$date, format = "%Y-%m-%d")
data <- na.omit(activity)

```

##2 Histogram of total number of steps each day
```{r initial hist by day}
library(ggplot2)
#PLOT STEPS BY DAY

hist(data$steps, ylab = "Range of Steps", xlab = "Steps", main = "Range of frequency ")
###I DON'T UNDERSTAND WHY A BARCHART WOULDN'T BE MORE USEFUL HERE...PLEASE SEE BELOW

data$weekday <- weekdays(data$date, abbr = TRUE)
#barplot(data$steps, names.arg = data$weekday, space = 0, ylab = "Steps", xlab = "Days", main = "Steps taken Each Day")
daily_groups <- aggregate(data[,1], list(data$weekday), sum)
barplot(daily_groups$x, names.arg = daily_groups$Group.1, space = 0, ylab = "Steps", xlab = "Days", main = "Steps taken Each Day")

#Any tips on how to re-order this data by day of the week would be much appreciated!

```

##3 Mean and median number of steps taken each day

```{r mean/median}

#could use summary() to find these numbers as well
mean(data$steps, na.rm = T)
median(data$steps, na.rm = T)

```

##4 Time series plot of average number of steps taken

```{r time series}


avg_steps_day <- data.frame(tapply(data[,1], data$date, mean))
plot(avg_steps_day, main = "Average Steps per Day", ylab = "Steps", xlab = "Days", type = "l")

#Shows how many days this study ran, and visually see outliers towards tail ends of the study

```

##5 The 5-minute interval that, on average, contains the max number of steps

```{r 5-min max}
#Shows group # and corresponding max average
groups <- aggregate(data[,1], list(data$interval), mean)
groups[which.max(groups$x), "x", drop = FALSE]

```

##6 Imputing missing data

```{r imputation, warning = FALSE, message= FALSE}
#Install necessary packages to visualize where NAs exist within original df
#install.packages("mice")

library(mice)


#install.packages("VIM")
library(VIM)

str(activity)
md.pattern(activity)

##Showing inverted data, red is where NAs exist

###Taking a look at where the missing data lies
aggr_plot <- aggr(activity, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, 
                  labels = names(activity), cex.axis = .7, gap = 3, ylab = c("Histogram of missing Data", "Pattern"))

#PMM PRODUCES ERROR DUE TO HIGH PROBABILITY OF CREATING A LINEAR COMBINATION OF ANOTHER COLUMN
#USED CLASSIFICATION AND REGRESSION TREES METHOD TO BYPASS INVERSION
imputData <- mice(activity, m=1, maxit=500, method = 'cart', seed = 500, trace = FALSE)

##ANY TIPS HOW TO REMOVE THESE ITERATIONS FROM THE .HTML DOC WOULD BE APPRECIATED!!


#insert imputed data back into original df
final_data <- complete(imputData, 1)

#compare distributions of original and imputed data
xyplot(imputData, steps ~ interval, pch = 18, cex = 1)
#imputed points (magenta) are in line with observed points (blue)
```

##7 Histogram of the total number of steps taken each day after missing values are imputed

```{r hist of imputation}
#find weekday of imputed data
final_data$weekday <- weekdays(final_data$date, abbr = TRUE)
imputed_daily_groups <- aggregate(final_data[,1], list(final_data$weekday), sum)

ranges_imp <- range(imputed_daily_groups$x)

options(scipen = 5)
barplot(imputed_daily_groups$x, names.arg = imputed_daily_groups$Group.1, space = 0, ylab = "Steps", xlab = "Days", main = "Steps taken Each Day", xpd = FALSE)


```

##8 Average number of steps taken per 5-minute interval across weekdays vs weekends

```{r 5 min avg by wkdy vs wknd}

#distinguish between weekends & weekdays
weekends <- subset(data, subset = (weekday == "Sat" | weekday == "Sun"))
weekdays <- subset(data, subset = (weekday != "Sat" | weekday != "Sun"))


#Create panel plot and groups by mean
par(mfrow=c(1,2))
weekday_groups <- aggregate(weekdays[,1], list(weekdays$interval), mean)
plot(weekday_groups, main = "Average Steps in Weekday", ylab = "Steps", xlab = "Days", type = "l")

ranges <- range(weekday_groups$x)

weekend_groups <- aggregate(weekends[,1], list(weekends$interval), mean)
plot(weekend_groups, main = "Average Steps in Weekends", ylab = "Steps", xlab = "Days", ylim = ranges, type = "l")

#Groupings ~1000 have a higher weekday average of steps than weekends, and tails are low average steps across all days


```






