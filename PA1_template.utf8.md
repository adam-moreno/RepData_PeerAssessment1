---
title: "Movement Activity Analysis from Personal Devices"
author: "Adam Moreno"
date: "October 10, 2018"
output: html_document
---



##1 Reading in and processing activity data

```r
#1 Reading in and processing activity data

coursework <- "C:\\Users\\morenoa\\Documents\\Coursera - Johns Hopkins\\Reproducible Research\\Assignment 1"
setwd(coursework)
activity <- read.csv("activity.csv")

#CONVERT DATE COLUMN TO POSIX & REMOVE NAs
activity$date <- as.POSIXct(activity$date, format = "%Y-%m-%d")
data <- na.omit(activity)
```

##2 Histogram of total number of steps each day

```r
library(ggplot2)
#PLOT STEPS BY DAY

hist(data$steps, ylab = "Range of Steps", xlab = "Steps", main = "Range of frequency ")
```

<img src="PA1_template_files/figure-html/initial hist by day-1.png" width="672" />

```r
###I DON'T UNDERSTAND WHY A BARCHART WOULDN'T BE MORE USEFUL HERE...PLEASE SEE BELOW

data$weekday <- weekdays(data$date, abbr = TRUE)
#barplot(data$steps, names.arg = data$weekday, space = 0, ylab = "Steps", xlab = "Days", main = "Steps taken Each Day")
daily_groups <- aggregate(data[,1], list(data$weekday), sum)
barplot(daily_groups$x, names.arg = daily_groups$Group.1, space = 0, ylab = "Steps", xlab = "Days", main = "Steps taken Each Day")
```

<img src="PA1_template_files/figure-html/initial hist by day-2.png" width="672" />

```r
#Any tips on how to re-order this data by day of the week would be much appreciated!
```

##3 Mean and median number of steps taken each day


```r
#could use summary() to find these numbers as well
mean(data$steps, na.rm = T)
```

```
## [1] 37.3826
```

```r
median(data$steps, na.rm = T)
```

```
## [1] 0
```

##4 Time series plot of average number of steps taken


```r
avg_steps_day <- data.frame(tapply(data[,1], data$date, mean))
plot(avg_steps_day, main = "Average Steps per Day", ylab = "Steps", xlab = "Days", type = "l")
```

<img src="PA1_template_files/figure-html/time series-1.png" width="672" />

```r
#Shows how many days this study ran, and visually see outliers towards tail ends of the study
```

##5 The 5-minute interval that, on average, contains the max number of steps


```r
#Shows group # and corresponding max average
groups <- aggregate(data[,1], list(data$interval), mean)
groups[which.max(groups$x), "x", drop = FALSE]
```

```
##            x
## 104 206.1698
```

##6 Imputing missing data


```r
#Install necessary packages to visualize where NAs exist within original df
#install.packages("mice")

library(mice)


#install.packages("VIM")
library(VIM)

str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
md.pattern(activity)
```

<img src="PA1_template_files/figure-html/imputation-1.png" width="672" />

```
##       date interval steps     
## 15264    1        1     1    0
## 2304     1        1     0    1
##          0        0  2304 2304
```

```r
##Showing inverted data, red is where NAs exist

###Taking a look at where the missing data lies
aggr_plot <- aggr(activity, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, 
                  labels = names(activity), cex.axis = .7, gap = 3, ylab = c("Histogram of missing Data", "Pattern"))
```

<img src="PA1_template_files/figure-html/imputation-2.png" width="672" />

```
## 
##  Variables sorted by number of missings: 
##  Variable     Count
##     steps 0.1311475
##      date 0.0000000
##  interval 0.0000000
```

```r
#PMM PRODUCES ERROR DUE TO HIGH PROBABILITY OF CREATING A LINEAR COMBINATION OF ANOTHER COLUMN
#USED CLASSIFICATION AND REGRESSION TREES METHOD TO BYPASS INVERSION
imputData <- mice(activity, m=1, maxit=500, method = 'cart', seed = 500, trace = FALSE)
```

```
## 
##  iter imp variable
##   1   1  steps
##   2   1  steps
##   3   1  steps
##   4   1  steps
##   5   1  steps
##   6   1  steps
##   7   1  steps
##   8   1  steps
##   9   1  steps
##   10   1  steps
##   11   1  steps
##   12   1  steps
##   13   1  steps
##   14   1  steps
##   15   1  steps
##   16   1  steps
##   17   1  steps
##   18   1  steps
##   19   1  steps
##   20   1  steps
##   21   1  steps
##   22   1  steps
##   23   1  steps
##   24   1  steps
##   25   1  steps
##   26   1  steps
##   27   1  steps
##   28   1  steps
##   29   1  steps
##   30   1  steps
##   31   1  steps
##   32   1  steps
##   33   1  steps
##   34   1  steps
##   35   1  steps
##   36   1  steps
##   37   1  steps
##   38   1  steps
##   39   1  steps
##   40   1  steps
##   41   1  steps
##   42   1  steps
##   43   1  steps
##   44   1  steps
##   45   1  steps
##   46   1  steps
##   47   1  steps
##   48   1  steps
##   49   1  steps
##   50   1  steps
##   51   1  steps
##   52   1  steps
##   53   1  steps
##   54   1  steps
##   55   1  steps
##   56   1  steps
##   57   1  steps
##   58   1  steps
##   59   1  steps
##   60   1  steps
##   61   1  steps
##   62   1  steps
##   63   1  steps
##   64   1  steps
##   65   1  steps
##   66   1  steps
##   67   1  steps
##   68   1  steps
##   69   1  steps
##   70   1  steps
##   71   1  steps
##   72   1  steps
##   73   1  steps
##   74   1  steps
##   75   1  steps
##   76   1  steps
##   77   1  steps
##   78   1  steps
##   79   1  steps
##   80   1  steps
##   81   1  steps
##   82   1  steps
##   83   1  steps
##   84   1  steps
##   85   1  steps
##   86   1  steps
##   87   1  steps
##   88   1  steps
##   89   1  steps
##   90   1  steps
##   91   1  steps
##   92   1  steps
##   93   1  steps
##   94   1  steps
##   95   1  steps
##   96   1  steps
##   97   1  steps
##   98   1  steps
##   99   1  steps
##   100   1  steps
##   101   1  steps
##   102   1  steps
##   103   1  steps
##   104   1  steps
##   105   1  steps
##   106   1  steps
##   107   1  steps
##   108   1  steps
##   109   1  steps
##   110   1  steps
##   111   1  steps
##   112   1  steps
##   113   1  steps
##   114   1  steps
##   115   1  steps
##   116   1  steps
##   117   1  steps
##   118   1  steps
##   119   1  steps
##   120   1  steps
##   121   1  steps
##   122   1  steps
##   123   1  steps
##   124   1  steps
##   125   1  steps
##   126   1  steps
##   127   1  steps
##   128   1  steps
##   129   1  steps
##   130   1  steps
##   131   1  steps
##   132   1  steps
##   133   1  steps
##   134   1  steps
##   135   1  steps
##   136   1  steps
##   137   1  steps
##   138   1  steps
##   139   1  steps
##   140   1  steps
##   141   1  steps
##   142   1  steps
##   143   1  steps
##   144   1  steps
##   145   1  steps
##   146   1  steps
##   147   1  steps
##   148   1  steps
##   149   1  steps
##   150   1  steps
##   151   1  steps
##   152   1  steps
##   153   1  steps
##   154   1  steps
##   155   1  steps
##   156   1  steps
##   157   1  steps
##   158   1  steps
##   159   1  steps
##   160   1  steps
##   161   1  steps
##   162   1  steps
##   163   1  steps
##   164   1  steps
##   165   1  steps
##   166   1  steps
##   167   1  steps
##   168   1  steps
##   169   1  steps
##   170   1  steps
##   171   1  steps
##   172   1  steps
##   173   1  steps
##   174   1  steps
##   175   1  steps
##   176   1  steps
##   177   1  steps
##   178   1  steps
##   179   1  steps
##   180   1  steps
##   181   1  steps
##   182   1  steps
##   183   1  steps
##   184   1  steps
##   185   1  steps
##   186   1  steps
##   187   1  steps
##   188   1  steps
##   189   1  steps
##   190   1  steps
##   191   1  steps
##   192   1  steps
##   193   1  steps
##   194   1  steps
##   195   1  steps
##   196   1  steps
##   197   1  steps
##   198   1  steps
##   199   1  steps
##   200   1  steps
##   201   1  steps
##   202   1  steps
##   203   1  steps
##   204   1  steps
##   205   1  steps
##   206   1  steps
##   207   1  steps
##   208   1  steps
##   209   1  steps
##   210   1  steps
##   211   1  steps
##   212   1  steps
##   213   1  steps
##   214   1  steps
##   215   1  steps
##   216   1  steps
##   217   1  steps
##   218   1  steps
##   219   1  steps
##   220   1  steps
##   221   1  steps
##   222   1  steps
##   223   1  steps
##   224   1  steps
##   225   1  steps
##   226   1  steps
##   227   1  steps
##   228   1  steps
##   229   1  steps
##   230   1  steps
##   231   1  steps
##   232   1  steps
##   233   1  steps
##   234   1  steps
##   235   1  steps
##   236   1  steps
##   237   1  steps
##   238   1  steps
##   239   1  steps
##   240   1  steps
##   241   1  steps
##   242   1  steps
##   243   1  steps
##   244   1  steps
##   245   1  steps
##   246   1  steps
##   247   1  steps
##   248   1  steps
##   249   1  steps
##   250   1  steps
##   251   1  steps
##   252   1  steps
##   253   1  steps
##   254   1  steps
##   255   1  steps
##   256   1  steps
##   257   1  steps
##   258   1  steps
##   259   1  steps
##   260   1  steps
##   261   1  steps
##   262   1  steps
##   263   1  steps
##   264   1  steps
##   265   1  steps
##   266   1  steps
##   267   1  steps
##   268   1  steps
##   269   1  steps
##   270   1  steps
##   271   1  steps
##   272   1  steps
##   273   1  steps
##   274   1  steps
##   275   1  steps
##   276   1  steps
##   277   1  steps
##   278   1  steps
##   279   1  steps
##   280   1  steps
##   281   1  steps
##   282   1  steps
##   283   1  steps
##   284   1  steps
##   285   1  steps
##   286   1  steps
##   287   1  steps
##   288   1  steps
##   289   1  steps
##   290   1  steps
##   291   1  steps
##   292   1  steps
##   293   1  steps
##   294   1  steps
##   295   1  steps
##   296   1  steps
##   297   1  steps
##   298   1  steps
##   299   1  steps
##   300   1  steps
##   301   1  steps
##   302   1  steps
##   303   1  steps
##   304   1  steps
##   305   1  steps
##   306   1  steps
##   307   1  steps
##   308   1  steps
##   309   1  steps
##   310   1  steps
##   311   1  steps
##   312   1  steps
##   313   1  steps
##   314   1  steps
##   315   1  steps
##   316   1  steps
##   317   1  steps
##   318   1  steps
##   319   1  steps
##   320   1  steps
##   321   1  steps
##   322   1  steps
##   323   1  steps
##   324   1  steps
##   325   1  steps
##   326   1  steps
##   327   1  steps
##   328   1  steps
##   329   1  steps
##   330   1  steps
##   331   1  steps
##   332   1  steps
##   333   1  steps
##   334   1  steps
##   335   1  steps
##   336   1  steps
##   337   1  steps
##   338   1  steps
##   339   1  steps
##   340   1  steps
##   341   1  steps
##   342   1  steps
##   343   1  steps
##   344   1  steps
##   345   1  steps
##   346   1  steps
##   347   1  steps
##   348   1  steps
##   349   1  steps
##   350   1  steps
##   351   1  steps
##   352   1  steps
##   353   1  steps
##   354   1  steps
##   355   1  steps
##   356   1  steps
##   357   1  steps
##   358   1  steps
##   359   1  steps
##   360   1  steps
##   361   1  steps
##   362   1  steps
##   363   1  steps
##   364   1  steps
##   365   1  steps
##   366   1  steps
##   367   1  steps
##   368   1  steps
##   369   1  steps
##   370   1  steps
##   371   1  steps
##   372   1  steps
##   373   1  steps
##   374   1  steps
##   375   1  steps
##   376   1  steps
##   377   1  steps
##   378   1  steps
##   379   1  steps
##   380   1  steps
##   381   1  steps
##   382   1  steps
##   383   1  steps
##   384   1  steps
##   385   1  steps
##   386   1  steps
##   387   1  steps
##   388   1  steps
##   389   1  steps
##   390   1  steps
##   391   1  steps
##   392   1  steps
##   393   1  steps
##   394   1  steps
##   395   1  steps
##   396   1  steps
##   397   1  steps
##   398   1  steps
##   399   1  steps
##   400   1  steps
##   401   1  steps
##   402   1  steps
##   403   1  steps
##   404   1  steps
##   405   1  steps
##   406   1  steps
##   407   1  steps
##   408   1  steps
##   409   1  steps
##   410   1  steps
##   411   1  steps
##   412   1  steps
##   413   1  steps
##   414   1  steps
##   415   1  steps
##   416   1  steps
##   417   1  steps
##   418   1  steps
##   419   1  steps
##   420   1  steps
##   421   1  steps
##   422   1  steps
##   423   1  steps
##   424   1  steps
##   425   1  steps
##   426   1  steps
##   427   1  steps
##   428   1  steps
##   429   1  steps
##   430   1  steps
##   431   1  steps
##   432   1  steps
##   433   1  steps
##   434   1  steps
##   435   1  steps
##   436   1  steps
##   437   1  steps
##   438   1  steps
##   439   1  steps
##   440   1  steps
##   441   1  steps
##   442   1  steps
##   443   1  steps
##   444   1  steps
##   445   1  steps
##   446   1  steps
##   447   1  steps
##   448   1  steps
##   449   1  steps
##   450   1  steps
##   451   1  steps
##   452   1  steps
##   453   1  steps
##   454   1  steps
##   455   1  steps
##   456   1  steps
##   457   1  steps
##   458   1  steps
##   459   1  steps
##   460   1  steps
##   461   1  steps
##   462   1  steps
##   463   1  steps
##   464   1  steps
##   465   1  steps
##   466   1  steps
##   467   1  steps
##   468   1  steps
##   469   1  steps
##   470   1  steps
##   471   1  steps
##   472   1  steps
##   473   1  steps
##   474   1  steps
##   475   1  steps
##   476   1  steps
##   477   1  steps
##   478   1  steps
##   479   1  steps
##   480   1  steps
##   481   1  steps
##   482   1  steps
##   483   1  steps
##   484   1  steps
##   485   1  steps
##   486   1  steps
##   487   1  steps
##   488   1  steps
##   489   1  steps
##   490   1  steps
##   491   1  steps
##   492   1  steps
##   493   1  steps
##   494   1  steps
##   495   1  steps
##   496   1  steps
##   497   1  steps
##   498   1  steps
##   499   1  steps
##   500   1  steps
```

```r
##ANY TIPS HOW TO REMOVE THESE ITERATIONS FROM THE .HTML DOC WOULD BE APPRECIATED!!


#insert imputed data back into original df
final_data <- complete(imputData, 1)

#compare distributions of original and imputed data
xyplot(imputData, steps ~ interval, pch = 18, cex = 1)
```

<img src="PA1_template_files/figure-html/imputation-3.png" width="672" />

```r
#imputed points (magenta) are in line with observed points (blue)
```

##7 Histogram of the total number of steps taken each day after missing values are imputed


```r
#find weekday of imputed data
final_data$weekday <- weekdays(final_data$date, abbr = TRUE)
imputed_daily_groups <- aggregate(final_data[,1], list(final_data$weekday), sum)

ranges_imp <- range(imputed_daily_groups$x)

options(scipen = 5)
barplot(imputed_daily_groups$x, names.arg = imputed_daily_groups$Group.1, space = 0, ylab = "Steps", xlab = "Days", main = "Steps taken Each Day", xpd = FALSE)
```

<img src="PA1_template_files/figure-html/hist of imputation-1.png" width="672" />

##8 Average number of steps taken per 5-minute interval across weekdays vs weekends


```r
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
```

<img src="PA1_template_files/figure-html/5 min avg by wkdy vs wknd-1.png" width="672" />

```r
#Groupings ~1000 have a higher weekday average of steps than weekends, and tails are low average steps across all days
```






