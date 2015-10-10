# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data <- tbl_df(read.csv(unz("activity.zip", "activity.csv")))
head(data)
```

```
## Source: local data frame [6 x 3]
## 
##   steps       date interval
##   (int)     (fctr)    (int)
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

```r
library(dplyr)
data <- tbl_df(read.csv(unz("activity.zip", "activity.csv")))
stepdata <- data %>% 
     group_by(date) %>% 
     summarize(total_steps = sum(steps, na.rm=TRUE))
head(stepdata)
```

```
## Source: local data frame [6 x 2]
## 
##         date total_steps
##       (fctr)       (int)
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

```r
hist(stepdata$total_steps, 
     breaks=20, 
     main="Total Daily Steps", 
     xlab="Total Number of Steps Per Day",
     col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mn <- mean(stepdata$total_steps, na.rm = TRUE)
md <- median(stepdata$total_steps, na.rm = TRUE)
rbind(mean=mn, median = md)
```

```
##            [,1]
## mean    9354.23
## median 10395.00
```



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
