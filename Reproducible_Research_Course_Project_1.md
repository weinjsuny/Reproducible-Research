This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

Loading and preprocessing the data
----------------------------------

    setwd("/Users/junwen/Documents/Coursera/Data Sciences/Reproducible Research")
    df <- read.csv("activity.csv")
    summary(df)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

What is mean total number of steps taken per day?
-------------------------------------------------

    df.total_step <- by(df$steps, df$date, sum, na.rm = TRUE)
    df.total_step <- df.total_step[which(df.total_step != 0)]
    hist(df.total_step, 
         main="Histogram for Total Number of Steps", 
         xlab="total number of steps taken per day")

![](Reproducible_Research_Course_Project_1_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Calculate and report the mean and median of the total number of steps
taken per day:

    mean(df.total_step)

    ## [1] 10766.19

    median(df.total_step)

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

<!-- -->

    df.average_step <- aggregate(steps~interval, df, mean, na.rm = TRUE)
    plot(steps~interval, data = df.average_step, type = "l")

![](Reproducible_Research_Course_Project_1_files/figure-markdown_strict/unnamed-chunk-4-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    df.average_step[which.max(df.average_step$steps), ]$interval

    ## [1] 835

Imputing missing values:
------------------------

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with 𝙽𝙰s)

<!-- -->

    sum(is.na(df$steps))

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset.

<!-- -->

    df.filled <- df
    filtered <- is.na(df$steps)
    by_interval <- tapply(df$steps, df$interval, median, na.rm=TRUE)
    df.filled$steps[filtered] <- by_interval[as.character(df$interval[filtered])]

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day.

<!-- -->

    total_step <- aggregate(steps~date, df.filled, sum, na.rm = TRUE)
    hist(total_step$steps, 
         main="Histogram for Total Number of Steps after Imputing", 
         xlab="total number of steps taken per day")

![](Reproducible_Research_Course_Project_1_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    mean(total_step$steps)

    ## [1] 9503.869

    median(total_step$steps)

    ## [1] 10395

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    df$date <- as.Date(strptime(df$date, format="%Y-%m-%d"))
    df$datetype <- sapply(df$date, function(x) {
                                if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                                  {y <- "Weekend"} else 
                                  {y <- "Weekday"}
                                  y
                                })
    head(df)

    ##   steps       date interval datetype
    ## 1    NA 2012-10-01        0  Weekday
    ## 2    NA 2012-10-01        5  Weekday
    ## 3    NA 2012-10-01       10  Weekday
    ## 4    NA 2012-10-01       15  Weekday
    ## 5    NA 2012-10-01       20  Weekday
    ## 6    NA 2012-10-01       25  Weekday

1.  Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).

<!-- -->

    library(ggplot2)
    by_date <- aggregate(steps~interval + datetype, df, mean, na.rm = TRUE)
    plot<- ggplot(by_date, aes(x = interval , y = steps, color = datetype)) +
           geom_line() +
           labs(title = "Average Daily Steps by Datetype", x = "Interval", y = "Average number of Steps") +
           facet_wrap(~datetype, ncol = 1, nrow=2)
    print(plot)

![](Reproducible_Research_Course_Project_1_files/figure-markdown_strict/unnamed-chunk-11-1.png)
