---
output: html_document
---


## Loading and preprocessing the data
As a first step we reach out to the internet storage facility 
to retrieve the file containing the step per interval by date file.
This file is retrieved in zip form and we unzip it to recover the
data file. 

- Change fileURL1 to point at the repository, 
- Change fileURL2 to point at the file within the repository



```r
temp.zip<-tempfile()
fileURL1<-"http://d396qusza40orc.cloudfront.net/";  
fileURL2<-"repdata/data/activity.zip"  
download.file(paste(fileURL1,fileURL2,sep=""),temp.zip)  
zipfiledata<-unzip(temp.zip,list=TRUE)  
dfNA<-read.csv(unz(temp.zip,as.character(zipfiledata$Name)),header=TRUE,as.is=TRUE) 
df<-na.omit(dfNA)
rm(temp.zip)
```
## What is mean total number of steps taken per day?
We remove rows in which the steps value is  missing (NA) 
then we build a histogram and compute the mean and median
The histogram is constructed by first summing by date as a factor
and then plotting the histogram from these values


```r
m<-tapply(df$steps,df$date,sum)
hist(m,breaks=50,ylab="Frequency of Total Steps per Day",main="Histogram of Total Steps Taken Each Day")
```

![plot of chunk basicStats](figure/basicStats-1.png) 

#### Compute and Show the mean and median number of steps for each day
The date, mean value and median value are show for each day in the following 
table. The median value is to be zero for each day.  The median is defined
are the central location of the sorted data values. In the table all median values are zero.
In order to investigate this further we calculated median values for several randomly
chosen days and these showed consistent zero median results. 
Of the 15264 rows without missing data there are 4250 that contain non-zero steps values while 11014 contain zero steps values, giving a ratio 
of 72% of all steps entires as zero.    
We report the mean and median values over the total steps for all days following the table


```r
x<-tapply(df$steps,df$date,mean)
y<-tapply(df$steps,df$date,median)
sprintf("%s  %g  %g",rownames(x),x,y)
```

```
##  [1] "2012-10-02  0.4375  0"   "2012-10-03  39.4167  0" 
##  [3] "2012-10-04  42.0694  0"  "2012-10-05  46.1597  0" 
##  [5] "2012-10-06  53.5417  0"  "2012-10-07  38.2465  0" 
##  [7] "2012-10-09  44.4826  0"  "2012-10-10  34.375  0"  
##  [9] "2012-10-11  35.7778  0"  "2012-10-12  60.3542  0" 
## [11] "2012-10-13  43.1458  0"  "2012-10-14  52.4236  0" 
## [13] "2012-10-15  35.2049  0"  "2012-10-16  52.375  0"  
## [15] "2012-10-17  46.7083  0"  "2012-10-18  34.9167  0" 
## [17] "2012-10-19  41.0729  0"  "2012-10-20  36.0938  0" 
## [19] "2012-10-21  30.6285  0"  "2012-10-22  46.7361  0" 
## [21] "2012-10-23  30.9653  0"  "2012-10-24  29.0104  0" 
## [23] "2012-10-25  8.65278  0"  "2012-10-26  23.5347  0" 
## [25] "2012-10-27  35.1354  0"  "2012-10-28  39.7847  0" 
## [27] "2012-10-29  17.4236  0"  "2012-10-30  34.0938  0" 
## [29] "2012-10-31  53.5208  0"  "2012-11-02  36.8056  0" 
## [31] "2012-11-03  36.7049  0"  "2012-11-05  36.2465  0" 
## [33] "2012-11-06  28.9375  0"  "2012-11-07  44.7326  0" 
## [35] "2012-11-08  11.1771  0"  "2012-11-11  43.7778  0" 
## [37] "2012-11-12  37.3785  0"  "2012-11-13  25.4722  0" 
## [39] "2012-11-15  0.142361  0" "2012-11-16  18.8924  0" 
## [41] "2012-11-17  49.7882  0"  "2012-11-18  52.4653  0" 
## [43] "2012-11-19  30.6979  0"  "2012-11-20  15.5278  0" 
## [45] "2012-11-21  44.3993  0"  "2012-11-22  70.9271  0" 
## [47] "2012-11-23  73.5903  0"  "2012-11-24  50.2708  0" 
## [49] "2012-11-25  41.0903  0"  "2012-11-26  38.7569  0" 
## [51] "2012-11-27  47.3819  0"  "2012-11-28  35.3576  0" 
## [53] "2012-11-29  24.4688  0"
```
## The mean and median value over total steps for all days

```r
sprintf("mean and median value of sum of steps over all days %f   %f",mean(m),median(m))
```

```
## [1] "mean and median value of sum of steps over all days 10766.188679   10765.000000"
```
  
## What is the average daily activity pattern?
The following plot shows the average number of steps taken over the five minute intervals. 
The maximum number of steps found is shown by the red vertical line and red value shown at 
the x-axis. 

```r
x<-tapply(df$steps,df$interval,mean)
plot(y=x,,x=rownames(x),type="l",ylab="average number of steps",
          xlab="five minute intervals\nInterval containing maximum is shown as green line with value",main="Number of Steps over Five Minute Interval")
abline(v=xxx<-as.numeric(names(x[max(x)==x])),col="green")
text(x=(xxx+70),y=0,as.character(xxx),col="green")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
sprintf("Maximum Value is %g",as.numeric(x[max(x)==x]))
```

```
## [1] "Maximum Value is 206.17"
```
## Imputing missing values
Calculatge the total number of missing values in the data 

```r
sprintf("total number of missing values in data set is %d",sum(is.na(dfNA)))
```

```
## [1] "total number of missing values in data set is 2304"
```
We calculate imputed values by using the average (mean) value for each of the five minute intervals 
data frame df contains step values with NA rows removed, data frame dfNA contain all steps data including NA values. 
The strategy used to impute missing values is to replace these with the simple mean for that interval calculated 
over all days.  

```r
q<-tapply(dfNA$steps,dfNA$interval,mean,na.rm=TRUE)
b<-ifelse(is.na(dfNA$steps), q,dfNA$steps)
dfN<-data.frame(b,dfNA$date,dfNA$interval)
names(dfN)<-names(dfNA)
```

### Does imputing missing values in this way change the value of the mean and median?

```r
m<-tapply(dfN$steps,dfN$date,sum)
hist(m,breaks=50,ylab="Frequency of Total Steps per Day", 
     main="Histogram of Total Steps Taken Per Day \nMissing Data Replaced by Interval Mean Value")
```

![plot of chunk imputedBasicStatistics](figure/imputedBasicStatistics-1.png) 
  
### Calculate mean and median for dataset with mean value replacing missing values
The values in the following table are date, mean and median for each day.  Do the values 
differ from the those calculated in the first part of the asisgnment
and what is the impact on the total daily number of steps?   The answer 
to both questions is replacing missing values with values choosen from this
imputing strategy seem not to significantly change the results. The histogram shapes
appear similar as well.  Our table of mean and median values by day does show some
changes both averge and median values in various places. 

```r
x<-tapply(dfN$steps,dfN$date,mean)
y<-tapply(dfN$steps,dfN$date,median)  
sprintf("%s %f  %f",rownames(x),x,y)
```

```
##  [1] "2012-10-01 37.382600  34.113208" "2012-10-02 0.437500  0.000000"  
##  [3] "2012-10-03 39.416667  0.000000"  "2012-10-04 42.069444  0.000000" 
##  [5] "2012-10-05 46.159722  0.000000"  "2012-10-06 53.541667  0.000000" 
##  [7] "2012-10-07 38.246528  0.000000"  "2012-10-08 37.382600  34.113208"
##  [9] "2012-10-09 44.482639  0.000000"  "2012-10-10 34.375000  0.000000" 
## [11] "2012-10-11 35.777778  0.000000"  "2012-10-12 60.354167  0.000000" 
## [13] "2012-10-13 43.145833  0.000000"  "2012-10-14 52.423611  0.000000" 
## [15] "2012-10-15 35.204861  0.000000"  "2012-10-16 52.375000  0.000000" 
## [17] "2012-10-17 46.708333  0.000000"  "2012-10-18 34.916667  0.000000" 
## [19] "2012-10-19 41.072917  0.000000"  "2012-10-20 36.093750  0.000000" 
## [21] "2012-10-21 30.628472  0.000000"  "2012-10-22 46.736111  0.000000" 
## [23] "2012-10-23 30.965278  0.000000"  "2012-10-24 29.010417  0.000000" 
## [25] "2012-10-25 8.652778  0.000000"   "2012-10-26 23.534722  0.000000" 
## [27] "2012-10-27 35.135417  0.000000"  "2012-10-28 39.784722  0.000000" 
## [29] "2012-10-29 17.423611  0.000000"  "2012-10-30 34.093750  0.000000" 
## [31] "2012-10-31 53.520833  0.000000"  "2012-11-01 37.382600  34.113208"
## [33] "2012-11-02 36.805556  0.000000"  "2012-11-03 36.704861  0.000000" 
## [35] "2012-11-04 37.382600  34.113208" "2012-11-05 36.246528  0.000000" 
## [37] "2012-11-06 28.937500  0.000000"  "2012-11-07 44.732639  0.000000" 
## [39] "2012-11-08 11.177083  0.000000"  "2012-11-09 37.382600  34.113208"
## [41] "2012-11-10 37.382600  34.113208" "2012-11-11 43.777778  0.000000" 
## [43] "2012-11-12 37.378472  0.000000"  "2012-11-13 25.472222  0.000000" 
## [45] "2012-11-14 37.382600  34.113208" "2012-11-15 0.142361  0.000000"  
## [47] "2012-11-16 18.892361  0.000000"  "2012-11-17 49.788194  0.000000" 
## [49] "2012-11-18 52.465278  0.000000"  "2012-11-19 30.697917  0.000000" 
## [51] "2012-11-20 15.527778  0.000000"  "2012-11-21 44.399306  0.000000" 
## [53] "2012-11-22 70.927083  0.000000"  "2012-11-23 73.590278  0.000000" 
## [55] "2012-11-24 50.270833  0.000000"  "2012-11-25 41.090278  0.000000" 
## [57] "2012-11-26 38.756944  0.000000"  "2012-11-27 47.381944  0.000000" 
## [59] "2012-11-28 35.357639  0.000000"  "2012-11-29 24.468750  0.000000" 
## [61] "2012-11-30 37.382600  34.113208"
```
## The mean and median value over total steps for all days

```r
sprintf("mean and median value of sum of steps over all days (with imputed values) %f   %f",mean(m),median(m))
```

```
## [1] "mean and median value of sum of steps over all days (with imputed values) 10766.188679   10766.188679"
```
## Are there differences in activity between weekdays and weekends?

```r
days<-weekdays(as.Date(dfN$date)) 
dfN["weekend"]<-factor(ifelse( ( days=="Saturday" | days=="Sunday" ),"weekend","weekday"))
d<-with(dfN, tapply(steps,list(interval,weekend),mean))
par(mfrow=c(1,2))
plot(y=d[,1],x=rownames(d), type="l",xlim=c(0,max(dfN$interval)),ylim=c(0,200), main="Weekday",xlab="Step Intervals",ylab="Mean Step Per Interval")
plot(y=d[,2],x=rownames(d), type="l",xlim=c(0,max(dfN$interval)),ylim=c(0,200), main="Weekend",xlab="Step Intervals",ylab="Mean Step Per Interval")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
