---
title: "PA1_Template"
output: html_document
---

# 1A - SET DIRECTORY AND DOWNLOAD/UNZIP DATA
```{r}
if (getwd() != "/Users/lylestewart/Desktop"){setwd( "/Users/lylestewart/Desktop"
)} else{ print(" We are already in the proper directory")}

if (!file.exists("data")){dir.create("data")} else{print("Directory Already Exists")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(fileUrl, temp, method = "curl")
activity <- unzip(temp)
activity <- read.csv(activity)
View(activity) # quick glance 
dateDownloaded <- date()
```
# 1B Check class and add variables 
```{r}
str(activity) # check variable classes  
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d") 
```
# 1C Clean data for missing values 
```{r}
cleanData <- activity[!is.na(activity$steps),]
str(cleanData)
```

_____________________________________________
# 2 What is mean total of steps taken per day
# 2A - Create subset object with total steps
```{r}
stepsTotal <- aggregate(activity$steps ~ activity$date, FUN=sum)
colnames(stepsTotal)<- c("Date", "Steps")
```
#2B  - Calculate mean/median of steps per day
```{r}
paste("The Average Number of Steps Taken Per Day Was",as.integer(mean(stepsTotal$Steps)), "Steps")
paste("The Meaian Steps Per Day Was", as.integer(median(stepsTotal$Steps)),"Steps")
```
#2C  - Create histogram of steps per day 
```{r}
hist(stepsTotal$Steps, breaks=5, xlab="Steps", main = "Total Steps Per Day", col = "turquoise")
```

_____________________________________________
#3 What is the average daily activity pattern?
#3A Load Packages and show clean data 
```{r}
library(plyr)
library(ggplot2)
# Remember the data we cleaned from earlier
cleanData <- activity[!is.na(activity$steps),]
str(cleanData)
```

#3B Show average steps per interval through table
```{r}
intervalTable <- ddply(cleanData, .(interval), summarize, Avg = mean(steps))
str(intervalTable)
```

#3C Create the required line plot of average steps per interval data 
```{r}
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

#3D Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}

paste("The Max Number Of Steps In A 5 Minute Interval Was", as.integer( max(intervalTable$Avg)), "Steps")
paste("The 5 Minute Interval That Contained The Max Average Number of Steps Was The", intervalTable[intervalTable$Avg==maxSteps,1], "Interval" )
```

_____________________________________________
#4 Inputting The Missing Values
```{r}
#4A Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity$steps)) 
```

#4B Devise a strategy for filling in all of the missing values in the dataset. - We sub NAs with the Average whereas to minimize effect on original data. 
```{r}
avgTable <- ddply(cleanData, .(interval, day), summarize, Avg = mean(steps)) # avg steps per weekday/ interval 

nadata<- activity[is.na(activity$steps),] # all NAs included for later substitution

NA.AVG.Activity<-merge(nadata, avgTable, by=c("interval", "day")) # merge the NA and non NA data 

NA.AVG.Activity2 <- NA.AVG.Activity[,c(6,4,1,2,5)] # reorder new data in same format as the clean 
colnames(NA.AVG.Activity2) <- c("steps", "date", "interval", "day", "DateTime")

mergeData <- rbind(cleanData, NA.AVG.Activity2)#merge averages 
```
#4C Histogram - Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
stepsTotal2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum ) # devise sum of steps per data to compare with orig step.
colnames(stepsTotal2)<- c("Date", "Steps") 

paste("The Mean Number of Steps With NA Data Accounted For Is", as.integer(mean(stepsTotal2$Steps)),"Steps")

paste("The Median Of Steps With NA Data Accounted For Is", as.integer(median(stepsTotal2$Steps)), "Steps")
```

```{r}
hist(stepsTotal2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(stepsTotal$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Turquoise", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "turquoise") )
```

```{r}
#New Median = 11015, Old Median = 10765 steps. 
#New Mean = 10821 steps, Old Mean = 10766 steps.
#Distribution Shape has not Changed at all. 
```

_____________________________________________
#5 Are there differences in activity patterns between weekdays and weekends? - Make Interval Time Series
```{r}
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
library(lattice) 

intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
```

```{r}
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

```{r}
# Finding : Overall the step data varies between weekday and weekend. Various reasons could be the cause for this. 
```
knit2html("/Users/lylestewart/Desktop/Coursera/Reproducible Research/PA1.rmd", output = PA1.html)
