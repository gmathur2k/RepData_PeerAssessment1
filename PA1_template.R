## Load libraries needed
library(stringr)

###############################################################################
## 1.Code for reading in the dataset and/or processing the data
###############################################################################

## Clean up R workspace of previously defined objects:
rm(list=ls())

## Set working directory:
setwd("C:/Users/gagan.mathur/OneDrive/Career/Data Science Specialization/Course 5 Reproducible Research/Course Project 1")
getwd()

## Download Dataset: Activity Monitoring Data
## URL: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip*
zipFile <- "ActivityMonitoringData.zip"; datFile <- "activity.csv"
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",zipFile)
fList <- unzip(zipFile)
df_Act_All <- read.csv(datFile)
df_Act_All$date <- as.Date(df_Act_All$date,format="%Y-%m-%d")
df_Act_All$sIntvl <- sprintf("%04d", df_Act_All$interval)
df_Act_All$hour <- substr(df_Act_All$sIntvl,1,2)

str(df_Act_All)
print(df_Act_All[1:5,]$rownames)
df_Act <- df_Act_All[complete.cases(df_Act_All),]

class(df_Act); dim(df_Act); str(df_Act); summary(df_Act); head(df_Act)

###############################################################################
## 2.Histogram of the total number of steps taken each day
###############################################################################

df_steps_each_day <-aggregate(steps ~ date, data=df_Act, sum)
mean_steps_each_day <- round(mean(df_steps_each_day$steps))
median_steps_each_day <- round(median(df_steps_each_day$steps))

png(filename="plot1-1 Steps each day.png", width=640, height=480, units="px")
par(mfrow=c(1,1), mar=c(5,5,3,1))
hist(df_steps_each_day$steps, breaks=10, col="aliceblue", xlab="Steps per day", main="Histogram of Steps each day")
rug(df_steps_each_day$steps)
abline(v=mean_steps_each_day, col="blue", lwd=3, lty=3)
abline(v=median_steps_each_day, col="red", lwd=3, lty=4)
legend("topright", lwd=3, lty = c(3,4), col = c("blue", "red"), 
       legend = c(paste("Mean:  ",mean_steps_each_day),
                  paste("Median:",median_steps_each_day)))
dev.off()

###############################################################################
## 3.Mean and median number of steps taken each day
###############################################################################

print(paste("The mean number of steps taken each day is:", mean_steps_each_day))
print(paste("The median number of steps taken each day is:", median_steps_each_day))

###############################################################################
## 4.Time series plot of the average number of steps taken
###############################################################################

df_avg_steps_per_interval <-aggregate(steps ~ interval, data=df_Act, mean)
df_avg_steps_per_date <-aggregate(steps ~ date, data=df_Act, mean)
df_avg_steps_per_hour <-aggregate(steps ~ hour, data=df_Act, mean)
dim(df_avg_steps_per_interval); dim(df_avg_steps_per_date); dim(df_avg_steps_per_hour)
png(filename="plot1-2 Avg# Steps per Interval.png", width=640, height=480, units="px")
par(mfrow=c(1,1), mar=c(5,5,3,1))
with(df_avg_steps_per_interval, 
     plot(interval, steps, type="l", col="brown", lwd=2, 
          main="Time series: Average # steps per interval", 
          ylab="Average # Steps", xlab="5-min Intervals")
)
maxRow <- df_avg_steps_per_interval[which.max(df_avg_steps_per_interval$steps),]
maxRow$steps <- round(maxRow$steps,0)
abline(v=maxRow[1], col="blue", lwd=2, lty=2)
text(x=maxRow[1]*1.1, y=maxRow[2]*0.9, paste("Max Avg Steps \nper interval: ", round(maxRow[2],0)), pos=4)
dev.off()

min(df_Act$interval); max(df_Act$interval)
###############################################################################
## 5.Imputing missing values
###############################################################################
n_total <- nrow(df_Act_All)
n_missing <- nrow(df_Act_All[is.na(df_Act_All$steps),])
n_valid <- nrow(df_Act)
pct_missing <- round(n_missing*100/n_valid,0)

print(paste0("There are ", n_total, " records, of which ", n_valid, " are valid and ", n_missing, " (", pct_missing, "%) are missing."))


library(data.table)

dt_Act_All <- data.table(df_Act_All)
dt_avg_per_intv <- data.table(df_avg_steps_per_interval)
dt_avg_per_date <- data.table(df_avg_steps_per_date)
dt_avg_per_hour <- data.table(df_avg_steps_per_hour)
head(dt_avg_per_hour)
names(dt_avg_per_intv)[2] <- "mean_steps_per_intv"
names(dt_avg_per_date)[2] <- "mean_steps_per_date"
names(dt_avg_per_hour)[2] <- "mean_steps_per_hour"

## 1 Set JOIN key to join mean steps by interval:
setkey(dt_Act_All, interval)
setkey(dt_avg_per_intv, interval)

dt_Act_All <- merge(dt_Act_All,dt_avg_per_intv, all.x=TRUE)

## 2 Set JOIN key to join mean steps by date:
setkey(dt_Act_All, date)
setkey(dt_avg_per_date, date)

dt_Act_All <- merge(dt_Act_All,dt_avg_per_date, all.x=TRUE)

## 3 Set JOIN key to join mean steps by hour:
setkey(dt_Act_All, hour)
setkey(dt_avg_per_hour, hour)

dt_Act_All <- merge(dt_Act_All,dt_avg_per_hour, all.x=TRUE)

## Treat any missing values in means as 0:

## Fill in missing data:
dt_Act_All[is.na(mean_steps_per_date),]$mean_steps_per_date <- 0
dt_Act_All[is.na(steps),]$steps <- 
  dt_Act_All[is.na(steps),]$mean_steps_per_date +
  dt_Act_All[is.na(steps),]$mean_steps_per_hour +
  dt_Act_All[is.na(steps),]$mean_steps_per_intv

## head(dt_Act_All[!complete.cases(df_Act_All),])
## dim(dt_Act_All)
## head(dt_Act_All[is.na(steps),])

## Make histogram again including missing data:
dt_all_steps_each_day <-aggregate(steps ~ date, data=dt_Act_All, sum)
mean_all_steps_each_day <- round(mean(df_steps_each_day$steps))
median_all_steps_each_day <- round(median(df_steps_each_day$steps))


png(filename="plot1-3 All Steps each day.png", width=640, height=480, units="px")
par(mfrow=c(1,1), mar=c(5,5,3,1))
hist(dt_all_steps_each_day$steps, breaks=10, col="aliceblue", xlab="Total Steps per day", main="Hist. Steps each day incl missing data")
rug(dt_all_steps_each_day$steps)
abline(v=mean_all_steps_each_day, col="green", lwd=3, lty=3)
abline(v=median_all_steps_each_day, col="brown", lwd=3, lty=4)
legend("topright", lwd=3, lty = c(3,4), col = c("green", "brown"), 
       legend = c(paste("Mean:  ",mean_all_steps_each_day),
                  paste("Median:",median_all_steps_each_day)))
dev.off()

print(paste("The mean number of steps taken each day (incl. missing data) is:", mean_all_steps_each_day))
print(paste("The median number of steps taken each day (incl. missing data) is:", median_all_steps_each_day))

###############################################################################
## 6. Are there differences in activity patterns between weekdays and weekends?
###############################################################################

dt_Act_All$weekday <- weekdays(dt_Act_All$date)
dt_Act_All$weekend_ind <- factor(dt_Act_All$weekday %in% c("Saturday","Sunday"), levels=c(TRUE,FALSE), labels=c("weekend","weekday"))
head(dt_Act_All)


df_avg_steps_per_intv_wend <-aggregate(steps ~ interval, data=dt_Act_All[weekend_ind=="weekend",], mean)
df_avg_steps_per_intv_wday <-aggregate(steps ~ interval, data=dt_Act_All[weekend_ind=="weekday",], mean)

png(filename="plot1-4 Panel time-series plot .png", width=640, height=800, units="px")
par(mfrow=c(2,1), mar=c(5,5,3,1))
with(df_avg_steps_per_intv_wend, 
     plot(interval, steps, type="l", col="green", lwd=2, 
          main="Time series: Average # steps per interval (weekend)", 
          ylab="Average # Steps", xlab="5-min Intervals")
)
maxRowWend <- df_avg_steps_per_intv_wend[which.max(df_avg_steps_per_intv_wend$steps),]
maxRowWend$steps <- round(maxRowWend$steps,0)
abline(v=maxRowWend[1], col="brown", lwd=2, lty=2)
text(x=maxRowWend[1]*1.1, y=maxRowWend[2]*0.9, paste("Max Avg Steps \nper interval: ", round(maxRowWend[2],0)), pos=4)

with(df_avg_steps_per_intv_wday, 
     plot(interval, steps, type="l", col="red", lwd=2, 
          main="Time series: Average # steps per interval (weekday)", 
          ylab="Average # Steps", xlab="5-min Intervals")
)
maxRowWday <- df_avg_steps_per_intv_wday[which.max(df_avg_steps_per_intv_wday$steps),]
maxRowWday$steps <- round(maxRowWday$steps,0)
abline(v=maxRowWday[1], col="blue", lwd=2, lty=2)
text(x=maxRowWday[1]*1.1, y=maxRowWday[2]*0.9, paste("Max Avg Steps \nper interval: ", round(maxRowWday[2],0)), pos=4)
dev.off()

