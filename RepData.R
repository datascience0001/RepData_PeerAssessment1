library(lattice)

# (A) Loading and preprocessing the data
activity<-read.csv("activity.csv")

totalByDate<-aggregate(steps~date, data=activity, sum, na.rm=TRUE)

# (B) mean total number of steps taken per day

# Create Histogram
png("Histogram of the total number of steps taken each day.png", height = 480, width = 480)
hist(	totalByDate$steps, 
      col = "red",
      main = "Histogram of the total number of steps taken each day",
      xlab = "Number of steps taken per day",
      ylab = "Frequency" )
dev.off()       
      
mean(totalByDate$steps)
median(totalByDate$steps)

# (C) Average daily activity pattern

meanByInterval<-aggregate(steps~interval, data=activity, mean, na.rm=TRUE)

png("Average daily activity pattern.png", height = 480, width = 480)
plot(	steps~interval, 
      data = meanByInterval,
      type = "l", 
      main = "Average daily activity pattern",
      xlab = "Interval", 
      ylab = "Mean")
dev.off() 

maxsteps = max(activity$steps, na.rm=TRUE)
activity[which.max(activity$steps),]$interval


# (D) Inputing missing values
sum(is.na(activity$steps))

GetSimulatedData<-function(interval) {
    meanByInterval[meanByInterval$interval==interval,]$steps
}

activitySimulated<-activity
for(i in 1:nrow(activitySimulated)){
    if(is.na(activitySimulated[i,]$steps)){
        activitySimulated[i,]$steps<-GetSimulatedData(activitySimulated[i,]$interval)
    }
}

totalByDateSimulated<-aggregate(steps~date, data=activitySimulated, sum, na.rm=TRUE)

png("Histogram of the total number of SIMULATED steps taken each day.png", height = 480, width = 480)
hist(	totalByDate$steps,
      col = "red",
      main = "Histogram of the total number of steps taken each day",
      xlab = "Number of steps taken per day",
      ylab = "Frequency" )
dev.off()       
      
mean(totalByDateSimulated$steps)
median(totalByDateSimulated$steps)

# (E) Differences in activity patterns between weekdays and weekends
activitySimulated$wday <- ifelse( (as.POSIXlt(as.Date(activitySimulated$date))$wday-1 %% 7) >= 5,
                                  "weekend",
                                  "weekday" )
totalByWeekdaySimulated<-aggregate(steps~interval+wday, data=activitySimulated, mean, na.rm=TRUE)
png("Panel plot of simulated data.png", height = 480, width = 480)
xyplot( steps~interval|factor(wday),
        data=totalByWeekdaySimulated,
        ylab="Number of steps",
        type="l",        
        aspect=1/2
        )
dev.off()        