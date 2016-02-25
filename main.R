##reading data
if (!file.exists("activity.csv")){
        unzip("activity.zip")
}
activity <- read.csv("activity.csv", colClasses = c("integer", "character", "integer"))
activity$date <- as.Date(activity$date, "%Y-%m-%d")
library(data.table)
activity <- data.table(activity)
activity[,weekday := weekdays(activity$date)]
wDays <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag","Freitag")
activity[,wDay := factor(activity$weekday %in% wDays,
                         levels = c(TRUE, FALSE), labels = c("weekday", "weekend"))]

library(dplyr)
#What is mean total number of steps taken per day?
b <- activity %>% group_by(date) %>% summarise(stepDay = sum(steps, na.rm = T))
hist(b$stepDay, main ="", xlab = "")
title(main = "Total steps per day", sub = paste("Mean = ", 
                                                as.character(floor(mean(b$stepDay))),
                                                "Mediam = ",
                                                as.character(median(b$stepDay))),
      xlab = "Total steps per day")


#What is the average daily activity pattern?
b <- activity %>% group_by(interval) %>% summarise(meanstepsinterval =mean(steps, na.rm =T))
plot(b$interval, b$meanstepsinterval, type = "l" , 
     main = "average of steps in each interval in all days")
print("intervals wiht the maimun averge steps")
b$interval[b$meanstepsinterval == max(b$meanstepsinterval)]

#Imputing missing values
print("Tortal number of missing values")
sum(is.na(activity$steps))
#Fill the null values by average steps in the same interval in all days
activityfull <- fillNA(activity)
#What is mean total number of steps taken per day?
b <- activityfull %>% group_by(date) %>% summarise(stepDay = sum(steps, na.rm = T))
hist(b$stepDay, main ="", xlab = "")
title(main = "Total steps per day", sub = paste("Mean = ", 
                                                as.character(floor(mean(b$stepDay))),
                                                "Mediam = ",
                                                as.character(median(b$stepDay))),
      xlab = "Total steps per day")
print("histogram is slightly different and mean and median very different")

#Are there differences in activity patterns between weekdays and weekends?
b <- activityfull %>% group_by(interval, wDay) %>% 
        summarise(meansteps = mean(steps, na.rm = T))
qplot(interval, meansteps, data = b, facets = wDay~., geom = "line",
      main = "mean of steps on each intervals",
      ylab = "Number of steps")
