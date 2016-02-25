hist(activity$steps)
plot(activity$date,activity$steps)
plot(unique(activity$interval), 
     tapply(activity$steps, activity$interval, mean, na.rm = T),
     type = "l", main = "mean steps in each interval")
library(ggplot2)
qplot(steps, data = activity, color = weekday, ylim = c(0,300))
qplot(steps, data = activity, ylim = c(0,150), facets = weekday~.,
      fill = weekday, color = weekday, legend = FALSE)
qplot(steps, data = activity, ylim = c(0,150), geom = ,facets = wDay~.,
      fill = wDay, legend = FALSE)
qplot(interval,steps, data = activity, facets = wDay~.,
      fill = wDay, legend = FALSE)

library(dplyr)
b <- activity %>% group_by(interval, wDay) %>% 
        summarise(meansteps = mean(steps, na.rm = T))
qplot(interval, meansteps, data = b, facets = wDay~., geom = "line",
      main = "mean of steps on each intervals")
