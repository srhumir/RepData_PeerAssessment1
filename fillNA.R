fillNA <- function(activity){
        activityfull <- activity
        index <- which(is.na(activityfull$steps))
        for (i in index){
                activityfull$steps[i] <- round(mean(activityfull$steps[
                        activityfull$interval == activityfull$interval[i]],
                        na.rm = TRUE), digits = 0)
                }
        activityfull
}