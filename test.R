    
## Loading and preprocessing the data

    #Reading and confirming data
    zipfile <- "activity.zip"
    filename <- "activity.csv"
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    
    #File already exists?
    if (!file.exists(zipfile)){
        download.file(fileURL, zipfile)
    }
    
    #Zip exists?
    if (!file.exists(filename)) { 
        unzip(zipfile) 
    }
   
    #Store data for reading 
    arquivo <- read.table(filename, 
                          sep = ",",
                          header = TRUE,
                          na.strings = "NA",
                          colClasses = c('numeric','character','numeric'))
    
    #Re-storing dates values for better work
    library(lubridate)
    arquivo <- cbind(arquivo, ymd(arquivo[,2]))
    colnames(arquivo) <- c("steps", "date", "interval", "newDate")
    

## What is mean total number of steps taken per day?
    
    #Calculate the total number of steps taken per day

    tnspd <- aggregate(steps ~ newDate,
                       arquivo,
                       sum)
    
    #Make a histogram of the total number of steps 
    #taken each day
    hist(as.numeric(tnspd$steps), 
       xlab = "Steps",
       ylab = "Frequency",
       main = "Steps per Day",
       breaks = 15,
       col="red")
    
    #Calculate and report the mean and median of the 
    #total number of steps taken per day
    
    mnPD <- round(mean(tnspd$steps),digits = 2)
    print(paste("Mean: ", mnPD))
    
    mdPD <- round(median(tnspd$steps),digits = 2)
    print(paste("Median: ", mdPD))
 

## What is the average daily activity pattern?

    #Make a time series plot (i.e. type = "l") of 
    #the 5-minute interval (x-axis) and the average 
    #number of steps taken, averaged across all 
    #days (y-axis)

    
    intavg <- aggregate(steps ~ interval,
                       arquivo,
                       mean)
    plot(x = intavg$interval, 
         y = intavg$steps,
         type="l", 
         xlab = "Intervals", 
         ylab = "Avg steps avg days",
         main = "Time series")

    #Which 5-minute interval, on average across 
    #all the days in the dataset, contains the 
    #maximum number of steps?

    maxstepsIndex<- which.max(intavg$steps)
    
    print(paste("Max number steps by interval:", intavg[maxstepsIndex,]$interval))
    
    
## Imputing missing values
    
    #Calculate and report the total number of 
    #missing values in the dataset (i.e. the total 
    #number of rows with NA)

    print(paste("Total number of rows with NA:",
                sum(is.na(arquivo))))

    
    #Devise a strategy for filling in all of the 
    #missing values in the dataset. The strategy 
    #does not need to be sophisticated. For example,
    #you could use the mean/median for that day, or 
    #the mean for that 5-minute interval, etc.
    
    #It will be used a new mean from already calculated 
    #mean per interval, named "intavg" 
        
    intavgMean <- mean(intavg$steps)
    
    #Create a new dataset that is equal to the original 
    #dataset but with the missing data filled in.

    arquivo2 <- arquivo
    arquivo2[is.na(arquivo2$steps),1] <- intavgMean

    
    #Make a histogram of the total number of steps taken 
    #each day and Calculate and report the mean and median 
    #total number of steps taken per day. Do these values 
    #differ from the estimates from the first part of the 
    #assignment? What is the impact of imputing missing data 
    #on the estimates of the total daily number of steps?
 
    tnspd2 <- aggregate(steps ~ newDate,
                        arquivo2,
                        sum)
    
    hist(as.numeric(tnspd2$steps), 
         xlab = "Steps",
         ylab = "Frequency",
         main = "Steps per Day - with NA filled in",
         breaks = 15,
         col="red")
 
    mnPD2 <- round(mean(tnspd2$steps),digits = 2)
    print(paste("Mean fill in: ", mnPD))
    
    mdPD2 <- round(median(tnspd2$steps),digits = 2)
    print(paste("Median fill in: ", mdPD))
 
    #Many differences?
    
    print("The values: Difference By Mean")
    print(paste("Not NA fill:", mnPD))
    print(paste("With NA fill:", mnPD2))
    
    print("The values: Differences By Median")
    print(paste("Not NA fill:", mdPD))
    print(paste("With NA fill:", mdPD2))
    
    
##Are there differences in activity patterns between weekdays 
##and weekends?

    #Create a new factor variable in the dataset with two 
    #levels – “weekday” and “weekend” indicating whether a 
    #given date is a weekday or weekend day.
    
    arquivo3 <- arquivo2
    arquivo3$date <- as.Date(arquivo3$date)
    arquivo3$weekday <- weekdays(arquivo3$date)
    
    #Weekdays names from Portuguese [PT-BR] - sorry for that
    arquivo3$weekend <- ifelse(arquivo3$weekday=="sábado" | arquivo3$weekday=="domingo", "Weekend", "Weekday" )

    #Make a panel plot containing a time series plot 
    #(i.e. type = "l") of the 5-minute interval (x-axis) and 
    #the average number of steps taken, averaged across all 
    #weekday days or weekend days (y-axis).
    
    
    library(ggplot2)
    meandataweekendweekday <- aggregate(arquivo3$steps, 
                                        by= list(arquivo3$weekend, arquivo3$interval),
                                        na.omit(mean))
    names(meandataweekendweekday) <- c("weekend", "interval", "steps")

    ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line() + facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") + ggtitle("Comparison of Average Number of Steps in Each Interval")
    
    
    
    
    
    