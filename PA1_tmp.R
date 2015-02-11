
setwd("/Users/Non-corrupt user/Desktop/Suzy/DataScience/ReproducibleResearch/RepData_PeerAssessment1")


#unzip files - file in forked repo
unzip("activity.zip" )
dateDownloades<-date()

#read in files
data <- read.csv("activity.csv",header=TRUE)
str(data)

#calc mean number of steps taken per day
total_steps <- tapply(data$steps,data$date,sum)


#create histogram of total number of steps taken each day
hist(total_steps, main="Total Steps", xlab="Number of Steps")

#mean and median of total steps 
mean_steps<- mean(total_steps,na.rm=TRUE)
median_steps<- median(total_steps,na.rm=TRUE)

#plot avg number of steps taken across all days
library(dplyr)
steps_by_interval <- data %>%
                     group_by(interval) %>%
                     summarize(avg_steps=mean(steps, na.rm=TRUE))%>%
                     arrange(interval)
steps_by_interval
              
plot(steps_by_interval, type="l",
     ylab="Average Steps per Interval",
     xlab="Interval",
     main="Average Number of Steps Taken per Interval")

#interval with max number of steps
max_steps <-max(steps_by_interval$avg_steps)     
max_int <-steps_by_interval[which(steps_by_interval$avg_steps == max_steps),1]   

#calc number of missing values in dataset
sum(is.na(data$steps))
mean(is.na(data$steps))

sum(is.na(data$date))
sum(is.na(data$interval))

#imput missing values
#using median of avg 5 minute interval
na <- data[is.na(data$steps),]   #missing steps
#merging using datatable method - faster
library(data.table)

na_DT <- data.table(na)
steps_by_interval_DT <- data.table(steps_by_interval)
setkey(na_DT,interval)
setkey(steps_by_interval_DT,interval)
data_miss_steps <-merge(na_DT,steps_by_interval_DT)
data_miss_steps[,steps:=NULL] #dropping steps column - all are NA
setnames(data_miss_steps,"avg_steps","steps") #renaming steps col
#head(data_miss_steps)

#delete records with NA steps from data then append imputed data on
data_noNA <- data[!is.na(data$steps),]
data_wimput <- rbind(data_noNA,data_miss_steps) #dataset to work with 

#create histogram of total number of steps taken each day - new dataset
total_steps_imput <- tapply(data_wimput$steps,data_wimput$date,sum)
hist(total_steps_imput, main="Total Steps \n Using Imputted Dataset", xlab="Number of Steps")

#mean and median of total steps - imputted dataset
mean_steps_imput<- mean(total_steps_imput,na.rm=TRUE)
median_steps_imput<- median(total_steps_imput,na.rm=TRUE)

#looking at weekdays vs. weekends
#use data_wimput
###################################################0
#change date to weekday
#step by step method
data_wimput$weekday <-weekdays(as.Date(data_wimput$date,"%Y-%m-%d"))
data_wimput$weekpart <-factor(data_wimput$weekday)
#add levels
levels(data_wimput$weekpart) <- list(
        weekday = c("Monday",
                    "Tuesday",
                    "Wednesday",
                    "Thursday",
                    "Friday"),
        weekend = c("Saturday",
                    "Sunday")
        )
table(data_wimput$weekday, data_wimput$weekpart)

#condensed - same as above - taking date column, making it a date, taking the weekdays()
# putting, as a factor into weekpart
# then using levels() to assign it to either weekday or weekend
data_wimput$weekpart <-factor(weekdays(as.Date(data_wimput$date,"%Y-%m-%d")))
#add levels
levels(data_wimput$weekpart) <- list(
    weekday = c("Monday",
                "Tuesday",
                "Wednesday",
                "Thursday",
                "Friday"),
    weekend = c("Saturday",
                "Sunday")
)
table(data_wimput$weekpart) #verifing same result as above

#head(data_wimput)
#str(data_wimput)

#plot avg number of steps taken across all days by weekday vs. weekend
steps_by_interval_weekend <- data_wimput %>%
                           group_by(weekpart,interval) %>%
                           summarize(avg_steps=mean(steps, na.rm=TRUE))%>%
                           arrange(weekpart,interval)
steps_by_interval_weekend


library(ggplot2)
qplot(interval,avg_steps, data=steps_by_interval_weekend, 
      geom=c("line"),type="l",facets=weekpart~.) + 
      ylab("Average Number of Steps") +
      theme_set(theme_gray(base_size = 15)) +
      ggtitle("Number of Steps per Interval Split \n  Weekday vs. Weekend") 

weekend_weekday <- data_wimput %>%
    group_by(weekpart) %>%
    summarize(avg_steps=mean(steps, na.rm=TRUE),
              median=median(steps,na.rm=TRUE))%>%
    arrange(weekpart)
weekend_weekday
weekday_mean <- weekend_weekday[avg_steps
