library(dplyr)
library(ggplot2)

if (!file.exists("activity.zip")) 
  {
  #File URL and name
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  Name <- "activity.zip"
  
  # download the file & note the time
  download.file(fileUrl, Name)
  dateDownloaded <- date()
  unzip(zipfile = "activity.zip")
}

Data_Inp<-read.csv("activity.csv",header =T )
Data_Inp<-tbl_df(Data_Inp)


Data_Filt<-Data_Inp[which(!is.na(Data_Inp$steps)),]
MissingValues <- !complete.cases(Data_Inp)
#Data_Filt$date<-strptime(Data_Filt$date, "%Y-%m-%d")
total_steps <- Data_Filt %>% group_by(date) %>% summarize(sum = sum(steps))
hist(x = total_steps$sum, xlab = "Total Steps", ylab = "Days", main = "Total Steps per Day")

mean_steps <-mean(total_steps$sum)
median_steps <-median(total_steps$sum)

Avg_step <- aggregate(Data_Filt$steps,list(interval=Data_Filt$interval),mean)
plot<-ggplot(data = Avg_step, aes(x = interval, y = x)) + geom_line() 
plot<-plot+xlab("5-minute interval")+ylab("average number of steps taken")+ggtitle("Steps Taken in 5-Minute Intervals Averaged Across All Days")
print(plot)

Avg_step[which.max(Avg_step$steps),]
