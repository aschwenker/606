library(tabulizer)
library(dplyr)
library(pdftables)
library(pdftools)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)
library(ResourceSelection)
library(DATA606)
library(e1071)

activities_path<-("Z:/GIS/Development/GPS_data/date_event.csv")
activities_counts_path<-("Z:/GIS/Development/GPS_data/Event_Counts.csv")
work_order <- 'C:/Users/aschwenker/Desktop/WorkOrders/Case_00272586_5161105428_Reinstalled.pdf'
WORD_ORDER_TRACKER_PATH<-'C:/Users/aschwenker/Desktop/WorkOrders/TTNM_WORK_ORDER_TRACKER.CSV'
work_orders<-read.csv(WORD_ORDER_TRACKER_PATH)
activities<-read.csv(activities_counts_path)
activities$EventSubTypeID<-paste0("X",activities$EventSubTypeID)
activities$Date_Txt<- as.character(activities$ActivityDateTimeEST)
activities<-separate(data = activities, col = Date_Txt, into = c("date", "time"), sep = " ")
activities$date<-as.Date(activities$date)
less<-c("date","EventSubTypeID","X")
activities_less<-activities[less]
activities_less$Y<-1
activities_less<-activities_less%>%group_by(date,EventSubTypeID)%>%tally(X)
activities_less_spread<-activities_less%>%spread(EventSubTypeID,n,fill=0)
closed_work_orders<-subset(work_orders,Status=='Closed')
simplified_fields<-c("Work.Order.Type","Last.Modified.Date")
closed_work_orders<-closed_work_orders[simplified_fields]
closed_work_orders$Last.Modified.Date<-as.character(closed_work_orders$Last.Modified.Date)
grouped_work_orders<-closed_work_orders%>%group_by(Last.Modified.Date,Work.Order.Type)%>%count(Work.Order.Type)
#closed_work_orders$Y<-1
#closed_work_orders<-closed_work_orders%>% distinct
closed_work_orders_spread<-grouped_work_orders%>%spread(Work.Order.Type,n,fill=0)
#closed_work_orders_spread$Last.Modified.Date<-as.character(closed_work_orders_spread$Last.Modified.Date)
closed_work_orders_spread$date<-as.Date(closed_work_orders_spread$Last.Modified.Date, format = "%m/%d/%Y")
drops <- c("Last.Modified.Date","date_txt")
closed_work_orders_spread<-closed_work_orders_spread[ , !(names(closed_work_orders_spread) %in% drops)]
(ALL <- inner_join(activities_less_spread, closed_work_orders_spread))
colnames(ALL)

colnames(ALL)[colnames(ALL)=="Re-Installation"] <- "Re_Int"
colnames(ALL)[colnames(ALL)=="De-Installaton"] <- "De_Int"
nrow(ALL)



#http://r-statistics.co/Linear-Regression.html
scatter.smooth(x=ALL$Re_Int, y=ALL$X175, main="Event 175 ~ Re-Installation")  # scatterplot

scatter.smooth(x=ALL$De_Int, y=ALL$X5, main="Event 106 ~ De-Installation")  # scatterplot



scatter.smooth(x=ALL$Re_Int, y=ALL$X15, main="Event 15 ~ Re-Installation")  # scatterplot

par(mfrow=c(1,2))  # divide graph area in 2 columns
plot(density(ALL$X15), main="Density Plot: Event 15", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(ALL$X175), 2)))  # density plot for 'speed'
polygon(density(ALL$X15), col="blue")
plot(density(ALL$Re_Int), main="Density Plot: Re-installations", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(ALL$Re_Int), 2)))  # density plot for 'dist'
polygon(density(ALL$Re_Int), col="blue")
cor(ALL$X15, ALL$Re_Int)  # calculate correlation between 15 and re-installations 
m1 <- lm(X106~ De_Int, data = ALL)
print(m1)
summary(m1)
plot(ALL$X106~ALL$De_Int)
abline(m1)

scatter.smooth(x=ALL$De_Int, y=ALL$X106, main="Event 106 ~ De-Installation")  # scatterplot
par(mfrow=c(1,2 ))  # divide graph area in 2 columns
plot(density(ALL$X106), main="Density Plot: Event 106", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(ALL$X175), 2)))  # density plot for 'speed'
polygon(density(ALL$X106), col="red")
plot(density(ALL$De_Int), main="Density Plot: De-installations", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(ALL$Re_Int), 2)))  # density plot for 'dist'
polygon(density(ALL$De_Int), col="red")
cor(ALL$X106, ALL$De_Int)  # calculate correlation between 106 and re-installations 


