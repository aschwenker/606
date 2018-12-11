library(tabulizer)
library(dplyr)
library(pdftables)
library(pdftools)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)

# Location of WARN notice pdf file
activities_path<-("Z:/GIS/Development/GPS_data/date_event.csv")
activities_counts_path<-("Z:/GIS/Development/GPS_data/Event_Counts.csv")
work_order <- 'C:/Users/aschwenker/Desktop/WorkOrders/Case_00272586_5161105428_Reinstalled.pdf'
WORD_ORDER_TRACKER_PATH<-'C:/Users/aschwenker/Desktop/WorkOrders/TTNM_WORK_ORDER_TRACKER.CSV'
work_orders<-read.csv(WORD_ORDER_TRACKER_PATH)
activities<-read.csv(activities_counts_path)
activities$EventSubTypeID<-paste0("X",activities$EventSubTypeID)
activities$Date_Txt<- as.character(activities$ActivityDateTimeEST)
activities<-separate(data = activities, col = Date_Txt, into = c("date", "time"), sep = " ")
head(activities)
activities$date<-as.Date(activities$date)
head(activities)

less<-c("date","EventSubTypeID","X")
activities_less<-activities[less]
activities_less$Y<-1
head(activities_less)
activities_less<-activities_less%>%group_by(date,EventSubTypeID)%>%tally(X)
activities_less
activities_less_spread<-activities_less%>%spread(EventSubTypeID,n,fill=0)
head(activities_less_spread)
closed_work_orders<-subset(work_orders,Status=='Closed')
closed_work_orders
simplified_fields<-c("Work.Order.Type","Last.Modified.Date")
closed_work_orders<-closed_work_orders[simplified_fields]
closed_work_orders$Last.Modified.Date<-as.character(closed_work_orders$Last.Modified.Date)
closed_work_orders
grouped_work_orders<-closed_work_orders%>%group_by(Last.Modified.Date,Work.Order.Type)%>%count(Work.Order.Type)
grouped_work_orders
write.csv(grouped_work_orders,"Z:/GIS/Development/GPS_data/grouped_work_orders.csv")
#closed_work_orders$Y<-1
#closed_work_orders<-closed_work_orders%>% distinct
closed_work_orders_spread<-grouped_work_orders%>%spread(Work.Order.Type,n,fill=0)
#closed_work_orders_spread$Last.Modified.Date<-as.character(closed_work_orders_spread$Last.Modified.Date)
closed_work_orders_spread$date<-as.Date(closed_work_orders_spread$Last.Modified.Date, format = "%m/%d/%Y")
drops <- c("Last.Modified.Date","date_txt")
closed_work_orders_spread<-closed_work_orders_spread[ , !(names(closed_work_orders_spread) %in% drops)]
closed_work_orders_spread

(ALL <- inner_join(activities_less_spread, closed_work_orders_spread))
colnames(ALL)
nrow(ALL$x175)
names(ALL)
colnames(ALL)[colnames(ALL)=="Re-Installation"] <- "Re_Int"
colnames(ALL)[colnames(ALL)=="De-Installaton"] <- "De_Int"

summary(m1 <- glm(X175~De_Int , family="poisson", data=ALL))


re_175_table<-table(ALL$`Re-Installation`,ALL$X175)
re_175_table
chisq.test(re_175_table, correct=FALSE)
fisher.test(re_175_table)