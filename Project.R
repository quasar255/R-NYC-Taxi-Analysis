install.packages("lubridate")
library(lubridate)

install.packages("tidyverse")
library(tidyverse)

green0115 <- read_csv("/Users/MGrewal/Documents/MBA/OM 620 Business Analytics/Data/green_tripdata_2015-01.csv")
weather0115 <- read_csv("/Users/MGrewal/Documents/MBA/OM 620 Business Analytics/Data/1051606.csv")
green0115_1 <- green0115 %>% mutate(hpick = lubridate::hour(lpep_pickup_datetime), mpick = lubridate::minute(lpep_pickup_datetime),spick = lubridate::second(lpep_pickup_datetime), wdaypick = lubridate::wday(lpep_pickup_datetime, label = T))
green0115_2 <- green0115_1 %>% mutate(hdrop = lubridate::hour(Lpep_dropoff_datetime), mdrop = lubridate::minute(Lpep_dropoff_datetime),sdrop = lubridate::second(Lpep_dropoff_datetime), wdaydrop = lubridate::wday(Lpep_dropoff_datetime, label = T))

#### Zones ####
install.packages("rgdal")
library(rgdal)
shape <- readOGR(dsn = ".")

df <- green0115_2 %>% 
  rename(longitude = Pickup_longitude, 
         latitude = Pickup_latitude) %>% 
  select(longitude, latitude)
coordinates(df) <- ~ longitude + latitude
proj4string(df) <- CRS("+proj=longlat")
df <- spTransform(df, proj4string(shape))
proj4string(df) <- proj4string(shape)

pickup_df <- over(df, shape)

df <- green0115_2 %>% 
  rename(longitude = Dropoff_longitude, 
         latitude = Dropoff_latitude) %>% 
  select(longitude, latitude)
coordinates(df) <- ~ longitude + latitude
proj4string(df) <- CRS("+proj=longlat")
df <- spTransform(df, proj4string(shape))
proj4string(df) <- proj4string(shape)

dropoff_df <- over(df, shape)
#######
green0115_3 <- green0115_2 %>% 
  mutate(pickup_zone = pickup_df$zone, 
         pickup_borough = pickup_df$borough,
         dropoff_zone = dropoff_df$zone, 
         dropoff_borough = dropoff_df$borough)

# 1. Based on Psg count in car, do people tend to tip more?
datax <- green0115_3 %>% filter(Payment_type ==1)  %>% group_by(Passenger_count) %>% summarise(avgtip = mean(Tip_amount, na.rm=TRUE), n=n()) %>% arrange(desc(avgtip)) 
datax
ggplot(data=datax) + geom_point(mapping = aes(x=Passenger_count, y=avgtip, color = Passenger_count))

# 2. Based on dropoff/pickup zone, do people tend to tip more?
datax1 <- green0115_3 %>% filter(Payment_type ==1)  %>% group_by(dropoff_borough) %>% summarise(avgtip = mean(Tip_amount, na.rm=TRUE), avgfare = mean(Fare_amount, na.rm=TRUE), n=n(), percent=(avgtip/avgfare)*100) %>% arrange(desc(avgtip)) 
datax1
write.csv(datax1, file = "PsgCountVSBoroughVSTips.csv")
ggplot(data=datax1) + geom_point(mapping = aes(x=dropoff_borough, y=avgtip, color = percent, size = n))

# 3. How does pickup volume varies over different times of the day for each group?
datax2 <- green0115_3  %>%  filter( hpick > 5 & hpick <10) %>% group_by(pickup_borough) %>% summarise( morning =n()) 
datax3 <-green0115_3  %>%  filter( hpick > 10 & hpick <= 15) %>% group_by(pickup_borough) %>% summarise( morningnoon=n())
set3 <- merge(datax2,datax3, by = "pickup_borough")
datax4 <-green0115_3  %>%  filter( hpick > 15 & hpick <= 19) %>% group_by(pickup_borough) %>% summarise( afternooneve=n()) 
set4 <- merge(set3,datax4, by = "pickup_borough")
datax5 <-green0115_3  %>%  filter( hpick > 19 & hpick <= 22) %>% group_by(pickup_borough) %>% summarise( eveningnight=n())
set5 <- merge(set4,datax5, by = "pickup_borough")
datax6 <-green0115_3  %>%  filter(hpick >22) %>% group_by(pickup_borough) %>% summarise( night1=n())
set6 <- merge(set5,datax6, by = "pickup_borough", all=TRUE)
datax7 <-green0115_3  %>%  filter(hpick <= 5) %>% group_by(pickup_borough) %>% summarise( night2=n())
set7 <- merge(set6,datax7, by = "pickup_borough")
set7
write.csv(set7, file = "DayofWeekVSVolume.csv")

#######Based on day of week, avg length and bucks made ..
green0115_3 <- subset(green0115_3, Total_amount < 2000 & Total_amount > 0)
data_allweekday <- green0115_3  %>% group_by(wdaypick) %>% summarise(avgfare = mean(Total_amount, na.rm=TRUE), AvgTripLength=mean(Trip_distance, na.rm=TRUE), Trips=n()) %>% arrange(desc(avgfare)) 
DayofWeekVSTotalFAre_OutliersRemoved1 <- ggplot(data=data_allweekday) + geom_point(mapping = aes(x=wdaypick, y=avgfare, color = AvgTripLength, size = Trips)) + labs(title = "Day of the Week Vs. Total Charge per Trip", y = "Average Total Fare($) per Trip", x= "Day of the Week")
ggsave("DayofWeekVSTotalFAre_OutliersRemoved1.jpeg")

data_sun <- green0115_3  %>% filter(wdaypick == "Sun") %>% group_by(hpick) %>% summarise(avgfare = mean(Total_amount, na.rm=TRUE), avglen=mean(Trip_distance, na.rm=TRUE), n=n()) %>% arrange(desc(avgfare)) 
data_sun
SunFareVshr <- ggplot(data=data_sun) + geom_point(mapping = aes(x=hpick, y=avgfare, color = avglen, size = n)) + labs(title = "Sunday: Time of day VS Total Charge VS Trip Length VS Number of Trips")
ggsave("SunFareVshr.jpeg")

data_mon <- green0115_3  %>% filter(wdaypick == "Mon") %>% group_by(hpick) %>% summarise(avgfare = mean(Total_amount, na.rm=TRUE), avglen=mean(Trip_distance, na.rm=TRUE), n=n()) %>% arrange(desc(avgfare)) 
data_mon
MonFareVshr <- ggplot(data=data_mon) + geom_point(mapping = aes(x=hpick, y=avgfare, color = avglen, size = n)) + labs(title = "Monday: Time of day VS Total Charge VS Trip Length VS Number of Trips")
ggsave("MonFareVshr.jpeg")

data_Tues <- green0115_3  %>% filter(wdaypick == "Tues") %>% group_by(hpick) %>% summarise(avgfare = mean(Total_amount, na.rm=TRUE), AvgTripLength=mean(Trip_distance, na.rm=TRUE), Trips=n()) %>% arrange(desc(avgfare)) 
data_Tues
TueFareVshr <- ggplot(data=data_Tues) + geom_point(mapping = aes(x=hpick, y=avgfare, color = AvgTripLength, size = Trips))+ labs(title = "Tuesday: Time of the Day Vs. Total Charge per Trip", y = "Average Total Fare($) per Trip", x= "Time of the Day")
ggsave("TueFareVshr.jpeg")

data_Wed <- green0115_3  %>% filter(wdaypick == "Wed") %>% group_by(hpick) %>% summarise(avgfare = mean(Total_amount, na.rm=TRUE), avglen=mean(Trip_distance, na.rm=TRUE), n=n()) %>% arrange(desc(avgfare)) 
data_Wed
WedFareVshr <- ggplot(data=data_Wed) + geom_point(mapping = aes(x=hpick, y=avgfare, color = avglen, size = n)) + labs(title = "Wednesay: Time of day VS Total Charge VS Trip Length VS Number of Trips")
ggsave("WedFareVshr.jpeg")

data_Thurs <- green0115_3  %>% filter(wdaypick == "Thurs") %>% group_by(hpick) %>% summarise(avgfare = mean(Total_amount, na.rm=TRUE), avglen=mean(Trip_distance, na.rm=TRUE), n=n()) %>% arrange(desc(avgfare)) 
data_Thurs
ThursFareVshr <- ggplot(data=data_Thurs) + geom_point(mapping = aes(x=hpick, y=avgfare, color = avglen, size = n)) + labs(title = "Thursday: Time of day VS Total Charge VS Trip Length VS Number of Trips")
ggsave("ThursFareVshr.jpeg")

data_Fri <- green0115_3  %>% filter(wdaypick == "Fri") %>% group_by(hpick) %>% summarise(avgfare = mean(Total_amount, na.rm=TRUE), avglen=mean(Trip_distance, na.rm=TRUE), n=n()) %>% arrange(desc(avgfare)) 
data_Fri
FriFareVshr <- ggplot(data=data_Fri) + geom_point(mapping = aes(x=hpick, y=avgfare, color = avglen, size = n)) + labs(title = "Friday: Time of day VS Total Charge VS Trip Length VS Number of Trips")
ggsave("FriFareVshr.jpeg")

data_Sat <- green0115_3  %>% filter(wdaypick == "Sat") %>% group_by(hpick) %>% summarise(avgfare = mean(Total_amount, na.rm=TRUE), avglen=mean(Trip_distance, na.rm=TRUE), n=n()) %>% arrange(desc(avgfare)) 
data_Sat
SatFareVshr <- ggplot(data=data_Sat) + geom_point(mapping = aes(x=hpick, y=avgfare, color = avglen, size = n)) + labs(title = "Saturday: Time of day VS Total Charge VS Trip Length VS Number of Trips")
ggsave("SatFareVshr.jpeg")

JoinSunMon <- full_join(data_sun, data_mon, by = "hpick", suffix = c(".sun", ".mon"))
JoinSunTue <- full_join(JoinSunMon,data_Tues, by = "hpick", suffix = c( "", ".Tue"))
JoinSunWed <- full_join(JoinSunTue,data_Wed, by = "hpick", suffix = c( "", ".Wed"))
JoinSunThu <- full_join(JoinSunWed,data_Thurs, by = "hpick", suffix = c( "", ".Thurs"))
JoinSunFri <- full_join(JoinSunThu,data_Fri, by = "hpick", suffix = c( "", ".Fri"))
JoinSunSat <- full_join(JoinSunFri,data_Sat, by = "hpick", suffix = c( "", ".Sat"))

##
shortdatacharge <- green0115_3 %>% select(Total_amount, hpick,  wdaypick ) %>% group_by(hpick, wdaypick) %>% summarise(avgfare=mean(Total_amount, na.rm=TRUE), Trips=n(), Revenue_Generated = (avgfare * Trips))
ggplot() + geom_line(data = shortdatacharge, mapping = aes(x=hpick, y=avgfare, color=wdaypick)) + labs(title = "Pick-up Hour Vs. Average Total Charge($) per Trip", x = "Time of the Day", y= "Average Total Charge($) per Trip")+labs(caption = "blahsvsjvj")
ggsave("TimeofDayVsTotalCharge.jpeg")

shortdataRevenue <- green0115_3 %>% select(Total_amount, hpick,  wdaypick ) %>% group_by(hpick, wdaypick) %>% summarise(fare=sum(Total_amount, na.rm=TRUE), Trips=n())
ggplot() + geom_line(data = shortdatacharge, mapping = aes(x=hpick, y=Revenue_Generated, color=wdaypick)) + labs(title = "Pick-up Hour Vs. Total Revenue Generated by all Trips", x = "Time of the Day", y= "Total Revenue Generated($)", fill = "Day")
ggsave("TimeofDayVsTotalRevenue.jpeg")

shortdatatrips <- green0115_3 %>% select( hpick,  wdaypick ) %>% group_by(hpick, wdaypick) %>% summarise(Trips=n())
ggplot() + geom_line(data = shortdatatrips, mapping = aes(x=hpick, y=Trips, color=wdaypick)) + labs(title = "Pick-up Hour Vs. Trips", x = "Time of the Day", y= "Trips", fill = "Day")
ggsave("TimeofDayVsTrips.jpeg")

shortdataarea <- green0115_3 %>% filter(wdaypick == "Thurs") %>% group_by(pickup_borough) %>% summarise(avgfare1=mean(Total_amount,na.rm=TRUE), trips = n())
p<-ggplot(shortdataarea, aes(pickup_borough, avgfare1))
p+geom_bar(stat="identity", aes(fill=trips))+
  labs(title = "Thursdays: Pick-up Borough Vs. Average Total Charge Vs. Number of Trips", x = "Borough", y= "Average Total Charge($) per Trip") 
ggsave("ThursdayBoroughVsTrips.jpeg")

###
green0115_4 <- green0115_3 %>% mutate(datepick = lubridate::day(lpep_pickup_datetime))
data_byzone <- green0115_4 %>% filter(pickup_zone != "NA") %>% group_by(pickup_borough, pickup_zone, na.rm=TRUE) %>% summarise(avgfare = mean(Total_amount, na.rm=TRUE), avglen=mean(Trip_distance, na.rm=TRUE), Trips=n())  %>% filter(Trips >= 50) %>% arrange(desc(pickup_zone,na.rm=TRUE)) %>% arrange(desc(avgfare,na.rm=TRUE))
topdata_byzone <- head(data_byzone, 15)
zoneplot <- ggplot(data=topdata_byzone) + geom_point(mapping = aes(x=pickup_zone, y=avgfare, color=pickup_borough, size=Trips)) + labs(title = "Top 15 Pick-up Zones by Average Total Fare", subtitle = "Top 15 Zones by Average Total Fare" , x= "Pick-up Zone", y="Average Total Fare($)", caption  = "We even have a caption. A very long one indeed.")
zoneplot + theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(size = 12, colour = "black", vjust = -1))
ggsave("zoneplot.jpeg")
