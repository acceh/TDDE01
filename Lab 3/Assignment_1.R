setwd("~/TDDE01/TDDE01/Lab 3")
RNGversion('3.5.1')
set.seed(1234567890)

library("geosphere")

stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by = "station_number")



## These three values are up to the user
a <- 58.4274 # The lat of the point to predict
b <- 14.826 # The long of the point to predict
date <-"2013-11-04" # The date to predict (up to the students) 
##End input
times <- c("04:00:00", "06:00:00","06:00:00","08:00:00","10:00:00","12:00:00",
           "14:00:00","16:00:00","18:00:00","20:00:00", "22:00:00", "24:00:00")

p1 <-  c(st$latitude,st$longitude)
p2 <- c(a,b)


#Filters posterior date
filterPosteriorDate <- function(data,date) {
  return(data[!(as.Date(data$date)-as.Date(date))>=0,])
}

#Filters posterior time
filterPosteriorTime <- function(data, time, date) {
  return (data[!(as.Date(data$date) == as.Date(date) & 
                 as.numeric(difftime(strptime(data$time, format="%H:%M:%S"), 
                 strptime(time, format="%H:%M:%S"))) > 0 ),])
}

#Kernel for distance computing to point of interest
distCompute <- function(p1, p2) {
  
  return (distHaversine(p1,p2));
}

distCompute(p1,p2)

h_distance <- 
h_date <-
h_time <-

temp <- vector(length = length(times)) # Students’ code here
plot(temp, type = "o")