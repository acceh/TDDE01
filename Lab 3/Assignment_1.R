setwd("~/TDDE01/TDDE01/Lab 3")
RNGversion('3.5.1')
set.seed(1234567890)

library("geosphere")

stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by = "station_number")

## These values are up to the user. ud = user defined
ud.lat <- 58.4274 # The lat of the point to predict
ud.long <- 14.826 # The long of the point to predict
ud.date <-"2013-11-04" # The date to predict (up to the students) 
ud.h_distance <- 100000
ud.h_date <- 10
ud.h_time <- 4

##End input

ud.times <- c("04:00:00", "06:00:00","06:00:00","08:00:00","10:00:00","12:00:00",
           "14:00:00","16:00:00","18:00:00","20:00:00", "22:00:00", "24:00:00")

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
distGaussian <- function(data, target, h) {
  return (exp(-(distHaversine(data.frame(data$longitude,data$latitude), target)/h)**2));
}

#Kenrel for date
dateGaussian <- function(p1, p2, h) {
  return(exp(-(as.numeric(as.numeric(as.Date(p1))-as.numeric(as.Date(p2)))/h)**2))
}


timeGaussian <- function(p1,p2,h) {
  return(exp(-(as.numeric(difftime(strptime(p1, format="%H:%M:%S"), strptime(p2, format="%H:%M:%S")))/h/3600)**2))
}


  
tempEst <- function(data, ud) {
  
  filtered_data <- filterPosteriorDate(data, ud.date)
  distance <- distGaussian(filtered_data, c(ud.lat,ud.long), ud.h_distance)
  day <- dateGaussian(filtered_data$date, ud.date, ud.h_date)
 
  length=length(ud.times)  
  t_sum <- vector(length=length)
  t_multi <- vector(length=length)
  
  for  (i in length) {
    filtered_data_by_time <- filterPosteriorTime(filtered_data, ud.date, ud.times[i])
    time <- timeGaussian(filtered_data_by_time$time, ud.times[i], ud.h_time)
    kernel_sum <- distance + day + time
    kernel_multi <- distance * day * time
    t_sum[i] <- sum(kernel_sum %*% filtered_data_by_time$air_temperature)/sum(kernel_sum)
    t_multi[i] <- sum(kernel_sum %*% filtered_data_by_time$air_temperature)/sum(kernel_multi)

  }
  
  return(list(t_sum=t_sum, t_multi=t_multi))
}
  


temps <- tempEst(st, ud)

plot(temps$t_sum, xaxt = "n", xlab="Time", ylab="Temperature", type="o", main = "Sum of kernels")
axis(1, at=1:length(ud.times), labels=ud.times)

temp <- vector(length = length(times)) # Students code here
plot(temp, type = "o")
