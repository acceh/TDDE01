setwd("~/Programming/TDDE01/Lab 3")
RNGversion('3.5.1')
set.seed(1234567890)

library("geosphere")

stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by = "station_number")

## These values are up to the user. ud = user defined
ud.lat <- 59.325 # The lat of the point to predict
ud.long <- 18.071 # The long of the point to predict
ud.date <-"2016-12-24" # The date to predict (up to the students) 
ud.h_distance <- 100000
ud.h_date <- 30
ud.h_time <- 4
##End input

ud.times <- c("04:00:00", "06:00:00","08:00:00","10:00:00","12:00:00","14:00:00",
           "16:00:00","18:00:00","20:00:00","22:00:00", "24:00:00")

#Filters posterior date
filterPosteriorDate <- function(data,date) {
  return(data[!(as.Date(data$date)-as.Date(date))>0,])
}

#Filters posterior time
filterPosteriorTime <- function(data, date, time) {
  return (data[!(as.Date(data$date) == as.Date(date) & 
                 as.numeric(difftime(strptime(data$time, format="%H:%M:%S"), 
                 strptime(time, format="%H:%M:%S"))) > 0 ),])
}



#Kernel for distance computing to point of interest
distGaussian <- function(data, target, h) {
  dist <- distHaversine(data.frame(data$longitude,data$latitude), target)
  u <- dist/h
  return (exp(-(u)^2));
}

#Kenrel for date
dateGaussian <- function(data, target, h) {
  date_diff <- as.numeric(as.Date(data$date)-as.Date(target), unit="days")
  u <- date_diff/h
  return(exp(-(u)^2))
}

#Kernel for time
timeGaussian <- function(data,target,h) {
  time_difference <- difftime(strptime(data$time, format="%H:%M:%S"), 
                              strptime(target, format="%H:%M:%S"))
  u <- as.numeric(time_difference)/h
  return(exp(-(u)^2))
}

filtered_data <- filterPosteriorDate(st, ud.date)
length = length(ud.times)
t_sum <- vector(length = length)
t_multi <- vector(length = length)
for (i in 1:length) {
  print(i)
  filtered_data_by_time <-
    filterPosteriorTime(filtered_data, ud.date, ud.times[i])
  time <-
    timeGaussian(filtered_data_by_time, ud.times[i], ud.h_time)
  distance <-
    distGaussian(filtered_data_by_time,
                 data.frame(ud.long, ud.lat),
                 ud.h_distance)
  day <- dateGaussian(filtered_data_by_time, ud.date, ud.h_date)
  kernel_sum <- distance + day + time
  kernel_multi <- distance * day * time
  t_sum[i] <-
    sum(kernel_sum %*% filtered_data_by_time$air_temperature) / sum(kernel_sum)
  t_multi[i] <-
    sum(kernel_multi %*% filtered_data_by_time$air_temperature) / sum(kernel_multi)
}
plot(1:length(distance), distance, main = "Distance")
plot(1:length(day), day, main = "Day")
plot(1:length(time), distance, main = "Time")


temps <- list(t_sum=t_sum, t_multi=t_multi)

plot(temps$t_sum,xaxt='n', xlab="Time", 
     ylab="Temperature", type="o", main = "Sum of kernels")
axis(1, at=1:length(ud.times), labels=ud.times)

plot(temps$t_multi,xaxt='n', xlab="Time", 
     ylab="Temperature", type="o", main = "Multiplication of kernels")
axis(1, at=1:length(ud.times), labels=ud.times)
