set.seed(1234567890)
library(geosphere) #distHaversine-function
library(ggplot2)

stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by="station_number")

################# FUNCTIONS ################# 

#Filter dates
filterDate <- function(data, filterDate) {
  return (data[!as.Date(data$date) > as.Date(filterDate),])
}

#Filter time
filterTime <- function(data, filterDate, filterTime) {
  return = (data[!(as.Date(k$data.date) == as.Date(filterDate) &
                     strptime(k$data.time, format = "%H:%M:%S") >= strptime(filterTime, format = "%H:%M:%S")),])
}

gaussianKernel <- function(X_Xn, h_value){
  u_value <- X_Xn / h_value
  return(exp(-u_value^2))
}

time.difference <- function(timeVector, timeTemp){
  diff.time = difftime(strptime(timeVector, format = "%H:%M:%S"), 
                       strptime(timeTemp, format = "%H:%M:%S"))
  diff.time = as.numeric(diff.time/(60))
  diff.time = ifelse(diff.time < -12, 24 + diff.time, -diff.time)
  return(diff.time)
}

date.difference <- function(dateVector, dateTemp){
  diff.date = as.numeric(as.Date(dateVector) - as.Date(dateTemp), unit="days")
  diff.date = diff.date %% 365.25
  diff.date = ifelse(diff.date > 182, 365-diff.date, diff.date)
  return(diff.date)
}

distance.difference <- function(distanceVector, distanceTemp){
  
}

################# INPUT ################# 

# Smoothing coeffiecient for the Gaussian kernels
h.distance <- 100000
h.date <- 25
h.time <- 3

# The point to predict
latitude <- 58.4274 
longitude <- 14.826
point_to_predict = c(longitude, latitude)

# The date to predict
date <- "2013-07-04" 


times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", 
           "12:00:00", "14:00:00", "16:00:00", "18:00:00", 
           "20:00:00", "22:00:00", "24:00:00")

temperature <- vector(length=length(times))


################# CODE ################# 

# Filter data
data.filtered = filterDate(st, date)

data.date = data.filtered$date
data.time = data.filtered$time
data.station = data.filtered$station_number
data.temperature = data.filtered$air_temperature


# Kernel distance
diff.distance = distHaversine(data.frame(data.filtered$longitude,data.filtered$latitude), point_to_predict)
k.distance = gaussianKernel(diff.distance, h.distance)


# Kernel date
k.date = gaussianKernel(date.difference(data.filtered$date, date), h.date)

# K data frame
k.time = data.time
k = data.frame(data.date, data.time, data.station, k.distance, k.date, k.time, data.temperature)

# Result vectors
temperature_sum = vector(length = length(times))
temperature_product = vector(length = length(times))


for (i in 1:length(times)){

  # Kernel time
  k$k.time = gaussianKernel(time.difference(data.filtered$time, times[i]), h.time)
  
  k$temp_sum = k$k.distance + k$k.date + k$k.time
  k$temp_product = k$k.distance * k$k.date * k$k.time
  
  k.filtered = filterTime(k, date, times[i])
  
  temperature_sum[i] = sum(k.filtered$temp_sum %*% k.filtered$data.temperature)/sum(k.filtered$temp_sum)
  temperature_product[i] = sum(k.filtered$temp_product %*% k.filtered$data.temperature)/sum(k.filtered$temp_product)
}

# Weight plots
## Comparison
plot.comparison <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + labs (x = "u", y = "k-value")
fun1 <- function(x) exp(-x^2)
plot.comparison = plot.comparison + stat_function(fun = fun1) + xlim(-5,5)

## Distance, Date and Time
plot.distance <- ggplot(k, aes(x = data.station, y = k.distance)) + geom_point() + labs(title = "Kernel-value for distance | Point of interest: Vadstena", x = "Station number", y = "k-value") 
plot.date <- ggplot(subset(k, as.Date(k$data.date) > as.Date("2012-01-01")), aes(x = data.date, y = k.date)) + geom_point() + labs(title = "Kernel-value for date | Date interest: 2013-07-04", subtitle = " for 2012-01-01 to 2013-07-04", x = "Date", y = "k-value")   
plot.time <- ggplot(k, aes(x = data.time, y = k.time)) + geom_point() + labs(title = "Kernel-value for time | Time of interest: 24:00", subtitle = " for 00:00 to 24:00", x = "Time of the day", y = "k-value")

################# RESULT ################# 
result = data.frame(times, temperature_sum, temperature_product)

plot.sum <- ggplot(result, aes(x = times, y = temperature_sum, group=1)) + geom_point() + geom_line() + labs(title = "Temperature by using the sum of gaussian kernels", x = "Time of the day", y = "Temperature")
plot.product <- ggplot(result, aes(x = times, y = temperature_product, group=1)) + geom_point() + geom_line() + labs(title = "Temperature by using the product of gaussian kernels", x = "Time of the day", y = "Temperature")

