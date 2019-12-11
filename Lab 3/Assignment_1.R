setwd("~/Programming/TDDE01/Lab 3")
set.seed(1234567890)

library("geosphere")

stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by = "station_number")
# These three values are up to the students
a <- 58.4274 # The point to predict (up to the students) 
b <- 14.826
p1 <-  c(st$latitude,st$longitude)
p2 <- c(a,b)

distCompute <- function(p1, p2) {
  
  return (distHaversine(p1,p2));
}

distCompute(p1,p2)

h_distance <- 
h_date <-
h_time <-

date <-"2013-11-04" # The date to predict (up to the students) times <- c("04:00:00", "06:00:00", ..., "24:00:00")
temp <- vector(length = length(times)) # Students’ code here
plot(temp, type = "o")