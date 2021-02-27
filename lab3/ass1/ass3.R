setwd("~/Desktop/TDDE01/tdde01/lab3/ass1")
set.seed(1234567890) 
library(geosphere)

## We are not to use measurements from the same day at all basically (according to lab assistant)
################################################
###############   DISTANCE      ################
#  Distance between days takes into account only the days 
#(not considering the year). For instance, 9 dec of one year is 
#expected to be very much similar to 9 dec of the previous/next year. 
#So they would be equal
################################################

# H-values based on plot below
#########
# H-values (smoothing factors) are based on "educated guesses, e.g. we plot the kernel functions 
# for different H-values and use the H-values that gives us a good kernel 
#########
#The smoothing factors makes measurements count to a limit of ca. double the amount of the smoothing factor, e.g. 300000m for distance
h_distance <- 100000 
h_date <- 10
h_time <- 3
H_vals <- data.frame(h_distance, h_date, h_time)

#These values can be set to what ever (needs to be a location though)
# Location for the forecast -> Leksand
latitude <- 60.7256 
longitude <- 15.0167


# Target date # date <- "2013-11-04" # The date to predict (up to the students) 
date <- "2013-07-15"
targets <- data.frame(latitude, longitude, date)


#These files contain information about weather stations and temperature measurements in 
#the stations at different days and times. The data have been kindly provided by the 
#Swedish Meteorological and Hydrological Institute (SMHI).
stations = read.csv("stations.csv", fileEncoding="latin1")
temps = read.csv("temps50k.csv")
st = merge(stations,temps,by="station_number")

# times <- c("04:00:00", "06:00:00", ..., "24:00:00")
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "00:00:00")


# Plot k against distance to find good h-values, e.g these values are just references to see how far away a measurement counts based on H-value
distance <- seq(0,400000,1000) #(default is meters -> plotting 40km) e.g our parameters that are returned from distHaversine are returned in meter format
date_diff <- seq(0,30,1)
time_diff <- seq(0,12,1)


#######    Gaussian kernel: k(u) = exp(−∣∣u∣∣2) where ∣∣ ⋅ ∣∣ is the Euclidean norm. #####
###### distance in this function resembles the distance to the location we want to forecast
###### Plotting to decide reasonable h-values
# Plot weights of Distance kernel
plotKernalDistance <- function(distances, h) {
  u <- distances/h
  k <- 1/exp(u^2) #same as exp(-u^2) but more intuitive
  pdf('Distance_Kernel.pdf')
  plot(k, type="l", xlab = "Distance kilometers")
  
  dev.off()
}

# Plot weights of Date kernel
plotKernalDate <- function(date_diff, h) {
  u <- date_diff/h
  k <- 1/exp(u^2)
  pdf('Date_Kernel.pdf')
  plot(k, type="l", xlab = "Date")
  dev.off()
}

# Plot weights of Hour kernel
plotKernalHour <- function(time_diff, h) {
  u <- time_diff/h
  k <- 1/exp(u^2)
  pdf('Hour_Kernel.pdf')
  plot(k, type="l", xlab = "Hour")
  dev.off()
} 


plotKernalDistance(distance, H_vals$h_distance)
plotKernalDate(date_diff, H_vals$h_date)
plotKernalHour(time_diff, H_vals$h_time)

# Filter dates in data posterior to target date
filterDataByDate <- function(data, date) {
  #### Drop rows with date > selected date
  filtered <-data[!(as.Date(data$date) > as.Date(date)),]
  return (filtered)
}

# We don't really need to use this according to lab assistant but i'll leave it here for possible future usage
# filterDataByTime <- function(data, date, time) {
#   return(data[!(as.Date(data$date) == as.Date(date) &
#                   as.numeric(difftime(strptime(data$time, format = "%H:%M:%S"),
#                                       strptime(time, format = "%H:%M:%S"))) > 0),])
# }


#Filter out all the dates, time and locations that are relevant to our prediction of the temperature,
#ONLY USED FOR INTUITIVITY,e.g we can easier see if our predicted temperatres are reasonable
# filterDataByRelativeness <- function(data, date1, time, distance) {
#   data <- data[!(as.Date(data$date) > as.Date(date1)),] #Removing all measurements from posterior date
#   data <- data[!(distHaversine(data.frame(data$longitude, data$latitude), distance) > 120000),] #removing all measurements from more than 120km away
#   data <- data[!((as.numeric(as.Date(date1) - as.Date(data$date), unit="days") %% 365) > 12),] # Removing all measurements that differ more than 10days
#   data <- data[!((as.numeric(difftime(strptime(data$time, format = "%H:%M:%S"),
#                                  strptime(time, format = "%H:%M:%S")))/3600) > 4),] #remove all measurements that differ more than 6 hours posterior
#   data <- data[!((as.numeric(difftime(strptime(time, format = "%H:%M:%S"),
#                                  strptime(data$time, format = "%H:%M:%S")))/3600) > 4 ),]#remove all measurements that differ more than 6 hours prior
#   return (data)
#   
# }
#relevant_data are now basically the data that affect our prediction
#relevant_data = filterDataByRelativeness(st, date, "00:00:00", c(targets$longitude[1], targets$latitude[1]))

#Data that we are allowed to use for our prediction
FilteredData_date=filterDataByDate(st, date)

# Gaussian Kernel for distance
Kernel_distance <- function(data, target, h) {
  distanceDifference <- distHaversine(data.frame(data$longitude, data$latitude), target)
  u <- distanceDifference/h
  k <- 1/exp(u^2)
  return(k)
}

# Gaussian Kernel for day
Kernel_day <- function(data, date1, h) {
  dayDifference <-as.numeric(as.Date(data$date) - as.Date(date1), unit="days")
  dayDifference <- abs(dayDifference)
  u <- dayDifference/h
  k <- 1/exp(u^2)
  return(k)
}

# Gaussian Kernel for hours
Kernel_hours <- function(data, time, h) {
  
  timeDifference <- as.numeric(difftime(strptime(data$time , format = "%H:%M:%S"), 
                        strptime(time , format = "%H:%M:%S"))) 


  timeDifference = timeDifference/3600
  for(i in 1:length(timeDifference)){
    if (timeDifference[i]<(-12)){
      timeDifference[i]<-timeDifference[i] %%24
    }
    else if (timeDifference[i]>12){
      timeDifference[i]<-abs(timeDifference[i] - 24)
    }
  }

  u <- timeDifference/h
  k <- 1/exp(u^2)
  return(k)
}


# Multiplication and Summation of kernels 
Est_temp <- function(data, target, smoothH, times) {
  #Basically a vector of values between 0-1 depending on the location of the data we are comparing to. The closer the location -> The bigger the value
  k_distance <- Kernel_distance(data, c(target$longitude[1], target$latitude[1]), smoothH$h_distance[1])
  
  #Basically a vector of values between 0-1 depending on the date of the data we are comparing to. The closer the date -> The bigger the value
  k_day <- Kernel_day(data, target$date[1], smoothH$h_date[1])

  #Constructing vectors that we can store the temperature values based on the summation and the product of the kernels
  temp_sum <- vector(length = length(times))
  temp_mult <- vector(length = length(times))
  
  for (i in 1:length(times)){
    #Basically a vector of values between 0-1 depending on the time of the data we are comparing to. The closer the time -> The bigger the value
    k_hour <- Kernel_hours(data, times[i], smoothH$h_time[1])
    
    #Summation of kernels
    k_tot_sum <- k_distance + k_day + k_hour
    
    #Product of kernels
    k_tot_mult <- k_distance * k_day * k_hour
    
    #Vectors of temperature values between 4am to 24pm
    #Kernel-weighted average over all the points slide 8, 3a
    temp_sum[i] <- sum(k_tot_sum %*% data$air_temperature)/sum(k_tot_sum)
    temp_mult[i] <- sum(k_tot_mult %*% data$air_temperature)/sum(k_tot_mult)
  }
  return(list(temp_sum = temp_sum, temp_mult = temp_mult))
}


Estimated_temps <- Est_temp(FilteredData_date, targets, H_vals, times)

pdf('Kernel_sum.pdf')
plot(Estimated_temps$temp_sum, xaxt = "n", xlab="Time of Day", ylab="Temperature", type="o", main = "Sum of kernels")
axis(1, at=1:length(times), labels=times)
dev.off()

pdf('Kernel_produkt.pdf')
plot(Estimated_temps$temp_mult, xaxt = "n", xlab="Time of Day", ylab="Temperature", type="o", main = "Product of kernels")
axis(1, at=1:length(times), labels=times)
dev.off()
