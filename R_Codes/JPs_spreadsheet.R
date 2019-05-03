# Spreadsheet from JP's Applied Ecology of Infectious Diseases course at UF
# Converting Spreadsheet code into R

rm(list=ls())
setwd("~/GitHub/arbovirusSA/R_Codes")

#parameters 
parms <- c( Tmin = 10.4,
            Tmax = 39.5,
            alpha = 0.0006)


# Rate(T) -----------------------------------------------------------------


#Temperature range
Temp <- c(parms["Tmin"]:parms["Tmax"]) 

#Development Rate 
Rate.temp <- function(temp, params){
        with(as.list(params),{
        alpha * temp * (temp - Tmin) * sqrt(Tmax - temp)
        })
}

rate <- Rate.temp(Temp, parms)

par(mfrow=c(2,1))
plot(Temp, rate, type = "l", col = "red", lwd = 3,
     xlab = "Temperature", ylab = "Development rate")

#Developmental Time 
Dev.time <- function(temp, params){
        with(as.list(params),{
        1/ Rate.temp(temp, params)
})
        }

dev <- Dev.time(Temp, parms)

plot(Temp, dev, type = "l", col = "blue", lwd = 3,
     xlab = "Temperature", ylab = "Development time (days)")

#increase in alpha increases the rate which shortens the period
#development rate increases with temp until ~37degrees 
#


# Constant (C) Daily ------------------------------------------------------


TempC <- rep( x = 30, times = 60) #constant temp

#Development Rate 
rateC <- Rate.temp(TempC, parms)

Time.spent <- 1

Progress <- function(temp, params, time){
        Rate.temp(temp, params) * time
}

ProgressC <- Progress(TempC, parms, Time.spent)

Cum.Progress <- function(temp, params, time){
        Progress <- Progress(temp, params, time)
        Cum.Progress <- Progress[1]
        for(i in 2 : length(Progress)){
                Cum.Progress[i] <- Cum.Progress[i-1] + Progress[i]
}
 return(Cum.Progress)
}

Cum.ProgressC <- Cum.Progress(TempC, parms, Time.spent)

par(mfrow=c(1,1))
plot(Cum.ProgressC, type = "l")

# Loading data ------------------------------------------------------------
library(readxl)

file <- system.file("~/GitHub/arbovirusSA/R_Codes", "Temperature_Effect_on_Development", package = "xlsx")

# Daily Mean (mean temp) --------------------------------------------------

my_data <- read_excel("~/GitHub/arbovirusSA/R_Codes/Temperature_Effect_on_Development.xlsx",
                      sheet = "DailyMean" ) 

Temp.mean <- my_data$TempC

rate.mean <- Rate.temp(Temp.mean, parms)

Progress.mean <- Progress(Temp.mean, parms, my_data$`Time spent`)

Cum.Progress.mean <- Cum.Progress(Temp.mean, parms, my_data$`Time spent`)

par(mfrow=c(2,1))
plot(Temp.mean, type = "l", col = "blue", lwd = 3, ylab = "Temperature")

plot(rate.mean, type = "l", col = "red", lwd = 3, ylab = "Development rate")
        
#rate increases with temperaure <=> dev days shorter with high temp


# Daily Max Min -----------------------------------------------------------


my_data1 <- read_excel("~/GitHub/arbovirusSA/R_Codes/Temperature_Effect_on_Development.xlsx",
                      sheet = "DailyMaxMin" ) 


Temp.max.min <- my_data1$TempC

rate.max.min <- Rate.temp(Temp.max.min, parms)

Time.spent.max.min <- my_data1$`Time spent`

Progress.max.min <- Progress(Temp.max.min, parms, Time.spent.max.min)

Cum.Progress.max.min <- Cum.Progress(Temp.max.min, 
                                             parms, Time.spent.max.min)
par(mfrow=c(2,1))

plot(Temp.max.min, type = "l", col = "blue", lwd = 3, ylab = "Temperature")


plot(rate.max.min, type = "l", col = "red", lwd = 3, ylab = "Temperature")


#Progress increases with the dev rate


# DAily Max Min2 ----------------------------------------------------------

             ### Min temp data
my_data2 <- read_excel("~/GitHub/arbovirusSA/R_Codes/Temperature_Effect_on_Development.xlsx",
                       sheet = "DailyMaxMin2") 

Temp.min2 <- my_data2$TempC_min

rate.min2 <- Rate.temp(Temp.min2, parms)

Time.min2 <- my_data2$Time_Tmin

Progress.min2 <- Progress(Temp.min2, parms, Time.min2)


             ##max temp data

Temp.max2 <- my_data2$TempC_max

rate.max2 <- Rate.temp(Temp.max2, parms)

Time.max2 <- my_data2$Time_Tmax

Progress.max2 <- Progress(Temp.max2, parms, Time.max2)


par(mfrow=c(1,1))
plot(Temp.max2, type = "l", col = "red", lwd = 2, ylab = "Temperature")
lines(Temp.min2, type = "l", col = "blue", lwd = 2)



plot(rate.max2, type = "l", col = "blue", lwd = 1, ylab = "Developmental rate")
lines(rate.min2 , type = "l", col = "red", lwd = 1)


##  Total Time spent

Time.spent <- Time.min2 + Time.max2

Progress.Total <- Progress.min2 + Progress.max2
 
##Cumulative progress


Cum.Progress.Total <- function(ProgressTotal){
        Progress.Total <- ProgressTotal
        Cum.Progress.Total <- ProgressTotal[1]
for (i in 2: length(Progress.Total)){
        Cum.Progress.Total[i] <- Cum.Progress.Total[i-1] + Progress.Total[i]
                
}
        return(Cum.Progress.Total)
}

Cum.progress.total <- Cum.Progress.Total(Progress.Total)
