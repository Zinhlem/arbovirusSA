# Spreadsheet from JP's Applied Ecology of Infectious Diseases course at UF
# Converting Spreadsheet code into R

rm(list=ls())

#parameters 
parms <- c( Tmin = 10.4,
            Tmax = 39.5,
            alpha = 0.00006)

############################ **Rate(T)** ###############################

#Temperature range
Temp <- c(parms["Tmin"]:parms["Tmax"]) 

#Development Rate 
Rate.temp <- function(temp, params){
        with(as.list(params),{
        alpha * temp * (temp - Tmin) * sqrt(Tmax - temp)
        })
}

rate <- Rate.temp(Temp, parms)

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


########################## **Constant (C) Daily** ########################

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

#########################**Daily Mean (mean temp)** ############################

Temp.mean <- c(29.7, 29.75, 29.75, 30.3, 30, 30.55, 29.75, 29.75, 31.1,
            31.4, 30.55, 29.75, 30, 28.35, 28.65, 28.6, 29.2, 29.7, 
            29.45, 29.7, 29.2, 28.05, 26.95, 29.15, 28.65, 29.45, 30, 
            29.45, 29.15, 29.7, 29.45, 29.15, 29.15, 29.45, 28.65, 28.35, 
            27.5, 28.35, 30.25, 29.75, 30.55, 30.85, 27.8, 29.2, 29.2, 
            28.35, 29.15, 27.5, 29.15, 29.2, 28.65, 29.75, 29.75, 28.9, 
            28.05, 28.65, 26.4, 28.9, 26.95, 29.75)    #daily mean temp 

rate.mean <- Rate.temp(Temp.mean, parms)

Progress.mean <- Progress(Temp.mean, parms, Time.spent)

Cum.Progress.mean <- Cum.Progress(Temp.mean, parms, Time.spent)

plot(Temp.mean, type = "l", col = "blue", lwd = 3, ylab = "Temperature")

plot(rate.mean, type = "l", col = "red", lwd = 3, ylab = "Development rate")
             


########################### ** DAily Max Min** ###########################

Temp.max.min <- c(27.2, 22.8, 27.2, 18.9, 21.1, 16.1, 23.9, 15, 25, 20, 25.6, 21.1,
              25.6, 23.3, 26.1, 21.7, 26.1, 21.7, 25.6, 23.9, 25.6, 21.1, 26.1, 20.6, 20,
              17.2, 21.7, 16.1, 23.9, 18.9, 23.9, 19.4) 

rate.max.min <- Rate.temp(Temp.max.min, parms)

Time.spent.max.min <- 0.5

Progress.max.min <- Progress(Temp.max.min, parms, Time.spent.max.min)

Cum.Progress.max.min <- Cum.Progress(Temp.max.min, 
                                             parms, Time.spent.max.min)

########################### ** DAily Max Min2** ###########################
