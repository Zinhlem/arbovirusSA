#Zinhle May 2019
#modelling mosquito population at fixed T
#assume eggs emerge to adults at rate(T)
#Simple model

rm(list=ls())
setwd("~/GitHub/arbovirusSA/R_Codes")

#parameters 
parms <- c( Tmin = 10.4,
            Tmax = 37.5,
            alpha = 0.0006)

# Rate(T) -----------------------------------------------------------------
#Temperature range

#Temp <- rep(x = 12, times = 60)
Temp <- c(parms["Tmin"]:parms["Tmax"]) 

#Development Rate 
Rate.temp <- function(temp, params){
        with(as.list(params),{
                alpha * temp * (temp - Tmin) * sqrt(Tmax - temp)
        })
}

rate <- Rate.temp(Temp, parms)

#Developmental Time 
Dev.time <- function(temp, params){
        with(as.list(params),{
                1/ Rate.temp(temp, params)
        })
}

dev <- Dev.time(Temp, parms)

#time soent in T 
Time.spent <- 1

#time taken to emerge as an adult from egg
Progress <- function(temp, params, time){
        Rate.temp(temp, params) * time
}

ProgressT <- Progress(Temp, parms, Time.spent)

#cumulative progress, when =1 egg becomes adult
Cum.Progress <- function(temp, params, time){
        Progress <- Progress(temp, params, time)
        Cum.Progress <- Progress[1]
        for(i in 2 : length(Progress)){
                Cum.Progress[i] <- Cum.Progress[i-1] + Progress[i]
        }
        return(Cum.Progress)
}

Cum.ProgressT <- Cum.Progress(Temp, parms, Time.spent)


# Eggs emerge as adults at once -------------------------------------------

#Initial pop
eggs <- 95

adults <- 5

#if cum.progress > 1, adults == eggs+adults0, else adults == adults        
Adults <- ifelse(Cum.ProgressT > 1, eggs+adults, adults) 

#if cum.progress < 1, eggs == eggs, else eggs == 0
Eggs <- ifelse(Cum.ProgressT < 1, eggs, 0)
         #But what happens when the eggs don't emerge at the same time?

#Total Population size
TotalPop <- Eggs + Adults

Mosquito.pop <- data.frame(rate, dev, ProgressT, Cum.ProgressT, Eggs, Adults, TotalPop)
#remove inf
Mosquito.pop <- Mosquito.pop[-1,]


# introduce birth rate -----------------------------------------

b = rep( x = 1/7, times = length(Cum.ProgressT)) #make it T function

Eggs1 <- ifelse(b < runif(1), eggs + 1, eggs)

TotalPop1 <- Adults + Eggs1

Mosquito.pop1 <- data.frame(rate, dev, ProgressT, Cum.ProgressT, Eggs, Adults,
                            TotalPop1)

# par(mfrow=c(2,1))
# plot(Temp, rate, type = "l", col = "red", lwd = 3,
#      xlab = "Temperature", ylab = "Development rate")
# plot(Temp, dev, type = "l", col = "blue", lwd = 3,
#      xlab = "Temperature", ylab = "Development time (days)")

