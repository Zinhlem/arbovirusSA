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
Progress <- function(temp, params, t.spent){
  Rate.temp(temp, params) * t.spent

}

ProgressT <- Progress(Temp, parms, Time.spent)

#cumulative progress, when =1 egg becomes adult
Cum.Progress <- function(temp, params, t.spent){
  Progress <- Progress(temp, params, t.spent) 
  diffinv(Progress)
}

Cum.ProgressT <- Cum.Progress(Temp, parms, Time.spent)


# Initial Population ------------------------------------------------------

InitEggs <- c(Eggs = 1, Age = Time.spent, CumProg = 0)


# NewEggs model --------------------------------------------------------------
NewEggs <- function(temp, params, t.spent){
  NE <- data.frame()
  for(i in 1:length(temp)){
    time <- i
    Eggs <- ifelse(temp[i] < 20, 1, 5)
    Age <-  t.spent
    CumProg <- 0
    NE <- rbind(NE, data.frame(time, Eggs, Age, CumProg))
    }
  return(NE)
}
Neweggs <- as.data.frame(NewEggs(Temp, parms, Time.spent))


# Current Eggs ------------------------------------------------------------
CurrentEggs <- function(temp, params, t.spent, initpop){
  progEggs <- Progress(temp, params, t.spent)
  cumprogEggs <- Cum.Progress(temp, params, t.spent)
  #Age <- data.frame()
  CurrentEggs <- data.frame()
  for(i in 2:length(temp)){
    time <- i
    Eggs <- ifelse(cumprogEggs[i] < 1, Neweggs$Eggs[i], 0)
    Age <- Neweggs$Age[i-1] + t.spent #fix age is not changing per time step
    #CumProg <- sum(cumprogEggs[i-1], progEggs[i], na.rm = T) #adding to new cohorts
    CumProg <- sum(Neweggs$CumProg[i-1], progEggs[i], na.rm = T)
    Adults <- ifelse(cumprogEggs[i] >= 1, Neweggs$Eggs[i], 0) #first cumprog
    CurrentEggs <- rbind(CurrentEggs, data.frame(time, Eggs, Age, CumProg, Adults))
  }
  return(CurrentEggs)
}

head(CurrentEggs(Temp, parms, Time.spent, init), 10)


#Update Eggs function ----------------------------------------------------

#  ???????????????

