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
  cum.progress <- c()
  for(i in 2 : length(temp)){
    cum.progress[i] <- Progress[i-1] + Progress[i]
  }
  return(cum.progress)
}

Cum.ProgressT <- Cum.Progress(Temp, parms, Time.spent)

##Initial conditions
EggsInit <- 10
AdultsInit <- 0 
AdultsAge <- 0
#EggsAge <- rep(x = 0, times = length(Temp))
EggsAge <- 0

EggsCount <- function(temp, params, time, Egg0, eggAge){
  
  CumProgEggs <- Cum.Progress(temp, params, time)
  Eggs <- ifelse(CumProgEggs < 1, 1 , 0) #birth
  Eggs <- ifelse(CumProgEggs <1,  Egg0 + Eggs, 0)
  age <- ifelse(Eggs >= 1, eggAge + 1, 0) #why &cum.prog? leave for now
  EggAge <- c()
   for(i in 2 : length(temp)){
    EggAge[i] <- ifelse(CumProgEggs[i] < 1, sum(EggAge[i-1], age[i], na.rm = T), 0)
  }
  TotEggs <- c()
  for(i in 2 : length(temp)){
    TotEggs[i] <- sum(Eggs[i])
  }
  return(data.frame("Temp" = temp, "Eggs" = Eggs, "Cumpro" = CumProgEggs, 
                    "Age" = EggAge, "TotalEgs" = TotEggs))
  
}
Eggsdf <- EggsCount(Temp, parms, Time.spent,EggsInit, EggsAge)
head(Eggsdf)

 
AdultsCount <- function(temp, params, time, Egg0, eggAge, adult0, adultAge){
  Adults <- ifelse(Eggsdf$Cumpro > 1, adult0 + max(Eggsdf$Eggs, na.rm = T), 0) #subtract emerged mosquitoes instead of 1
  A.age <- ifelse(Adults >= 1, adultAge + 1, 0)
  AdultAge <- c()
  for(i in 2 : length(temp)){
    AdultAge[i] <- ifelse(Eggsdf$Cumpro[i] >1, 
                          sum(AdultAge[i - 1] , A.age[i], na.rm = T), 0)
  }
  return(data.frame("Adults" = Adults, "AdultsAge" = AdultAge))
}

Adultsdf <- AdultsCount(Temp, parms, Time.spent, Eggsdf$Eggs, EggsAge,
                        AdultsInit, AdultsAge )

Popdf <- data.frame(Eggsdf, Adultsdf)

