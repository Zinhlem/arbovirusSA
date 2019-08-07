## Zinhle August 2019
## Eip with fixed temperature
## IP function adapted from tsetse emergence function

rm(list=ls())

library(here)
library(readxl)

##parameters
params <- c(a = 5.3,
            b = -0.24,
            k = 0.16)

#temperature
my_data <- read_xlsx(paste(here(),"Temperature_Effect_on_Development.xlsx",sep="/"),
                     sheet = "DailyMean") 

temp <- my_data$TempC# 20:33

##rate function from tsetse emergence function
EIP_Fxn <- function(temprature,parms){
  with(as.list(parms),{
    (1+exp(a+b*temp))/ k
  })
}

EIP <- EIP_Fxn(temp,params)
#plot(temp, EIP_Fxn)

## Cumulative EIP, Eip >= 1 inefctious
Cum_EIP_Fxn <- function(temprature, parms){
  with(as.list(parms),{
    cum_eip_rate <- 1/EIP_Fxn(temprature, parms) #rate fucntion : 1/EIP
    diffinv(cum_eip_rate)
  })
}
Cum_EIP <- Cum_EIP_Fxn(temp, params)

##initial conditions
## start of with one mosquito
#msqt doesn't neccesarily have to be 1 day old
## infcetivity =1 when mosquito is infected
msqt_init <- c(Age = 1, days_infec_bite = 0, infectivity = 0)
# *days_infec_bite* - days since infectious feed/bite

##function to update mosquito age, status and EIPO
update <- function(temprature, parms, eip, timestep, n){
  EIP_Cum_Calc <- Cum_EIP_Fxn(tmprt, parms) #cum EIP
  Msqt_data <- data.frame()
  
  for(i in seq(2, n ,timestep)){
    time <- i - 1 #to make time start from 1
    Age <- msqt_init['Age'] + time  #update at each time step
    days_infec_bite <- msqt_init['days_infec_bite'] + time
    tmprt <- temprature[i]
    Cum_EIP_rate <- EIP_Cum_Calc[i] #evaluates cum EIP at [i]
    Cum_EIP_rate <- ifelse(Cum_EIP_rate >= 1, 1, Cum_EIP_rate) #ensure rate isn't >1
    infectivity <- ifelse(Cum_EIP_rate < 1, 0, 1) #0 if not infected, 1 otherwise
    Msqt_data <- rbind(Msqt_data, data.frame(time, Age, tmprt, days_infec_bite, Cum_EIP_rate, infectivity))
  }
  return(Msqt_data)
}
mosquito_dat <- as.data.frame(update(temprature = temp, parms = params, eip = EIP, timestep = 1, n = length(temp) ))
View(mosquito_dat)

