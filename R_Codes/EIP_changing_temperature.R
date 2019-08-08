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
# my_data <- read_xlsx(paste(here(),"~/GitHub/arbovirusSA/R_Codes/Temperature_Effect_on_Development.xlsx",sep="/"),
#                      sheet = "DailyMean") 

temp <- 18:33 #my_data$TempC


# Changing temperaure changing EIP ----------------------------------------


##rate function from tsetse emergence function
EIP_Fxn <- function(temprature,parms){
  with(as.list(parms),{
    (1+exp(a+b*temprature))/ k
  })
}

EIP <- EIP_Fxn(temp,params)
#plot(temp, EIP_Fxn)

## Cumulative EIP, Eip >= 1 inefctious
# Cum EIP rate is the parasite development rate 

Cum_EIP_Fxn <- function(temprature, parms){
  with(as.list(parms),{
    cum_eip_rate <- 1/EIP_Fxn(temprature, parms) #rate fucntion : 1/EIP
    diffinv(cum_eip_rate)
  })
}
Cum_EIR <- Cum_EIP_Fxn(temp, params) ## Cumulative EI rate

##initial conditions
## start of with one mosquito
#msqt doesn't neccesarily have to be 1 day old
## infcetivity =1 when mosquito is infected
msqt_init <- c(Age = 1, days_infec_bite = 0, infectivity = 0)
# *days_infec_bite* - days since infectious feed/bite

##function to update mosquito age, status and EIPO
update <- function(temprature, parms, eip, timestep, n){
  EIP_Cum_Calc <- eip #cum EIP
  Msqt_data <- data.frame()
  
  for(i in seq(2, n ,timestep)){
    time <- i - 1 #to make time start from 1
    Age <- msqt_init['Age'] + time  #update at each time step
    days_infec_bite <- msqt_init['days_infec_bite'] + time
    tmprt <- temprature[i]
    Cum_EI_rate <- EIP_Cum_Calc[i] #evaluates cum EIP at [i]
    Cum_EI_rate <- ifelse(Cum_EI_rate >= 1, 1, Cum_EI_rate) #ensure rate isn't >1
    infectivity <- ifelse(Cum_EI_rate < 1, 0, 1) #0 if not infected, 1 otherwise
    Msqt_data <- rbind(Msqt_data, data.frame(time, Age, tmprt, days_infec_bite, Cum_EI_rate, infectivity))
  }
  return(Msqt_data)
}

mosquito_dat <- as.data.frame(update(temprature = temp, parms = params , eip = Cum_EIR , timestep = 1, n = length(temp) ))
View(mosquito_dat)

# Changing temperature fixed EIP ------------------------------------------

Fixed_EIP <- rep(x = 9, times = length(temp)) #days

Cum_Fixed_EIP_Fxn <- function(temprature, parms){
    with(as.list(parms),{
        cum_fixed_eip_rate <- 1/Fixed_EIP #rate fucntion : 1/EIP
        diffinv(cum_fixed_eip_rate)
    })
}

Cum_Fixed_EIR <- Cum_Fixed_EIP_Fxn(temp, params)

mosquito_dat_fixed_eip <- as.data.frame(update(temprature = temp, parms = params, eip = Cum_Fixed_EIR, timestep = 1, n = length(temp) ))
View(mosquito_dat_fixed_eip)


# fixed temp, fixed EIP ---------------------------------------------------

temp_fixed <- rep(x = 25, times = length(temp))

Fixed_EIP_Temp <- EIP <- EIP_Fxn(temp_fixed,params)

Cum_Fixed_EIR_Temp <- Cum_EIP_Fxn(temp_fixed, params)

mosquito_dat_fixed_eip_temp <- as.data.frame(update(temprature = temp_fixed, parms = params, eip = Cum_Fixed_EIR_Temp, timestep = 1, n = length(temp) ))
View(mosquito_dat_fixed_eip_temp)

## plot rate over temp 

plot(mosquito_dat$tmprt, mosquito_dat$Cum_EI_rate, xlab = "Temperature", 
     ylab = "parasite development rate", main = "Extrinsic Incubation rate", 
     type =  "l", lwd = 3, col = "red")
lines(mosquito_dat$tmprt, mosquito_dat_fixed_eip$Cum_EI_rate, lwd = 3,
      col = "blue")
lines(mosquito_dat$tmprt, mosquito_dat_fixed_eip_temp$Cum_EI_rate, lwd = 3,
      col = "black")
abline( h = 1, lwd = 2, lty = 2, col = "gray")
legend("bottomright", legend = c("Changing temperature and EIR","Fixed EIR, varying temperature",
                                 "Fixed temperature and EIP"), col = c("red","blue", "black"),
       lwd = 3)
