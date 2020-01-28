## Zinhle August 2019
## EIP with fixed and changing temperature
## EIP function adapted from tsetse emergence function

rm(list=ls())

library(here)
library(readxl)

source("Functions_mosqt_pop.R")
#temperature

 my_data <- read_xlsx(paste("~/GitHub/arbovirusSA/R_Codes/Input_data/Temperature_Effect_on_Development.xlsx",sep="/"),
                      sheet = "DailyMean") 

temp <- my_data$TempC


# Changing temperaure changing EIP ----------------------------------------

EIP <- EIP_Fxn(temp,params)
plot(temp, EIP)

### Cumulative EIP, Eip >= 1 inefctious
# Cum EIR is the parasite development rate 

Cum_EIR <- Cum_EIR_Fxn(temp, params) ## Cumulative EI rate

##initial conditions
## start of with one mosquito
#msqt doesn't neccesarily have to be 1 day old
## infcetivity =1 when mosquito is infected
msqt_init <- c(Age = 1, days_infec_bite = 0, infectivity = 0)
# *days_infec_bite* - days since infectious feed/bite

##function to update mosquito age, status and EIPO

mosquito_dat_EIP_Temp <- as.data.frame(update(temprature = temp, parms = params , eip = Cum_EIR , timestep = 1, n = length(temp) ))
head(mosquito_dat_EIP_Temp)

# Changing temperature fixed EIP ------------------------------------------

Fixed_EIP <- rep(x = 9, times = length(temp)) #days

Cum_Fixed_EIP_Fxn <- function(temprature, parms){
    with(as.list(parms),{
        cum_fixed_eip_rate <- 1/Fixed_EIP #rate fucntion : 1/EIP
        diffinv(cum_fixed_eip_rate)
    })
}

Cum_Fixed_EIR <- Cum_Fixed_EIP_Fxn(temp, params)


##function to update mosquito age, status and EIPO
mosquito_dat_fixed_eip <- as.data.frame(update(temprature = temp, parms = params, eip = Cum_Fixed_EIR, timestep = 1, n = length(temp) ))
head(mosquito_dat_fixed_eip)


# fixed temp, fixed EIP ---------------------------------------------------

temp_fixed <- rep(x = 25, times = length(temp))

Fixed_EIP_Temp <- EIP <- EIP_Fxn(temp_fixed,params)

Cum_Fixed_EIR_Temp <- Cum_EIR_Fxn(temp_fixed, params)

mosquito_dat_fixed_eip_temp <- as.data.frame(update(temprature = temp_fixed, parms = params, eip = Cum_Fixed_EIR_Temp, timestep = 1, n = length(temp) ))
head(mosquito_dat_fixed_eip_temp)

## plot rate over temp 

plot(mosquito_dat_EIP_Temp$time, mosquito_dat_EIP_Temp$Cum_EI_rate, xlab = "Time", 
     ylab = "parasite development rate", main = "Extrinsic Incubation rate", 
     type =  "l", lwd = 3, col = "red")
 lines(mosquito_dat_EIP_Temp$time, mosquito_dat_fixed_eip$Cum_EI_rate, lwd = 3,
      col = "blue")
lines(mosquito_dat_EIP_Temp$time, mosquito_dat_fixed_eip_temp$Cum_EI_rate, lwd = 3,
      col = "black")
abline( h = 1, lwd = 2, lty = 2, col = "gray")
legend("bottomright", legend = c("Changing temperature and EIR","Fixed EIR, varying temperature",
                                 "Fixed temperature and EIP"), col = c("red","blue", "black"), lwd = 3)
