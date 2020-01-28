## Zinhle August 2019
## Eip with fixed temperature
## IP function adapted from tsetse emergence function

rm(list=ls())


source("Functions_mosqt_pop.R")

#temperature
temp <- 10:40

##rate function from tsetse emergence function 
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
mosquito_dat <- as.data.frame(update(temprature = temp, parms = params, 
                                     eip = Cum_EIR, timestep = 1, n = length(temp) ))
head(mosquito_dat, 10)

plot(mosquito_dat$time, mosquito_dat$EIPs_calc, lty = 3, xlab = "Temperature",
     ylab = "EIP", col = "red")
