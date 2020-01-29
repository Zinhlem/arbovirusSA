#' Functions that I will use often in my mosquito model

library(here)
library(readxl)
library(knitr)

#(s = system.file("examples", "Functions_mosq_pop.R", package = "knitr"))

#' a,b,k are parameters adapted from the tsetse fly emergence rate function
#' THis will change to be a mosquito function in future
params <- c(a = 5.3,
            b = -0.24,
            k = 0.16)

#' rate function from tsetse emergence function
EIP_Fxn <- function(temprature,parms){
    with(as.list(parms),{
        (1+exp(a+b*temprature))/ k
    })
}

#' Cumulative EIP. Add the daily EIP such that when EIP >= 1 the vector is infectious
#' Cum EIR is the parasite development rate 

Cum_EIR_Fxn <- function(temprature, parms){ 
    with(as.list(parms),{
        cum_eip_rate <- 1/EIP_Fxn(temprature, parms) #' EI rate fucntion = 1/EIP
        diffinv(cum_eip_rate)
    })
}

#' function to update mosquito age, status and EIP
#' inputs: temperature data, params, EIP, timestep and max period (n)
update <- function(temprature, parms, eip, timestep, n){
    EIP_Cum_Calc <- eip #cum EIP
    Msqt_data <- data.frame()
    
    for(i in seq(2, n ,timestep)){
        time <- i - timestep #' to make time start from 1
        Age <- msqt_init['Age'] + time  #' update at each time step
        days_infec_bite <- msqt_init['days_infec_bite'] + time
        tmprt <- temprature[i]
        Cum_EI_rate <- EIP_Cum_Calc[i] #' evaluates cum EIP at [i]
        Cum_EI_rate <- ifelse(Cum_EI_rate >= 1, 1, Cum_EI_rate) #'  ensure rate isn't >1
        infectivity <- ifelse(Cum_EI_rate < 1, 0, 1) #' 0 if not infected, 1 otherwise
        Msqt_data <- rbind(Msqt_data, data.frame(time, Age, tmprt, days_infec_bite, Cum_EI_rate, infectivity))
    }
    return(Msqt_data)
}
