## Zinhle August 2019
## Eip with fixed temperature
## IP function adapted from tsetse emergence function

rm(list=ls())

##parameters
params <- c(a = 5.3,
            b = -0.24,
            k = 0.16)
temp <- 33


##rate function from tsetse emergence function 
EIP_Fxn <- function(temprature,parms){
  with(as.list(parms),{
       (1+exp(a+b*temp))/ k
  })
}
EIP <- EIP_Fxn(temp,params)
#plot(temp, EIP_Fxn)


##initial conditions
## start of with one mosquito
#msqt doesn't neccesarily have to be 1 day old
## infcetivity =1 when mosquito is infected
msqt_init <- c(Age = 1, days_infec_bite = 0, infectivity = 0) 
# *days_infec_bite* - days since infectious feed/bite

##function to update mosquito age, status and EIPO
update <- function(temprature, parms, eip, timestep, n){ 
  EIP_calc <- eip     #Calculated EIP from EIP function
  Msqt_data <- data.frame() 
  
  for(i in seq(2, n ,timestep)){
    time <- i - 1 #to make time start from 1
    Age <- msqt_init['Age'] + time  #update at each time step
    days_infec_bite <- msqt_init['days_infec_bite'] + time
    infectivity <- ifelse(days_infec_bite <= EIP_calc, 0, 1) #0 if not infected, 1 otherwise
    Msqt_data <- rbind(Msqt_data, data.frame(time, temprature, EIP_calc, Age, days_infec_bite, infectivity))
  }
  return(Msqt_data)
}

mosquito_dat <- as.data.frame(update(temprature = temp, parms = params, eip = EIP, timestep = 1, n =20 ))
head(mosquito_dat, 10)