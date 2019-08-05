## Zinhle August 2019
## Eip with fixed temperature
## IP function adapted from tsetse emergence function

rm(list=ls())

##parameters
params <- c(temp <- 33, #16:33,
            a <- 5.3,
            b <- -0.24,
            k <- 0.16)


##rate function from tsetse emergence function 
EIP_Fxn <- (1+exp(a+b*temp))/k
#plot(temp, EIP_Fxn)


##initial conditions
## start of with one mosquito
#msqt doesn't neccesarily have to be 1 day old
## infcetivity =1 when mosquito is infected
msqt_init <- c(Age = 1, EIP = 0, infectivity = 0)

##function to update mosquito age, status and EIPO
update <- function(time, eip, tmp){ 
  EIP_calc <- eip     #Calculated EIP from EIP function
  Msqt_data <- data.frame() 
  
  for(i in 2:time){
    time <- i - 1 #to make time start from 1
    Age <- msqt_init['Age'] + time  #update at each time step
    EIP <- msqt_init['EIP'] + time
    infectivity <- ifelse(EIP <= EIP_calc, 0, 1) #0 if not infected, 1 otherwise
    Msqt_data <- rbind(Msqt_data, data.frame(time, tmp, EIP_calc, Age, EIP, infectivity))
  }
  return(Msqt_data)
}

mosquito_dat <- as.data.frame(update(20, EIP_Fxn, temp))
head(mosquito_dat, 10)