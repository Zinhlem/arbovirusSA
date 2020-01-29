
defaultPars <- list(a = 5.3, b = -0.24, k = 0.16)

#' calculates EIP
#'
#' @param temperature numeric vector of daily temperatures (degrees C)
#' @param parms list(a, b, k), EIP coefficients
#'
#' @return numeric vector, same length as \code{temperature} the corresponding EIPs for each day (TODO: what time unit?)
#'
#' @examples
#' temps <- 10:40
#' eips <- EIP_Fxn(temps, c(a = 5.3, b = -0.24, k = 0.16))
#' plot(temps, eips, type="l")
#'
#' @export

EIP_Fxn <- function(temperature, parms){
    return(with(parms,{ (1+exp(a+b*temperature))/ k }))
}

#' Calculates the EIP rate. Cum EIR is the parasite development rate
#'
#' @param temperature numeric vector of daily temperatures (degrees C)
#' @param parms list(a, b, k), EIP coefficients
#'
#' @return numeric vector, same length as \code{temperature} the corresponding EIPs for each day (TODO: what time unit?)
#'
#'  @examples
#' temps <- 10:40
#' eir <- Cum_EIR_Fxn(temps, c(a = 5.3, b = -0.24, k = 0.16))
#' plot(temps, eir, type="l")
#'
#' @export

Cum_EIR_Fxn <- function(temprature, parms){
    with(parms,{
        cum_eip_rate <- 1/EIP_Fxn(temprature, parms) #' EI rate fucntion = 1/EIP
        diffinv(cum_eip_rate)
    })
}

#' function to update mosquito age, infectivity status and EIP
#'
#' @param temperature numeric vector of daily temperatures (degrees C)
#' @param parms list(a, b, k), EIP coefficients
#' @param eip numeric vector eips calculated from the EIP_Fxn
#' @param timestep an integer indicating the timestep (days)
#' @param n length of \code{temperature}
#' @return dataframe, same length as \code{temperature}, the corresponding EIPs for each day (TODO: what time unit?)
#'
#' @export

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
