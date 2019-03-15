# Spreadsheet from JP's Applied Ecology of Infectious Diseases course at UF
# Converting Spreadsheet code into R


#parameters 
parms <- c( Tmin = 10.4,
            Tmax = 39.5,
            alpha = 0.00006)

#Temperature range
Temp <- c(parms["Tmin"]:parms["Tmax"])


#Development Rate 
Rate.temp <- function(params, Temp){
        with(as.list(params),{
        alpha * Temp * (Temp - Tmin) * sqrt(Tmax - Temp)
        })
}

rate <- Rate.temp(parms,Temp)

plot(Temp, rate, type = "l", col = "red", lwd = 3,
     xlab = "Temperature", ylab = "Development rate")

#Developmental Time 
Dev.time <- function(Temp, params){
        with(as.list(params),{
        1/ (alpha * Temp * (Temp - Tmin) * sqrt(Tmax - Temp))
})
        }

dev <- Dev.time(Temp, parms)

plot(Temp, dev, type = "l", col = "blue", lwd = 3,
     xlab = "Temperature", ylab = "Development time (days)")

