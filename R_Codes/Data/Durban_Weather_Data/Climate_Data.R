## Zinhle Mthombothi August 2019
# Weather data from Durban

library(tidyverse)
library(zoo)

rm(list = ls())

#max temperature data from Durban betwen 1992 to 2012
max_temp_Data <- read.delim("0240808A2_tasmax.txt",
                             stringsAsFactors =  F) #1992 to 2012
head(max_temp_Data)

#min temperature data from Durban betwen 1992 to 2012
min_temp_Data <- read.delim("0240808A2_tasmin.txt", 
                              stringsAsFactors =  F)
head(min_temp_Data)

#Dates
#max_temp_Data[1,]
startDate <- as.Date('1992/09/01', format = '%Y/%m/%d')
endDate <- as.Date('2012/12/31', format = '%Y/%m/%d')


#Function to clean the Durban data. Remove the first two rows with the date and coordinates
clean_temp_data <- function(temperature_df, start, end){
   
    temperature_df <- temperature_df[-c(1, 2),] #remove the first 2 rows with characters the other 3 have temp of -999
    dates_df <- start:end
    temperature_df <- as.numeric(temperature_df)
    temperature_df <- as.data.frame(temperature_df) #turn it back to dataframe 
    names(temperature_df) <- c("Durban_temperature")
    dates_data <- as.data.frame(dates_df)
    names(dates_data) <- c("dates")
    temperature_data <- as.data.frame(cbind(dates_data$dates, temperature_df$Durban_temperature)) #1992 to 2012
    names(temperature_data) <- c("date", "Durban_temperature")
    temperature_data <- temperature_data %>% filter((temperature_data$Durban_temperature != -999)) #removing the undefined values -999
     
 }

# Cleaned weather data frame
max_temp_DF <- clean_temp_data(max_temp_Data, startDate, endDate)
min_temp_DF <- clean_temp_data(min_temp_Data, startDate, endDate)

plot(as.Date(max_temp_DF$date), max_temp_DF$Durban_temperature, type = "l")
plot(as.Date(min_temp_DF$date), min_temp_DF$Durban_temperature, type = "l")

###***************************PRECIPITATION****************************************

#precipitation data from Durban betwen 1959 to 2000
precip_Durban <-  read.delim("0240808A2_pr.txt", 
                           stringsAsFactors =  F)
head(precip_Durban) #1959 to 2000

#dates
startDatePrecip1 <- as.Date('1959/01/01', format = '%Y/%m/%d')
endDatePrecip1 <- as.Date('2012/12/31', format = '%Y/%m/%d')

##precipitation data from Durban betwen 1871 and 2000
precip_Durban2 <-  read.delim("0240891_0_pr.txt", 
                            stringsAsFactors =  F)
head(precip_Durban2) #1871 to 2000

#dates
startDatePrecip2 <- as.Date('1871/10/01', format = '%Y/%m/%d')
endDatePrecip2 <- as.Date('2000/12/31', format = '%Y/%m/%d')


## function to clean precipitation data 

clean_precip_data <- function(precip_df, start, end){
    precip_df <- precip_df[-c(1, 2),] #remove the first 2 rows with characters the other 3 have temp of -999
    precip_df <- as.numeric(precip_df)
    precip_df <- as.data.frame(precip_df) #turn it back to dataframe
    dates_df <- start:end
    dates_data <- as.data.frame(dates_df)
    names(dates_data) <- c("dates")
    names(precip_df) <- c("Durban_precipitation")
    head(precip_df) #1992 to 2012
    precip_data <- as.data.frame(cbind(dates_data$dates, precip_df$Durban_precipitation)) #1992 to 2012
    names(precip_data) <- c("dates", "Durban_precipitation")
    precip_df <- precip_data %>%  filter((precip_data$Durban_precipitation != -999)) #removing the undefined values -999

}

#Clean precipitation DF
precip_df_59_00 <- clean_precip_data(precip_Durban, start = startDatePrecip1, end = endDatePrecip1 )
plot(as.Date(precip_df_59_00$dates), precip_df_59_00$Durban_precipitation, type = "l")
precip2_df_71_00 <- clean_precip_data(precip_Durban2, start = startDatePrecip2, end = endDatePrecip2)
plot(as.Date(precip2_df_71_00$dates), precip2_df_71_00$Durban_precipitation, type = "l")
