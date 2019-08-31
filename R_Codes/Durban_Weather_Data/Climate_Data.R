## Zinhle Mthombothi August 2019
# Weather data from Durban

library(tidyverse)

rm(list = ls())

#max temperature data from Durban betwen 1992 to 2012
max_temp_Data <- read.delim("0240808A2_tasmax.txt",
                             stringsAsFactors =  F) #1992 to 2012
head(max_temp_Data)


#min temperature data from Durban betwen 1992 to 2012
min_temp_Data <- read.delim("0240808A2_tasmin.txt", 
                              stringsAsFactors =  F)
head(min_temp_Data)

#Function to clean the Durban data. Remove the first two rows with the date and coordinates

clean_temp_data <- function(temperature_df){
    temperature_df <- temperature_df[-c(1, 2),] #remove the first 2 rows with characters the other 3 have temp of -999
    temperature_df <- as.numeric(temperature_df)
    temperature_df <- as.data.frame(temperature_df) #turn it back to dataframe 
    names(temperature_df) <- c("Durban_temperature")
    head(temperature_df) #1992 to 2012
    
    temperature_df <- temperature_df %>% filter_all(any_vars(. != -999)) #removing the undefined values -999
    
}

# Cleaned weather data frame
max_temp_DF <- clean_temp_data(max_temp_Data)
min_temp_DF <- clean_temp_data(min_temp_Data)


###***************************PRECIPITATION****************************************

#precipitation data from Durban betwen 1959 to 2000
precip_Durban <-  read.delim("0240808A2_pr.txt", 
                           stringsAsFactors =  F)
head(precip_Durban) #1959 to 2000

##precipitation data from Durban betwen 1871 and 2000
precip_Durban2 <-  read.delim("0240891_0_pr.txt", 
                            stringsAsFactors =  F)
head(precip_Durban2) #1871 to 2000

## function to clean precipitation data 

clean_precip_data <- function(precip_df){
    precip_df <- precip_df[-c(1, 2),] #remove the first 2 rows with characters the other 3 have temp of -999
    precip_df <- as.numeric(precip_df)
    precip_df <- as.data.frame(precip_df) #turn it back to dataframe 
    names(precip_df) <- c("Durban_precipitation")
    head(precip_df) #1992 to 2012
    
    precip_df <- precip_df %>% filter_all(any_vars(. >= 0)) #removing the undefined values -999
    
}

#Clean precipitation DF
precip_df_59_00 <- clean_precip_data(precip_Durban)
precip2_df_71_00 <- clean_precip_data(precip_Durban2)
