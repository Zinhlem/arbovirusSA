rm(list = ls())

max_temp_Durban <- read.delim("0240808A2_tasmax.txt",
                              stringsAsFactors =  F) 
head(max_temp_Durban)#1992 to 2012

min_temp_Durban <- read.delim("0240808A2_tasmin.txt", 
                              stringsAsFactors =  F)
head(min_temp_Durban) #1992 to 2012

prec_Durban <-  read.delim("0240808A2_pr.txt", 
                           stringsAsFactors =  F)
head(prec_Durban) #1959 to 2000

prec_Durban2 <-  read.delim("0240891_0_pr.txt", 
                            stringsAsFactors =  F)
head(prec_Durban2) #1871 to 2000






