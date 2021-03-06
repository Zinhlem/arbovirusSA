# Relies on output from Climate_Data.R
library(tidyverse)
max_temp <-readr::read_csv('Durban_max.csv') %>% select(-X1)
names(max_temp) <- c('dt', 'maxC')
min_temp <- readr::read_csv('Durban_min.csv') %>% select(-X1)
names(min_temp) <- c('dt', 'minC')

dat <- (full_join(max_temp, min_temp, by = 'dt') 
        %>% mutate(meanC = (minC + maxC)/2)
        %>% mutate(month = as.numeric(format(dt, '%m')))
        %>% mutate(week = as.numeric(format(dt, '%U')))
        %>% mutate(year = format(dt, '%Y'))
)
dat

weekly_dat <- (dat 
               %>% group_by(week, year) 
               %>% summarize(medMean = median(meanC)
                             , meanMean = mean(meanC)
                             , meanMax = mean(maxC)
                             , meanMin = mean(minC)
                             , maxMax = max(maxC)
                             , minMin = min(minC))
               )
monthly_dat <- (dat 
               %>% group_by(month, year) 
               %>% summarize(medMean = median(meanC)
                             , meanMean = mean(meanC)
                             , meanMax = mean(maxC)
                             , meanMin = mean(minC)
                             , maxMax = max(maxC)
                             , minMin = min(minC))
)

(ggplot(weekly_dat)
  + aes(x = week, y = meanMean, color = year)
  + geom_line()
  + ylim(0,30)
  + ylab('Temperature (C)')
  + stat_summary(fun = mean, color = 'black', geom = 'line', lwd = 1.5)
  + stat_summary(mapping = aes(y = meanMax), fun = mean,  geom = 'line', color = 'black')
  + stat_summary(mapping = aes(y = meanMin), fun = mean, geom = 'line', color = 'black')
  + stat_summary(mapping = aes(y = maxMax), fun = max, geom = 'line', color = 'black', lty = 3)
  + stat_summary(mapping = aes(y = minMin), fun = min, geom = 'line', color = 'black', lty = 3)
)

(ggplot(monthly_dat)
  + aes(x = month, y = meanMean, color = year)
  + geom_line()
  + ylim(0,30)
  + ylab('Temperature (C)')
  + stat_summary(fun = mean, color = 'black', geom = 'line', lwd = 1.5)
  + stat_summary(mapping = aes(y = meanMax), fun = mean,  geom = 'line', color = 'black')
  + stat_summary(mapping = aes(y = meanMin), fun = mean, geom = 'line', color = 'black')
  + stat_summary(mapping = aes(y = maxMax), fun = max, geom = 'line', color = 'black', lty = 3)
  + stat_summary(mapping = aes(y = minMin), fun = min, geom = 'line', color = 'black', lty = 3)
)

