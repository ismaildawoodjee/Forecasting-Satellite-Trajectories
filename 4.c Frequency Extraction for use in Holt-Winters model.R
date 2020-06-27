train <- read.csv('train_6.csv')
#test <- read.csv('test_forR.csv')

library(forecast)
library(fpp2)
library(dplyr)

freq <- list() # list of frequencies
sat <- list() # list of satellites having that frequency

sat_ids <- as.vector(distinct(train, sat_id)[,'sat_id'])

i <- 1
for (n in sat_ids) {
  
  satdata_n <- filter(train, sat_id %in% n)
  f <- findfrequency(satdata_n[,'Vz'])
  
  freq[[i]] <- f
  i <- i+1
  
  #if (f != 24) {
  #  freq[[i]] <- f
  #  sat[[i]] <- n
  #  #freq[i+1] <- f
  #  i <- i+1
  #} 
}

freq <- unlist(freq)

print(freq)
print(sat)


#df$f_x <- freq

# export df to csv
write.csv(df, 'frequencies.csv', row.names = FALSE)





