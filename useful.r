
## ================= Course 2 week 1 ========================= 

## Clean NA values fom Osone

 osone <- data[1]
 good.ozone <- osone[!is.na(osone)]


## Extract the subset of rows of the data frame where Ozone 
## values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?

mean(subset(data, Ozone>31 & Temp > 90, select=Solar.R)[[1]])
##[1] 212.8
##Alternative:
rsolar <- data[data[1]>31 & data[4]>90, ][,2]
rsolar.good <- rsolar[!is.na(rsolar)]
mean(rsolar.good)
##[1] 212.8

##You can get the column names of a data frame with the `names()' function.

##You can use the `nrow()' function to compute the number of rows in a data frame.
##I v'e used head(2) or:
data[1:2, ]
##  Ozone Solar.R Wind Temp Month Day
##1    41     190  7.4   67     5   1
##2    36     118  8.0   72     5   2

##The `tail()' function is an easy way to extract the last few elements of an R object.
