#load libraries
library(xts)

#The purpose of this function is to create a function that will
#create weighted calculations in a time-series matrix.
#For example sum(AHT * Handle)/sum(handled) = aggregated AHT

#First at least 2 generic time-series matrix will need to be created
set.seed(12822)
dates = seq(as.Date("2016-01-01"),
            as.Date("2016-09-30"), by="days")
rcvd = rnorm(n = length(dates), mean = 8000, sd = 650)
aht = rnorm(n = length(dates), mean = 650, sd = 15)
handled = rcvd - rnorm(n = length(dates), mean = 240, sd = 24)
sl = rnorm(n = length(dates), mean = .75, sd = .25/3.1)
df = cbind(dates, rcvd, handled, aht, sl)
#make sure to use as.xts as the xts() call is used to make NEW xts objects
a.xts = as.xts(df[, -1], order.by = dates)

set.seed(2)
dates = seq(as.Date("2016-01-01"),
            as.Date("2016-09-30"), by="days")
rcvd = rnorm(n = length(dates), mean = 4500, sd = 300)
aht = rnorm(n = length(dates), mean = 700, sd = 20)
handled = rcvd - rnorm(n = length(dates), mean = 135, sd = 13.5)
sl = rnorm(n = length(dates), mean = .65, sd = .30/3.1)
df = cbind(dates, rcvd, handled, aht, sl)

b.xts = as.xts(df[, -1], order.by = dates)
