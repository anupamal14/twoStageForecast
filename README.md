# twoStageForecast
Use this package to forecast time series with multiple levels of seasonality

Example:
elec <- read.csv("elecLoad.csv") 
d <- elec$load
pred <- auto.twoStage(d,288*7*10,288*7*2,c(288,288*7)) 

