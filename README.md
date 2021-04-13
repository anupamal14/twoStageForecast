# twoStageForecast
Use this package to forecast time series with multiple levels of seasonality <br>

Example: <br>
elec <- read.csv("elecLoad.csv") <br> 
d <- elec$load <br>
pred <- auto.twoStage(d,288*7*10,288*7*2,c(288,288*7)) <br> 

