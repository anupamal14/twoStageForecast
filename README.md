# twoStageForecast
Use this package to forecast time series with multiple levels of seasonality. <br>
The elecLoad.csv is a sample electricity load data provided with the package. <br>

Example: <br>
elec <- read.csv("elecLoad.csv") <br> 
d <- elec$load <br>
pred <- auto.twoStage(d,288 x 7 x 10,288 x 7 x 2,c(288,288 x 7)) <br> 

