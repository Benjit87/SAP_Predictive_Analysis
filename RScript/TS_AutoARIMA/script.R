#AutoARIMA
tsAutoArima<-function (data,input,startDate,Quarter,endDate,endQuarter) {
output <- ts(data[,input],frequency=4,start=c(startDate,Quarter),end=c(endDate,endQuarter))
results <- auto.arima(output)
forecastresults<- forecast(results)
plot(forecast(forecastresults))
forecastresults1 <- data.frame(forecastresults)
forecastresults1$dates <- rownames(forecastresults1)
return(list(out=forecastresults1,model=results))
}