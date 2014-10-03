#ARIMA
tsArima<-function (data,input,startDate,Quarter,endDate,endQuarter,o1,o2,o3,s1,s2,s3,number) {
output <- ts(data[,input],frequency=4,start=c(startDate,Quarter),end=c(endDate,endQuarter))
results <- arima(output,order=c(o1,o2,o3), seasonal = list(order = c(s1,s2,s3)))
forecastresults<- forecast(results,h=number)
plot(forecast(forecastresults))
forecastresults1 <- data.frame(forecastresults)
forecastresults1$dates <- rownames(forecastresults1)
return(list(out=forecastresults1,model=results))
}