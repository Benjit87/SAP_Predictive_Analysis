#Expanding Arima
tsExpandingArima<-function(data,input,startDate,Quarter,shiftNumber,startWindow,endWindow,o1,o2,o3,s1,s2,s3) {

output <- ts(data[,input],frequency=4,start=c(startDate,Quarter))
fcastResults <- data.frame()

for ( i in 0:(shiftNumber-1) )
{
win <- window(output,start=startWindow,end=endWindow+(i*0.25))
fit <- arima(win,order=c(o1,o2,o3), seasonal = list(order = c(s1,s2,s3)))
fcasts <- data.frame(forecast(fit,h=1))
fcasts$WindowItineration <- (i+1)

fcastResults <- rbind(fcastResults,fcasts)
}

actual <- data.frame(window(output,start=endWindow+0.25,end=endWindow+((shiftNumber)*0.25)))
names(actual) <- "Actual Values"
df <- cbind(fcastResults,actual)
df$dates <- rownames(df)

#RMSE
predictdiff <- data.frame(df$Point.Forecast-df$"Actual Values")
rmse <-mean(predictdiff[,1]^2)
rmse <- data.frame(paste("[",rmse,"]"))
names(rmse) <- "RMSE in []"

#Plot
plot(ts(data.frame(df$Point.Forecast,df$"Actual Values"),frequency=4,start=endWindow+0.25,end=(endWindow+((shiftNumber)*0.25))),main="Forecasted Results (BLACK) Vs Actual (RED)",plot.type="single",col=1:10,ylab="Output")

return(list(out=df,model=rmse))
}