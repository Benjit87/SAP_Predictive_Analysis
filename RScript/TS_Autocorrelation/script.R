#Autocorrelation function
tsExplore<-function (data,input,startDate,Quarter) {
library(fpp)
output <- ts(data[,input],frequency=4,start=c(startDate,Quarter))
acf(output,main="Autocorrelation")
return(list(out=data,model=output))
}