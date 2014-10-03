#Seasonal Plot
tsExplore<-function (data,input,startDate,Quarter) {
library(fpp)
output <- ts(data[,input],frequency=4,start=c(startDate,Quarter))
seasonplot(output,ylab="Output",main="Seasonal plot",col=1:50,pch=19, year.labels.left=TRUE)
return(list(out=data,model=output))
}