#TS_Augmented_DickeyFuller
tsADF<-function(data,input,startDate,Quarter,endDate,endQuarter) {
library(fpp)
ts1 <- ts(data[,input],frequency=4,start=c(startDate,Quarter),end=c(endDate,endQuarter))
results<-adf.test(ts1, alternative="stationary")

Method <- paste(results$method)
DickeyFuller <- paste(results$statistic)
Pvalue <- paste(results$p.value)
Lag_Order <- paste(results$parameter)
Alternative_Hypothesis <- paste(results$alternative)
df <- data.frame(DickeyFuller ,Lag_Order ,Alternative_Hypothesis ,Pvalue,Method)

library(plotrix)
df2 <- data.frame(rbind(Method,DickeyFuller,Pvalue,Lag_Order,Alternative_Hypothesis))
names(df2) <- ""
plot(1, type="n", axes=F, xlab="", ylab="")
addtable2plot(1,1,df2,bty="o",display.rownames=TRUE,hlines=TRUE, vlines=TRUE, title="Augmented Dickey-Fuller Unit Root Test",xpad=0.5,ypad=1)

return(list(out=df,model=df ))
}