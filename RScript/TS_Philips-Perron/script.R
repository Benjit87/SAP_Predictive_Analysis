#Philips-Perron Test for Unit Roots
tsPP<-function(data,input,startDate,Quarter,endDate,endQuarter) {
ts1 <- ts(data[,input],frequency=4,start=c(startDate,Quarter),end=c(endDate,endQuarter))
results<-PP.test(ts1)

Method <- paste(results$method)
DickeyFuller <- paste(results$statistic)
Pvalue <- paste(results$p.value)
Truncation_Lag_Parameter <- paste(results$parameter)
df <- data.frame(DickeyFuller ,Truncation_Lag_Parameter ,Pvalue ,Method )

library(plotrix)
df2 <- data.frame(rbind(Method,DickeyFuller,Pvalue,Truncation_Lag_Parameter))
names(df2) <- ""
plot(1, type="n", axes=F, xlab="", ylab="")
addtable2plot(1,1,df2,bty="o",display.rownames=TRUE,hlines=TRUE, vlines=TRUE, title="Phillips-Perron Unit Root Test",xpad=0.5,ypad=1)

return(list(out=df,model=df ))
}