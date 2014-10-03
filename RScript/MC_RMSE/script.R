#Caclulate the different RMSE of different models 
#when results of various models are appended together
MC_RMSE <- function(data,actual,predicted,modelname)
{
df <- data.frame(modelname=character(),rmse=character())
modeltype <- unique(data[,modelname])
modellen <- length(modeltype)
modelcol <- data[,modelname]
for ( i in 1:modellen )
{
modelfilter <- modelcol == modeltype[i]
model <- data[modelfilter==TRUE,]
predictdiff <- model[,actual] - model[,predicted]
rmse <-mean(predictdiff^2)
df <- rbind(df,data.frame(modeltype[i],rmse))

}
names(df) <- c("MODELNAME","RMSE")

return(list(out=df))

}