#Decision Tree, Random Forest#
DT_RF <- function(data,y,x,tree)
{
library(randomForest)
formattedString<-paste(x,collapse='+')
formatedString <- as.formula(paste(paste(y," ~ ",collapse=""),formattedString,collapse=""))
rf <- randomForest(formatedString , data=data, ntree=tree, proximity=FALSE)
predictrf <- data.frame(predict(rf))

#visualization
plot(rf)

return(list(out=cbind(data,predictrf), model=rf))
}

#Scoring random forest
DT_RF_Score <- function(data,model,x)
{
library(randomForest)
results <- predict(model,newdata=data)
resultsdf <- data.frame(results)
return(list(out=cbind(data,resultsdf)))
}