#Variable Selection with Random Forest
VS_RF <- function(data,y,x,tree)
{
library(randomForest)

y1 <- data.frame(data[,y])
names(y1) <- y
xtemp <- data.frame(data[,x])
names(xtemp) <- x
x1 <- cbind(y1,xtemp)

formula <- as.formula(paste(y," ~  .",collapse=""))
rf <- randomForest(formula, data=x1, ntree=tree, proximity=FALSE)
dfimportance <- data.frame(importance(rf))
df <- cbind(row.names(dfimportance),dfimportance)
results <- rbind(paste("Number of tree:",rf$ntree,collapse=""),paste("Type of random forest:",rf$type,collapse=""),paste("No of variable tried at each split:",rf$mtry,collapse=""),paste("Mean of squared residuals:",rf$rsq[100]*100,collapse=""),paste("% Var explained: ",rf$mse[100],collapse=""))
varImpPlot(rf)
return(list(out=df,model=results))
}