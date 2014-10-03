#Variable Selection with Recursive partitioning#
VS_rpart <- function(data,y,x)
{
library(rpart)

y1 <- data.frame(data[,y])
names(y1) <- y
xtemp <- data.frame(data[,x])
names(xtemp) <- x
x1 <- cbind(y1,xtemp)

formula <- as.formula(paste(y," ~  .",collapse=""))
results <- rpart(formula, data = x1)
df <- data.frame(results$variable.importance)
df <- cbind(row.names(df),df)
names(df) <- c("Variable","Importance")

return(list(out=df,model=results ))
}