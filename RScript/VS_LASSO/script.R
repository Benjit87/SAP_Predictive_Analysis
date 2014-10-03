#Variable selection with LASSO
VS_Lasso <- function(data,dependent,target,top)
{
library(lars)
y <- as.numeric(data[,target])
x <- as.matrix(data[,dependent])
model.lasso <- lars(x, y, type="lasso")
lambda.lasso <- c(model.lasso$lambda,0)
beta <- coef(model.lasso)

df <- data.frame(as.vector(beta[top+1,]))
names(df) <- "Values"
row.names(df) <- names(data.frame(beta))
lambdadf <- data.frame(lambda.lasso[top+1])
names(lambdadf) <- "Values"
row.names(lambdadf) <- "Lambda"
df <- rbind(df,lambdadf)
df <- cbind(row.names(df),df)


#Plot
colors <- rainbow(8)
matplot(lambda.lasso, beta, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
 ylab=expression(hat(beta)), col=colors)
abline(v=lambda.lasso[top+1], lty=2)
abline(h=0, lty=2)


return(list(out=df))
}