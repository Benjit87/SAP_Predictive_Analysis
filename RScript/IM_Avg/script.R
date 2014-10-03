#Impute missing columns with average
IM_Avg<- function(data,col)
{
toimpute <- data[,col]
missing <- is.na(toimpute)
toimpute[missing] <- mean(toimpute[!missing])
data[,col] <- toimpute
hist(data[,col],main="",xlab=col)
return(list(out=data,model=data[,col]))
}