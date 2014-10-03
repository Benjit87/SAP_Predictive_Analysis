#Replacing missing columns with specified value
IM_Replace<- function(data,col,replace)
{
toimpute <- data[,col]
missing <- is.na(toimpute)
toimpute[missing] <- replace
data[,col] <- toimpute
return(list(out=data,model=data[,col]))
}