#Accuracy from predicted values and actual values
MC_Accuracy <- function(data,x,y)
{
count <- nrow(data)
compare <- as.vector(data[,x]) == as.vector(data[,y])
data$results <- 0
data[compare,"results"] <- 1
accuracy <- sum(data$result)/nrow(data)
df <- data.frame(accuracy)
return (list(out=df))
}