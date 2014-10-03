#Stratified sampling using code from code from 
#rwiki.sciview.org

random_sample = function (dt, sample_size) {
   # determine the number of records in the data frame
   length_dt = length (dt[,1])
   # extract the sample
   dt [sample (1:length_dt, size = sample_size),]
}

get_stratified_sample = function (dt, strat_by, sample_intensity=0.1) {
   # dt contains the data
   # strat_by contains the name of the variable according to which the sample is to be stratified
   # sample intensity is proportion of data to be sampled, default = 10%
   # deterine the number of levels of factor 'strat_by' and store the number of rows relevant to the factor in table tmp
   f = factor (dt[,strat_by])
   tmp = aggregate (dt[,strat_by], by=list(f), FUN=length)
   #
   # determine the number of rows to be included within the sample for each level of the factor
   tmp$sz = round(tmp[,2] * sample_intensity, 0)
   #
   f.number = length (tmp [,1])      # determine the number of levels of the factor
   #
   # obtain the sample for the first level and store in strat_sample
   strat_sample = random_sample (subset (dt, f == tmp[1,1]), tmp[1,3])
   # obtain subsample for the remaining levels of the factor.
   if (f.number > 1) {
      for (i in 2:f.number) { 
          rs = random_sample (subset (dt, f == tmp[i,1]), tmp[i,3])
          strat_sample = rbind (strat_sample, rs)
      }
   }
   strat_sample    # returned to calling statement.
}

# ---------------------------------------------------------------------------
random_sample = function (dt, sample_size) {
   # determine the number of records in the data frame
   length_dt = length (dt[,1])
   # extract the sample
   dt [sample (1:length_dt, size = sample_size),]
}
# ---------------------------------------------------------------------------
#The function which PA will call
SM_Stratified <- function(data,col,percent)
{
train <- get_stratified_sample(data,col,sample_intensity=percent)
data$label <- "TEST"
data[row.names(train),"label"] <- "TRAIN"
return(list(out=data))
}