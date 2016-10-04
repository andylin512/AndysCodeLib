F_test <- function(dat_list, alpha = 0.05){
  # A Fucntion created to calculate the F test. Pass a list including data for different groups
  # Will output the F statistics, critical value under the alpha level, DF1 and DF2.
  df1 <- length(names(dat_list)) -1
  df2 <- 0
  for(i in seq(df1+1)){
    df2 <- df2 + length(dat_list[[i]]) #no easy way to count the length of the list
  }
  df2 <- df2 - df1 -1 
  
  grand_mean <- mean(unlist(dat_list))
  
  # calculate the Between group variance:
  bet_var <- 0
  for (i in seq(df1 + 1)){
    bet_var <- bet_var + length(dat_list[[i]]) * (mean(dat_list[[i]]) - grand_mean)^2

  }
  bet_var <- bet_var / df1
  
  # calculate the Within group varaince:
  with_var <- sum((unlist(lapply(dat_list, length))-1) * unlist(lapply(dat_list, var))) / df2
  
  F_stat <- bet_var / with_var
  
  if (F_stat > qf(1 - alpha, df1, df2)){
    cat('Reject Null Hypo, with alpha level--', alpha, '\n')
  }else{
    cat('Fail to reject Null Hypo, with alpha level--', alpha, '\n')
  }
  
  cat('\n')
  return(c('F.Stat' = F_stat, 'F.CriValue' = qf(1 - alpha, df1, df2), 'DF1' = as.integer(df1),
           'DF2' = as.integer(df2)))
  }
