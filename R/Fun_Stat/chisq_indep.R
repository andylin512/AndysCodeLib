chisq_indep <- function(x, alpha = 0.05){

# This is a function to help you calculate the indepence-Chisq test.
# a input should be a list object
  #    list(Cabin = c(Yes = 299, No = 280), Steerage = c(Yes = 186, No = 526))
  if (!is.list(x)){
    cat('x should be a list object\n')
  }else{
    df <- as.data.frame(x)
    row <- apply(df, 1, sum); row <- row / sum(row); row <- as.matrix(row, ncol =1)
    col <- apply(df,2, sum); col <- as.matrix(col, ncol = 1)
    expected_mat <- row%*%t(col)
    chisq_stat <- sum((df - expected_mat)^2 / expected_mat)
    setNames(chisq_stat, 'Chi-squared Statistics')
    DF <- (max(nrow(df) - 1, 1))*(max(ncol(df) - 1))
    p_value <- 1 - pchisq(chisq_stat, DF)
    
    if(p_value < alpha){
      cat('Reject NULL Hypo, with p value: ', p_value, "\n")

    }else{
      cat('Retain NULL Hypo, with p value: ', p_value, "\n")
    }
    return(chisq_stat)
  }

}