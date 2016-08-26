


library(MASS)

Table = data.matrix(read.csv( file = 'stdin', header=TRUE ))


Distribution = c( "normal", "t", "chi-squared", "lognormal", "exponential", "logistic")
Distribution_can_have_negative_values = c( TRUE, TRUE, FALSE, FALSE, FALSE, TRUE )

for (j in 1:ncol(Table)) {
  Dataset = Table[,j]        #  j-th dataset = the j-th column of the table
  Dataset_is_nonnegative = !any( Dataset < 0 )
  
  
  likelihood=list()
  parameter=list()
  name=list()
  for (i in 1:length(Distribution)) {
    dist_name = Distribution[i]
    if (Distribution_can_have_negative_values[i] || Dataset_is_nonnegative) {
      # don't try to fit a nonnegative distribution to data that is negative
      
      if (dist_name == "chi-squared") {
        # fitdistr requires special handling of chi-squared
        fit = suppressWarnings( fitdistr( Dataset, dist_name,
                                          list(df=round(mean(Dataset))), method="BFGS" ) )
      } else {
        fit = suppressWarnings( fitdistr( Dataset, dist_name ) )
      }
      
      # "fit" is the object returned by fitdistr, describing the fit
      
      fitted_parameters = fit$estimate
      log_likelihood = fit$loglik
      
      likelihood[i]=log_likelihood
      
      name[i]=Distribution[i]
      parameter_value_string = paste(round(fitted_parameters), collapse=" ")
      parameter[i]=parameter_value_string
      # we round the parameter values so that they are integers.
      
      # This is what the output is supposed to look like:
      # cat(sprintf("%s %s\n", dist_name, parameter_value_string))  
      
      # To show how good the fit is, we also print the log-likelihood here
      # cat(sprintf("log-likelihood = %f\n", log_likelihood))
      
    }
    else{
      likelihood[i]=-1111111
      parameter[i]=" "
      name[i]=" "
      
    }
    
  }
  
  a=unlist(likelihood)
  b=unlist(parameter)
  c=unlist(name)
  
  cat(sprintf("%s %s\n", c[which.max(a)], b[which.max(a)]))
  
}