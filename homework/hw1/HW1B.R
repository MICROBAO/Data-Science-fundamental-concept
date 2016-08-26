dchi = function(x,nu) dchisq(x^2, nu) * (2*x)
Table = data.matrix(read.csv( file = 'stdin', header=TRUE ))

for (i in 1:ncol(Table)){
  Dataset=Table[,i]
  log_likelihood = function(theta) sum( log(dchi(Dataset, theta)) )
  initial_value_for_nu = 2
  negative_log_likelihood = function(theta) -log_likelihood(theta)

# optim always _minimizes_ a function,
#   so to find the MLE we minimize the negative log likelihood:

  output_of_optimization =
  optim( initial_value_for_nu, negative_log_likelihood, method="BFGS" )

  #print(output_of_optimization)

  minimum_negative_log_likelihood_value = output_of_optimization$value

  MLE_for_nu = round( output_of_optimization$par )

  #print(MLE_for_nu)
  cat(sprintf("chi %d\n", MLE_for_nu ))
}
