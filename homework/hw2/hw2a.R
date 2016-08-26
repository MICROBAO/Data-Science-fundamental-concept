#Table = data.matrix(read.csv( file="", header=TRUE ))

Table = data.matrix(read.csv( file = 'stdin', header=TRUE ))
X = Table[, 1:(ncol(Table)-1) ]
classifications = Table[, ncol(Table) ]
k = length(unique(classifications))

y = unclass(classifications)         # convert the class values into numeric indices

n = nrow(X)
p = ncol(X)

distance_value = matrix(0, nrow=n, ncol=k)

#calculate the means std cov of X
#means  = apply(X, 2, mean)
#sigmas = apply(X, 2, sd)
#Sigma = cov(X)  # covariance matrix
#detSigma = det(Sigma)

#SigmaInverse = solve(Sigma) 

g = function(xvec, meanvec, inverseCovMatrix) {
  1 / sqrt(2*pi)^2 / sqrt(detSigma) *
    exp( -1/2 * ( t(xvec-meanvec) %*% inverseCovMatrix %*% (xvec-meanvec) )[1,1] )
}

for (j in 1:k){
  Data_for_j_th_class = subset(X, (classifications==j) )
  mean_vector = matrix( apply(Data_for_j_th_class, 2, mean), nrow=p, ncol=1 )  # column vector
  cov_matrix = cov(Data_for_j_th_class)
  covinverse=solve(cov_matrix)
  

  
  for (i in 1:n){
    distance_value[i,j]=t(X[i,]-mean_vector)%*%covinverse%*%(X[i,]-mean_vector)
  }
}



for (i in 1:n){
  if (which.min(distance_value[i,])!=classifications[i])
  {
    cat(c(i, which.min(distance_value[i,]),classifications[i],"\n"))
    # cat(sprintf("%d %d %d\n", i, which.min(distance_value[i,]),classifications[i]))
}

}
