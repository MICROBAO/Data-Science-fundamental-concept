

Table = data.matrix(read.csv( file = "stdin", header=TRUE ))
#Table = data.matrix(read.csv( file = "mtcars.csv", header=TRUE ))

y = Table[,ncol(Table)]


X_ = as.matrix( subset(Table, select = -ncol(Table)))
Xt_X = t(X_) %*% X_
Xt_y = t(X_) %*% y

minimum_lambda_value = 0
maximum_lambda_value = 100 ##  norm( Xt_X ) / 100000
number_of_lambda_values = 101

lambda_values = seq( minimum_lambda_value, maximum_lambda_value, length = number_of_lambda_values )

n = nrow(X_)
p = ncol(X_)
coefficient_values_for_each_lambda = matrix(0, nrow=number_of_lambda_values, ncol=p)
colnames(coefficient_values_for_each_lambda) = colnames(X_)
I_p = diag(rep(1,p))

for (i in 1:number_of_lambda_values) {
  w = solve(  (Xt_X  +  I_p * lambda_values[i]),  Xt_y )
  coefficient_values_for_each_lambda[i,] = w
}


d=matrix(0,ncol=1,nrow=p)
for (i in 1:p){
  d[i]=max(coefficient_values_for_each_lambda[,i])-min(coefficient_values_for_each_lambda[,i])
  
}

maxco=which.max(d)
cat(maxco,'\n')

