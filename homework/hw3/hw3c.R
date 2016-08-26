Table = data.matrix(read.csv( file = "stdin", header=TRUE ))
#Table = data.matrix(read.csv( file = "arrests.csv", header=TRUE ))
#Table = data.matrix(read.csv( file = "mtcars.csv", header=TRUE ))

X= Table[, 1:ncol(Table) ]
PCAsvd = svd( cor(X) )
PrincipalComponents = PCAsvd$u
Eigenvalues = PCAsvd$d   

U = PrincipalComponents
Z = scale(X, center=TRUE, scale=FALSE) %*% U

a = apply(Z^2, 1, sum )
i = which( a == max(a) )

b = Z^2%*%(1/Eigenvalues)
j = which( b == max(b) )

c=Z^2%*%Eigenvalues
t = which( c == max(c) )


cat(i,"\n")
cat(j,"\n")
cat(t,"\n")


