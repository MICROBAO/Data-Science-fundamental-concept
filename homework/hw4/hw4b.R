
Table = data.matrix(read.csv( file = "stdin", header=TRUE ))
#Table = data.matrix(read.csv( file = "mtcars.csv", header=TRUE ))

y = Table[,ncol(Table)]


X_ = subset(Table, select = -ncol(Table))
X_minus_Xbar = scale(X_, center=TRUE, scale=FALSE)

C_inverse = solve( cov(X_) )
Leverage = diag( (X_minus_Xbar) %*% C_inverse %*% t(X_minus_Xbar) )  ## very wasteful -- we just want the diagonal

i = which( Leverage == max(Leverage) )

Xt_X = t(X_) %*% X_
Xt_y = t(X_) %*% y


n = nrow(X_)
p = ncol(X_)

w = solve( Xt_X, Xt_y )
y_predict=X_%*%w

w_cd=list()
CookDistance=matrix(0,nrow=n,ncol=1)
for(e in 1:n){
  X_cd= X_
  y_cd=y
  X_cd[e,]=0
  y_cd[e]=0
  InverseX_Xt=solve(t(X_cd)%*%X_cd)
  aa=InverseX_Xt%*%t(X_cd)
  w_cd=aa%*%y_cd
  cook=X_%*%(w-w_cd)
  CookDistance[e]=t(cook)%*%cook
  }




j = which( CookDistance == max(CookDistance) )


cat(i, "\n")
cat(j, "\n")

