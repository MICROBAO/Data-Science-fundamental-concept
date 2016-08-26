Table = data.matrix(read.csv( file = "stdin", header=TRUE ))
#Table = data.matrix(read.csv( file = "USArrests.csv", header=TRUE ))
#Table = data.matrix(read.csv( file = "mtcars.csv", header=TRUE ))

X= Table[, 1:ncol(Table) ]
PCAsvd = svd( cor(X) )
PrincipalComponents = PCAsvd$u
Eigenvalues = PCAsvd$d   

PercentageOfVarianceExplained = Eigenvalues / sum(Eigenvalues)
CumulativePercentageOfVarianceExplained = cumsum( PercentageOfVarianceExplained )

ruleone=which(CumulativePercentageOfVarianceExplained>=0.7)
ruletwo=which(Eigenvalues >= 1)

ruletwonum=length(ruletwo)

cat(ruleone[1],"\n")

cat(ruletwonum,"\n")
