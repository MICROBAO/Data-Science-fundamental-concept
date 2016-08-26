Table = data.matrix(read.csv( file = "stdin", header=TRUE ))
#Table = data.matrix(read.csv( file = "arrests.csv", header=TRUE ))
#Table = data.matrix(read.csv( file = "mtcars.csv", header=TRUE ))

X= Table[, 1:ncol(Table) ]
PCAsvd = svd( cor(X) )
PrincipalComponents = PCAsvd$u
Eigenvalues = PCAsvd$d   

ComponentEntryRanks = apply( -PrincipalComponents^2, 2, rank )
SortedSquaredComponents = apply(PrincipalComponents^2, 2, function(x) sort(x, decreasing=TRUE))
CumulativeSortedSquaredComponents = apply( SortedSquaredComponents, 2, cumsum)

loading = matrix(0, nrow=ncol(Table), ncol=1)

for (i in 1:ncol(Table)){
  threshold=which(CumulativeSortedSquaredComponents[,i]>0.7)
  loading[i]=threshold[1]
}

cat(loading,sep=" ","\n")
