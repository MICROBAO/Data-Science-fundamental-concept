#Table = read.csv( file = "", header=TRUE )
Table = read.csv( file = "stdin", header=TRUE )
library(MASS)

X = data.matrix( Table[, 1:(ncol(Table)-1) ])
classifications = Table[, ncol(Table) ]
n = nrow(X)
p = ncol(X)

y = unclass(classifications)

LDA.model <- lda(y ~ X)
LDAclassification = function(Model,X)  {
  predict(Model, as.data.frame(X))$class
}
LDA.classifications = LDAclassification(LDA.model, X)
LDA.disagreements = (1:nrow(X))[ LDA.classifications != y ]
LDA.confusion.matrix = table( LDA.classifications, y )


print_matrix_for_Mooshak = function(Matrix) {
  for (i in 1:nrow(Matrix)) {
    cat( Matrix[i,], "\n" )  # print each row as a sequence
  }
}


QDA.model <- qda(y ~ X)

QDAclassification = function(Model,X)  {
  predict(Model,as.data.frame(X))$class
}
QDA.classifications = QDAclassification(QDA.model, X)
QDA.disagreements = (1:nrow(X))[ QDAclassification(QDA.model, X) != y ]

QDA.confusion.matrix = table( QDA.classifications, y )


print_matrix_for_Mooshak( LDA.confusion.matrix )
print_matrix_for_Mooshak( QDA.confusion.matrix )

