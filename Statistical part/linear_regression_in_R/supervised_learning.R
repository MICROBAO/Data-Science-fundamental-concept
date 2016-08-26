#------------------------------------------------------------------------
# training and testing a classifier with R
# see also:  example(lda)
#------------------------------------------------------------------------

data(iris)
str(iris)

n <- dim(iris)[1]
p <- dim(iris)[2]

table(iris$Species)

attach(iris)    # attach the iris namespace to our namespace

#------------------------------------------------------------------------
#   generate a training set, and create a model on the training set
#------------------------------------------------------------------------

iris_ids <- (1:n)

training_set <- sample(iris_ids, 75)     # Generate a random sample of size 75
training_set

testing_set  <- iris_ids[-training_set]  # The set complement of training_set
testing_set


table(Species)                  # equivalent to the table() command above
table(Species[training_set])    # Tabulate species for the training set
table(Species[testing_set])     # Tabulate species for the testing set

#------------------------------------------------------------------------
#   generate a model with the LDA package
#------------------------------------------------------------------------

#  install.packages("MASS")  # install the MASS package if you don't have it

library(MASS)  #  get the lda() function, and the parcoord() function

#  plot the data as a parallel coordinates plot

parcoord(iris[,1:4],  col=c("red", "green3", "blue")[unclass(iris$Species)])

#  fit an LDA model to the data

ldaModel <- lda( Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
                 subset = training_set )
ldaModel

#------------------------------------------------------------------------
# find out how well the model predicts the remaining data (testing_set)
#------------------------------------------------------------------------

predictions <- predict( ldaModel, iris[ testing_set ,] )
predictions$class

incorrect.prediction  <-  (predictions$class != Species[testing_set])
testing_set[ incorrect.prediction ]          # ids  for which prediction was incorrect
iris[ testing_set[incorrect.prediction], ]   # data for which prediction was incorrect

confusion_matrix <- table( Species[testing_set], predictions$class )
confusion_matrix

m <- length(testing_set)
m

accuracy <- (m - sum(incorrect.prediction))/m
accuracy

# 
# # equivalent to the above, but using "complement of training_set"
# 
# predictions <- predict( ldaModel, iris[ -training_set ,] )   #
# incorrect.prediction  <-  (predictions$class != Species[ -training_set ])
# 

#-----------------------------------------------------------------------
#  test the model on the entire data set
#------------------------------------------------------------------------

predictions <- predict( ldaModel, iris )
predictions$class

incorrect.prediction  <-  (predictions$class != Species)
iris_ids[ incorrect.prediction ] # ids  for which prediction was incorrect
iris[ incorrect.prediction , ]   # data for which prediction was incorrect

accuracy <- (n - sum(incorrect.prediction))/n
accuracy

confusion_matrix <- table( Species, predictions$class )
confusion_matrix

#------------------------------------------------------------------------
#  detach the iris namespace
#------------------------------------------------------------------------

detach(iris)  # remove iris namespace

