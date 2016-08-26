#   With k-means clustering, people often decide how many clusters is "best" by computing some statistic
#   that measures sums of squares of distances of objects from their centroids.  The concept is
#   that a good choice for the number of clusters k should minimize this statistic.
#   
#   This is exactly what the "gap statistic" is used for:
#   
#      T. Hastie, G. Walther, R. Tibshirani,
#      Estimating the number of clusters in a data set via the Gap statistic,
#      J.R. Stat Soc B, 2001, 63:2, pp.411-423.
#      http://web.stanford.edu/~hastie/Papers/gap.pdf
#   
#   The Gap statistic is one-half the sum over all clusters of:
#      (within-cluster sums-of-squares  divided by  the number of items in the cluster).
#   
#   With the iris dataset, for example, the R code below finds that the best number of clusters is k=3.

data(iris)
X = iris[,1:4]
gap_statistic = function(kM) { clusterSizes = table(kM$cluster); sum( kM$withinss / clusterSizes )/2 }

gap_values = rep(1,10)
for (k in 2:10) { kM = kmeans(X,k, nstart=10); inertia_values[k] = gap_statistic(kM) }

round(inertia_values, 3)

#   1.000 0.907 0.787 0.788 0.791 0.794 0.826 0.848 0.859 0.875
# 
#  So the gap statistic for the iris dataset has a local minimum at k=3 clusters.

