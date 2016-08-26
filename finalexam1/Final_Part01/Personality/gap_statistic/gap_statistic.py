
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
#   With the iris dataset, for example, the scikit-learn code below finds that the best number of clusters is k=3.

from sklearn import cluster
from sklearn.datasets import load_iris

iris = load_iris()
X = iris.data
y = iris.target

def gap_statistic(X,k):
    centroids, labels, inertia = cluster.k_means(X, n_clusters=k)
    cluster_sizes = [ labels.tolist().count(j) for j in range(k) ]
    n, p = X.shape
    gap_value = sum( [ sum((X[i,:] - centroids[labels[i]])**2)/cluster_sizes[labels[i]] for i in range(n)] ) / 2.0
    return(gap_value)

best_gap = 1
best_k = 1
for k in range(2, 10):
    gap_value = gap_statistic(X,k)
    print( 'iris dataset gap value for %d clusters is %6.3f' % (k, gap_value) )
    if best_gap > gap_value:
       best_gap = gap_value
       best_k = k

print( 'best iris dataset gap statistic value is %6.3f, for %d clusters' % (best_gap, best_k) )

# iris dataset gap value for 2 clusters is  0.908
# iris dataset gap value for 3 clusters is  0.788
# iris dataset gap value for 4 clusters is  0.789
# iris dataset gap value for 5 clusters is  0.792
# iris dataset gap value for 6 clusters is  0.791
# iris dataset gap value for 7 clusters is  0.834
# iris dataset gap value for 8 clusters is  0.796
# iris dataset gap value for 9 clusters is  0.814
# best iris dataset gap value is  0.788, for 3 clusters

