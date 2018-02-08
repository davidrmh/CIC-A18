# K-means

This is an **unsupervised** algorithm for finding clusters. It works by finding *k* clusters each one of them having a center (the so called centroids) which turns out to be the mean of the values in the corresponding cluster.

### Pros
+ Easy to implement

### Cons
+ Can fall into a local minima.
+ Slow on large data sets.

### Pseudo-code
~~~~
1. Initialize k centroids randomly
2. Repeat until stop condition is met:
    2.1 for every point in the dataset
        find the distance to each centroid
    2.2 assign to the cluster with the
        lowest distance   
    2.3 for every cluster calculate the
        mean and define the new centroids
        using these values    
~~~~

### Data details
Since we are using a distance function *k-means* algorithm works with numeric values, thus we have to turn nominal values into numbers.

Also normalizing the data can be useful when the units in the features vary widely.
