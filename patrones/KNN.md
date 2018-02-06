# K-NN (K-Nearest Neighbors) algorithm

### Pros
+ It's a **supervised* *** classification algorithm.
+ High accuracy, insensitive to outliers, no assumptions about the data.
+ Works with both numeric and nominal values.

### Cons
+ Computationally expensive (requires a lot of memory).
+ There is no clear way for determining the value of *k* (rule of thumb): $$$ k \leq 20$$$
+ You can't see underlying structure of the data.

### How it works
Using our training data set (with known labels) and a new example that we want to classify we compare this new example with every existing data in our  training data set. Then we look at the top *k* most similar pieces and assign the label with majority vote from the *k* similar pieces of data.

### Pseudo-code

```
D-> Data set
x -> New input
k-> First k neighbors
f->Distance function
dist-> Object with distances
majority->

for row in D:
	dist[row] = f(x,D[i])
#Sorts distances in increasing order	
dist = sort.increasing(dist)

majority = dist[1:k].labelsCountMax
```

### Tips for data preparation
+ Normalize values between 0 to 1 or -1  to 1. For normalizing in a range in 0 to 1 one can use the following transformation:
$$$ normValue = \frac{originalValue - min}{max - min}$$$
