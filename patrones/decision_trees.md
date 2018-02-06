# Decision trees
+ Supervised classification algorithm.
+ Unlike KNN algorithm, decision trees allow a better understanding of data.

### Pros
+ Computationally cheap.
+ Easy interpretation.
+ Can deal with irrelevant features.

### Cons
+ Overfitting risk.
+ Works only with nominal values, so continuous values must be quantized first.

The objective is splitting the data in such a way that we end up with the highest information gain.
In order to decide how to split the data *Shannon entropy* is used (another measure that we can use is *Gini's Impurity*).

Lets suppose that $X$  is a random variable that can take the value $x_i$  with probability $p(x_{i})$, then the information for this value is defined as
$$
l(x_{i}) = log_{2} p(x_{i})
$$
Entropy is defined as:
$$
H = - \sum_{i=1}^{n} p(x_i)log_2 p(x_i)
$$
Observe that when the value of some $x_{i}$ is pretty certain, that is, when $p(x_{i})$ is almost $1$ then the value of $H$ is approximately $0$, whereas the more uncertain the data is the higher the entropy.
