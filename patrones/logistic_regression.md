# Logistic regression

This is a classification method based on optimizing an objective function (namely a sort of likelihood function) in order to find the best weights to use.

It uses *sigmoid* function in order to determine the class label for the data examined

$$
\sigma(z) = \dfrac{1}{1 + e^{-z}}
$$
where $z = w_{0} + w_{1}x_{1} + \ldots + w_{n}x_{n}$
and the $w_{i}$ are found by using gradient ascent or stochastic gradient ascent methods.

The classifier creates **linear decision** boundaries
### Pros
 + Computationally inexpensive and easy to implement.
 + Gives a probability of

### Cons
+ Low accuracy due to underfitting.

### Data
 + Works with numerical values.
 + Data contains only two class labels **0** and **1**.

### Stochastic gradient ascent
(deterministic) gradient ascent method uses all the data in order to have the weights updated once (all-at-once method), instead, stochastic gradient ascent updates those same weights using only one instance at a time (an example of online learning algorithm)

#### Trade-off
+ Stochastic gradient ascent is faster than regular gradient ascent but the classifications might be less accurate if the former method is not implemented carefully, e.g. without changing the learning rate dynamically or without randomly selecting the instance to be used in the update step.
