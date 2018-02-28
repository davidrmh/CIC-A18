# ID3 algorithm

This is a **supervised classification** algorithm based on building a decision tree in order to assign labels to the data.

The leaf nodes of the tree contain the class name whereas a non-leaf node is a decision node. In order to determine how to create these nodes the concept of entropy is needed.

One of the advantages in using decision trees as a tool for classifying objects is that the program is the one eliciting knowledge rather than an expert.

## Data
ID3 algorithm works better for data in which the features are categorical variables.

It is important to notice that classes must be defined precisely, that is, our data shouldn't contain categories such as "very hot, quite hot, cold, very cold".

Also, since this is an inductive generalization process, our data should be balanced and must contain a large (yeah I know "large" is a vague term) number of examples.

## Algorithm

ID3 algorithm uses **information gain** in order to select the features used in the decision nodes, and this gain is based on the concept of entropy

**Definition ((Shannon) entropy)**
The entropy of a discrete random variable, $X$, is defined as

$$Entropy(X) = - \sum_{i} p_{i} log_{2}(p_{i})$$

where $p_{i}$ is the probability of $X$ taking the value $i$.

**Note 1** We define $0log(0)$ as $0$.

**Note 2** For binary classification,  $0\leq Entropy \leq 1$.

**Entropy** measures the amount of information in an attribute, the higher the entropy the more mixed up the data is (impure data).

**Definition (Information gain)**
Given an example set $S$ and an attribute $A$, the **information gain** on set $S$ over attribute $A$ is defined as:

$$Gain(S,A)=Entropy(S)-\sum_{v\in \Omega_{A}}\dfrac{|S_{v}|}{|S|}Entropy(S_{v})$$

where $S_{v}$ is the subset of $S$ for which attribute $A$ has value $v$.

Thus **information gain** is the information provided about the *target function value*, given the value of some other attribute.

For each attribute, the gain is calculated and the one with highest value is used in the decision node.

## References

* Mitchell Tom, *Machine Learning*.

* Harrington Peter, *Machine Learning in Action*.
