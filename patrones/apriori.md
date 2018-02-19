# Apriori algorithm

This algorithm is used for *association analysis* or *association rule learning*, e.g. purchases made in e-commerce sites and the association between the purchased items.

*Association analysis* is used for finding relationships in a dataset. These relationships can take two forms:

* *Frequent items sets:* collection of items frequently occurring together.

* *Association rules:* These rules suggest a strong relationship between two items.

In order to understand *association analysis* first we need to define:

* What's and interesting relationship?

* What's interesting?

* What's frequent?

The above questions can be answered using the folliwng concepts.

**Definition (support of itemset):** The *support* of an itemset $\{A,B,\ldots\}$ is the percentage of the dataset that contains this itemset.

**Definition (confidence for an association rule):** The confidence for an association rule $\{A\} \rightarrow \{B\}$ is defined as
$$
\dfrac{support\left( \{A,B\}\right)}{support\left( \{A\}\right)}
$$
This means that confidence is the percentage of times our rule is correct.

## Apriori principle

This principle is used to reduce the number of possible interesting itemsets (a dataset containing $N$ possible items can generate $2^{N}-1$ possible itemsets which is computationally expensive calculate the support of each one of them) and states that **if an itemset is frequent, then all of its subsets are frequent** or equivalently **if an itemset is not frequent, then its supersets are not frequent**.

**Theorem (Apriori principle)** Let $A$ and $B$ be itemsets such that $A \subset B$, then $support(B) < support(A)$.

**Proof** This follows since for every observation of itemset $B$ we have to count one observation of itemset $A$, then we have counted itemset $A$ at least as many times as itemset $B$.

Now as itemset $B$ has more elements than itemset $A$, it follows that not every count of itemset $A$ is a count for itemset $B$ and hence $support(B) < support(A) \,\,\,\square$

Notice that for calculating the support the latter statement is more useful than the former one since we start calculating the support of a "small" itemset instead of the support of a "big" one, thus reducing the exponential growth.

### Finding frequent itemsets

In order to find frequent itemsets we use the support as a measure of frequency and we set a threshold level. If the support of an itemset is above this threshold then this itemset is combined with the remaining ones to make bigger itemsets, in other case it is discarded.

Naturally the process begins with itemsets containing only one element and finishes when no itemset passes the threshold requirement.

### Finding association rules
In order to find association rules we start with a frequent itemset and use confidence as a measure of association.

Again we set a threshold as the minimum required level of confidence and notice that if the confidence of a rule is less than the threshold then confidence of subsets of that rule will be less too.

Using the above we can reduce the number of rules to be evaluated. Starting with rules with one item on the right-hand side and then using the ones fulfilling the threshold for creating rules with two elements on the right-hand side and so on.

## Pros
* Easy to code

## Cons
* Slow on large datasets

## Data
* Numeric and nominal data.

## Possible application

* Finding association rules among stocks. Each day could be a transaction and the itemsets could be $\{up-stock_{1},down-stock_{2},...,up-stock_{k}
\}$.
  Where $up(down)-stock_{i}$ means that the price change from moment $t-1$ to $t$ was bigger(lower) than a certain threshold.
