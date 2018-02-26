# Support vector machines

Is a supervised learning method used mainly for binary classification problems.

It involves convex optimization (and hence global optimum) but unlike logistic regression it doesn't provides posterior probabilities.

## Data
The dataset consists of $N$ vectors, $\mathbf{x_{1},\ldots,x_{N}}$ with corresponding target values $t_{1},\ldots,t_{N} \in \{-1,1\}$.

The points will be classified as $1 \text{ or } -1$ according to whether the function
$$y(\mathbf{x})=\mathbf{w^{T}\phi(x)}+b$$
is positive or negative respectively, here $\mathbf{\phi(x)}$ is a feature-space transformation.

Our purpose is finding the separating hyperplane that best classifies our data in some sense, namely, we're going to find the best separating hyperplane by maximizing the *margin*, that is, the minimum distance from a data point to the hyperplane, doing so can help us improve generalization in our predictions.

## Problem formulation

**Note** The following formulation assumes data is *linearly separable*.

Let us first observe if a data point, $\mathbf{x_{n}}$, is correctly classified, then $t_{n}y(x_{n})>0$.

Now let's consider a hyperplane defined by $y(\mathbf{x})=0$ (or $\mathbf{w^T}\phi(\mathbf{x})=-b$) and a point $\mathbf{x}$, for sake of simplicity let's consider the case when $y(\mathbf{x})=\mathbf{w^{T}x}+b$

The point $\mathbf{x}$ can be expressed as a linear combination of $\mathbf{w}$ and $\mathbf{w_{*}}$, where $\mathbf{w_{*}}$ is orthogonal to $\mathbf{w}$

$$\mathbf{x}=s\mathbf{w}+t\mathbf{w_{*}}$$

From this equation it follows that

$$s=\dfrac{\mathbf{w^T}\mathbf{x}}{||\mathbf{w}||^2}$$

and thus the projection of $\mathbf{x}$ onto $\mathbf{w}$ is given by

$$\mathbf{Proj_{w}x}=\dfrac{\mathbf{w^T}\mathbf{x}}{||\mathbf{w}||^2} \mathbf{w}$$

Finally the distance from $\mathbf{x}$ to the hyperplane $y(\mathbf{x})=0$ is given by the norm of the projection, i.e.,

$$\dfrac{|\mathbf{w^T}\mathbf{x}|}{||\mathbf{w}||}=\dfrac{|b|}{||\mathbf{w}||}$$

in our more general case the distance of a point $\mathbf{x_{n}}$ to the decision surface is

$$\dfrac{t_{n}(\mathbf{w^T}\mathbf{\phi (x_{n})} + b)}{||\mathbf{w}||}$$

**Observation** We're assuming *linearly separable* data, so there must exist a pair $(\mathbf{w},b)$ such that $t_{n}(\mathbf{w^T}\mathbf{\phi(x_{n})} + b)>0$ and thus the distance above is well defined, without this assumption an absolute value would be required in order to have positive distances.

Since we are maximizing the *margin* we are looking for a pair $(\mathbf{w^*},b^*)$ such that

$$(\mathbf{w^*},b^*)=\stackrel{arg-max}{\mathbf{w},b} \left\{ \dfrac{1}{||\mathbf{w}||} \min_{n} \left\{t_{n}(\mathbf{w^T}\mathbf{\phi (x_{n})} + b) \right\} \right\}$$

By observing the formula for the distance to the hyperplane we can conclude that rescaling $\mathbf{w}$ and $b$ to $\kappa\mathbf{w}$, $\kappa b$ preserves distances so we can set

$$t_{n}(\mathbf{w^T}\mathbf{\phi (x_{n})} + b)=1$$

for the points that are closest to the surface (*support vectors*) and for the rest of the points we'll have the condition

$$t_{n}(\mathbf{w^T}\mathbf{\phi (x_{n})} + b)\geq1\,\,\,\,n=1,\ldots,N.$$

Thus our problem can be expressed as

$$\stackrel{arg-max}{\mathbf{w},b} \left\{ \dfrac{1}{||\mathbf{w}||} \right\}$$

constrained to

$$t_{n}(\mathbf{w^T}\mathbf{\phi (x_{n})} + b)\geq1\,\,\,\,n=1,\ldots,N.$$

Which is equivalent to solving the minimization problem

$$\stackrel{arg-max}{\mathbf{w},b}\frac{1}{2} ||\mathbf{w}||^2$$

constrained to

$$t_{n}(\mathbf{w^T}\mathbf{\phi (x_{n})} + b)\geq1\,\,\,\,n=1,\ldots,N.$$

a *quadratic optimization* problem.
