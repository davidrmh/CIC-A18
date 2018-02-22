# Machine learning interpretability
[Source](https://christophm.github.io/interpretable-ml-book/)

## What is interpretability?

Interpretability can be defined as **the degree to which a human can understand the causes behind a decision**, that is, what is causing certain behavior.

Interpretability comes with the need of understanding more about our problem. Although finding the reasons behind a decision might not necessarily be straightforward, being able to interpret the causes of it increases our chances of having better insights about the environment under study.

Even more, having interpretability in a model help us to social acceptance on the decisions it makes and help us in debugging/auditing biases coming from the *incompleteness in problem formulation*.

## Ways of interpreting machine learning models

* **Global holistic interpretation** This interpretation is based on understanding the trained model from a holistic point of view (i.e. considering multiple interactions) considering the features and learned components as a whole. This interpretation seeks to answer the question *How does the trained model make predictions?*

* **Global modular level interpretation** This level of interpretation focuses on a parameters learned level, for example the coefficients on a linear regression model. At this level one seeks to ansew *How do parts of the model influence predictions?*

* **Local single prediction interpretability** At this level of interpretation one focuses on a single prediction and how the local distribution of the target variable behaves locally. Using this kind of interpretation we can answer the question *Why did the model make a specific decision for an instance*

* **Local group prediction interpretability** This kind of interpretation takes a groups of instances and pretends that is the complete dataset after this uses one of the methods mentioned above. The question to be answered is *Why did the model make specific decisions for a group of instances?* 
