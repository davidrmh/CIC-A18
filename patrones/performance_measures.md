# Performance measures

## Classification
### Accuracy
Accuracy is defined as the number of items categorized correctly divided by the total number of items

In a confusion matrix accuracy is calculated as the sum of every element in the diagonal (trace) divided by the sum of every entry in the matrix.

$$Accuracy = \dfrac{\sum_{i} M_{ii}}{\sum_{i}\sum_{j}M_{ij}}$$

**Caution:** Accuracy is not a good performance measure when the dataset is unbalanced, for example when we are classifying a rare event or the events have unbalanced costs, e.g. benign/malign tumor.

### Precision
Precision is what fraction of the items the classifier flags as being in the class actually are in the class.

In a confusion matrix, precision for **predicted class** $i$ can be calculated by taking the entry $M_{ii}$ and dividing it by the sum of the elements in that same column $i$

$$Precision(i) = \dfrac{M_{ii}}{\sum_{j} M_{ji}}$$

Precision for class $i$ tell us how often we are correct, that is, prediction is a measure of confirmation.

### Recall
Recall refers to what fraction of the things that are in a class are detected by the classifier.

In a confusion matrix, recall for **class** $i$ can be calculated by taking the entry $M_{ii}$ and dividing it by the sum of the elements across that same row.

$$Recall(i) = \dfrac{M_{ii}}{\sum_{j} M_{ij}}$$

Recall is a measure of utility, it tell us how much the classifier finds of what there's actually is to find.

### F1 score
F1 score is a measure that combines precision and recall, if either of these two measures is small then F1 is small too.

$$F1 = \dfrac{2 \times precision \times recall}{precision + recall}$$
