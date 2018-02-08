from numpy import *
import kNN

def distEclud(vecA,vecB):
    return sqrt(sum(power(vecA-vecB,2)))

def randCent(dataSet,k=3):
    #Number of columns
    n = shape(dataSet)[1]
    centroids=mat(zeros((k,n)))

    #Creates the centroids by uniformly
    #selecting the number for each feature

    for j in range(n):
        minJ = min(dataSet[:,j])
        rangeJ = float(max(dataSet[:,j]) - minJ)
        centroids[:,j] = minJ + rangeJ*random.rand(k,1)
    return centroids

def normalize(dataSet):
    minVals = dataSet.min(0)
    maxVals = dataSet.max(0)
    ranges = maxVals - minVals
    normDataSet = zeros(shape(dataSet))
    m=dataSet.shape[0]
    normDataSet=dataSet-tile(minVals,(m,1))
    normDataSet = normDataSet / tile(ranges,(m,1))
    return array(normDataSet),ranges,minVals
