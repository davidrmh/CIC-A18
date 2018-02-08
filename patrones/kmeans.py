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
    normDataSet=array(normDataSet)
    return normDataSet

def getClusters(dataSet,centroids,classLabels,niter=500):
    rows=dataSet.shape[0]
    k=centroids.shape[0]

    for iter in range(niter):
        clusters = initializeClusters(k)
        for row in range(rows):
            distances=[]
            for i in range(k):
                distances.append(distEclud(dataSet[row],centroids[i]))
            argMin=array(distances).argmin()
            clusters[argMin]['classLabels'].append(classLabels[row])
            clusters[argMin]['elements'].append(dataSet[row])
        centroids=recalculateCentroids(clusters,k)
    return clusters,centroids

def recalculateCentroids(clusters,k):
    n=array(clusters[0]['elements']).shape[1]
    centroids=mat(zeros((k,n)))

    #Calculates the mean per feature
    for i in range(k):
        centroids[i,:]=array(clusters[i]['elements']).mean(0)
    return centroids

def initializeClusters(k):
    #Initializes the clusters
    clusters=[]
    for j in range(k):
        clusters.append({})
        clusters[j]['classLabels']=[]
        clusters[j]['elements']=[]
    return clusters
