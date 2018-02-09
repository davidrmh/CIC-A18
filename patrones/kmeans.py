from numpy import *

'''
1. Load dataSet,classLabel using file2matrix
2. Split dataSet
2. Normalize trainDataSet and testSet
3. Create initial centroids
4. getClusters
5. confusionMatrix

Perfomance:

Accuracy 0.52
precision [ 0.3253012 ,  0.484375  ,  0.88679245]
recall [ 0.44262295,  0.5       ,  0.61038961]
f1 [ 0.375     ,  0.49206349,  0.72307692]
'''

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
    centroidsLabels=getRepresentativeLabels(clusters)
    return clusters,centroids,centroidsLabels

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

def getRepresentativeLabels(clusters):
    k=len(clusters)
    centroidsLabels=[]
    for i in range(k):
        aux=unique(array(clusters[i]['classLabels']),return_counts=True)
        centroidsLabels.append(aux[0][aux[1].argmax()])
    return array(centroidsLabels)

def euclidClassifier(dataPoint,centroids,centroidsLabels):
    k=centroids.shape[0]
    distances=[]

    for i in range(k):
        distances.append(distEclud(dataPoint,centroids[i]))
    argMin=array(distances).argmin()
    return centroidsLabels[argMin]


def confusionMatrix(testDataSet,testLabels,centroids,centroidsLabels):
    #Creates the confusion matrix
    #Rows are the true values
    #Columns are the predicted ones
    m=unique(centroidsLabels).shape[0]
    confMat=zeros((m,m))

    #Creates a dictionary with the indices
    #this dictionary will be used for filling the confusion matrix
    dicIndex={}
    uniqueLabels=unique(centroidsLabels)
    for i in range(m):
        dicIndex[uniqueLabels[i]]=i

    #Starts the predictions
    for i in range(testDataSet.shape[0]):
        prediction=euclidClassifier(testDataSet[i],centroids,centroidsLabels)
        row=dicIndex[testLabels[i]]
        column=dicIndex[prediction]
        confMat[row,column] +=1
    print 'The confusion matrix is: '
    print confMat
    return confMat

def performanceMeasures(confMat):
    m=confMat.shape[0]
    accuracy=confMat.trace()/confMat.sum()
    precision=zeros(m)
    recall=zeros(m)

    for i in range(m):
        precision[i] = confMat[i,i]/confMat[:,i].sum()
        recall[i] = confMat[i,i]/confMat[i,:].sum()
    f1 = 2*precision*recall/(precision + recall)

    return accuracy,precision,recall,f1

def splitDataSet(dataSet,classLabelVector,testPercentage=0.2):
    totalRows=dataSet.shape[0]
    testRows=int(totalRows*testPercentage)
    trainingRows=totalRows-testRows
    trainingDataSet=dataSet[0:trainingRows]
    testDataSet=dataSet[trainingRows:]
    trainingLabels=classLabelVector[0:trainingRows]
    testLabels=classLabelVector[trainingRows:]
    return trainingDataSet,testDataSet,trainingLabels,testLabels

def autNorm(dataSet):
    minVals = dataSet.min(0)
    maxVals = dataSet.max(0)
    ranges = maxVals - minVals
    normDataSet = zeros(shape(dataSet))
    m=dataSet.shape[0]
    normDataSet=dataSet-tile(minVals,(m,1))
    normDataSet = normDataSet / tile(ranges,(m,1))
    return normDataSet

def file2matrix(filename):
    fr=open(filename)
    numberOfLines = len(fr.readlines())
    returnMat = zeros((numberOfLines,3)) # 3 Corresponds to the number of features
    classLabelVector=[]
    fr.close()
    fr=open(filename)
    index=0
    for line in fr.readlines():
        line=line.strip()
        listFromLine=line.split('\t')
        returnMat[index,:]=listFromLine[0:3]
        classLabelVector.append(listFromLine[-1]) #Before it did an int casting
        index +=1
    return returnMat,classLabelVector
