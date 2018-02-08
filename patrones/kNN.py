from numpy import *
import operator

'''Instructions
1. file2matrix
2. autNorm (normalize the data)
3. Split data set
4. Confusion matrix
A good proportion test/training is 20/80
A good value for k is 3

With this parameters the performance
in the datingTestSet is

Accuracy 0.95
Precision [.93,.95,.96]
recall [.98,.93,.94]
f1 [.96,.94,.94]
'''


def createDataSet():
  group = array ([[1.0,1.1],[1.0,1.0],[0,0],[0,0.1]])
  labels = ['A','A','B','B']
  return group, labels

def classify0(inX,dataSet,labels,k):
  dataSetSize = dataSet.shape[0]
  diffMat = tile(inX,(dataSetSize,1)) - dataSet
  sqDiffMat = diffMat**2
  sqDistances = sqDiffMat.sum(axis=1)
  distances = sqDistances**0.5
  sortedDistIndices = distances.argsort()
  classCount = {}
  for i in range(k):
    voteLabel = labels[sortedDistIndices[i]]
    classCount[voteLabel] = classCount.get(voteLabel,0) + 1
  sortedClassCount = sorted(classCount.iteritems(),key=operator.itemgetter(1),reverse=True)
  return sortedClassCount[0][0]

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

def autNorm(dataSet):
    minVals = dataSet.min(0)
    maxVals = dataSet.max(0)
    ranges = maxVals - minVals
    normDataSet = zeros(shape(dataSet))
    m=dataSet.shape[0]
    normDataSet=dataSet-tile(minVals,(m,1))
    normDataSet = normDataSet / tile(ranges,(m,1))
    return normDataSet,ranges,minVals

def splitDataSet(dataSet,classLabelVector,testPercentage=0.2):
    totalRows=dataSet.shape[0]
    testRows=int(totalRows*testPercentage)
    trainingRows=totalRows-testRows
    trainingDataSet=dataSet[0:trainingRows]
    testDataSet=dataSet[trainingRows:]
    trainingLabels=classLabelVector[0:trainingRows]
    testLabels=classLabelVector[trainingRows:]
    return trainingDataSet,testDataSet,trainingLabels,testLabels

def confusionMatrix(testDataSet,trainingDataSet,trainingLabels,classLabelVector,testLabels,k):
    #Creates the confusion matrix
    #Rows are the true values
    #Columns are the predicted ones
    m=unique(classLabelVector).shape[0]
    confMat=zeros((m,m))

    #Creates a dictionary with the indices
    #this dictionary will be used for filling the confusion matrix
    dicIndex={}
    uniqueLabels=unique(classLabelVector)
    for i in range(m):
        dicIndex[uniqueLabels[i]]=i

    #Starts the predictions
    for i in range(testDataSet.shape[0]):
        prediction=classify0(testDataSet[i],trainingDataSet,trainingLabels,k)
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
