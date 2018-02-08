from numpy import *

def loadDataSet(fileName,sep='\t'):
    dataMat=[]
    fr=open(fileName)
    for line in fr.readlines():
        curLine = line.strip().split(sep)
        #The line below is needed in order to be able
        #work with datingTestSet
        curLine = curLine[0:len(curLine)-1]
        #Converts the data into floating point
        fltLine = map(float,curLine)
        dataMat.append(fltLine)
    #Converts the list into a numpy array
    dataMat=array(dataMat)
    return dataMat

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
