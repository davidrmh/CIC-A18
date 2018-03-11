from numpy import *
import matplotlib.pyplot as plt

def loadDataSet(fileName, delim=','):
    fr = open(fileName)
    stringArr = [line.strip().split(delim) for line in fr.readlines()]
    datArr = [map(float,line) for line in stringArr]
    return mat(datArr)

def pca(dataMat, topNfeat=9999999,meanAxis=1,covShape=0,corr=False):
    meanVals = mean(dataMat, axis=meanAxis) #Para mis datos tiene que ser axis=1
    meanRemoved = dataMat - meanVals #remove mean
    if corr:
        covMat = corrcoef(meanRemoved, rowvar=covShape) #Correlations instead of covariances
    else:
        covMat = cov(meanRemoved, rowvar=covShape) #Covariances
    eigVals,eigVects = linalg.eig(mat(covMat))
    eigValInd = argsort(eigVals)            #sort, sort goes smallest to largest
    eigValInd = eigValInd[:-(topNfeat+1):-1]  #cut off unwanted dimensions
    redEigVects = eigVects[:,eigValInd]       #reorganize eig vects largest to smallest
    lowDDataMat = meanRemoved * redEigVects #transform data into new dimensions
    reconMat = (lowDDataMat*redEigVects.T) + meanVals
    return lowDDataMat, reconMat

def createIndex(fileName="proyectos/proyecto1_pca_datos_indice_top10.csv",ncomp=1,corr=False):
    data=loadDataSet(fileName)
    lowDDataMat,reconMat=pca(data,ncomp,0,0,corr)
    lowDDataMat=real(lowDDataMat[::-1]) #Ajusta a orden creciente de fechas
    changes=array(sum(lowDDataMat,axis=1)+1)
    index=[100]
    naftrac=[100]
    changesNaftrac=array(loadDataSet("proyectos/naftrac.csv")+1)
    #Creates indices with base 100
    for i in range(1,len(changes)+1):
        index.append(index[i-1]*changes[i-1])
        naftrac.append(naftrac[i-1]*changesNaftrac[i-1])
    plt.plot(index,c="red")
    plt.plot(naftrac,c="blue")
    plt.xlim((0,len(index)))
    plt.title("Utilizando covarianzas")
    plt.legend(['PCA','NAFTRAC'],loc=0)
    plt.show()
    #return index
