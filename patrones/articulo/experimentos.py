# coding: utf-8
import pandas as pd
import numpy as np
import copy as cp
import matplotlib.pyplot as plt
from sklearn import preprocessing
from sklearn import svm
from sklearn import tree
##==============================================================================
##                       EXPERIMENTOS PARA EL ARTÍCULO
##==============================================================================


##==============================================================================
##                      SUPPORT VECTOR MACHINES
##==============================================================================
def clasificadorSVM(entrenamiento,prueba):
    '''
    ENTRADA
    entrenamiento: Pandas DataFrame. Conjunto de entrenamiento con últimas
    tres columnas Clase, Date y Adj Close

    prueba: Pandas DataFrame. Conjunto de prueba con últimas dos columnas
    Date y Adj Close

    SALIDA
    prueba: Pandas DataFrame. Conjunto de prueba con la nueva columna Clase
    '''

    #Atributos utilizados para ajustar el modelo
    numAtributos=entrenamiento.shape[1]-3

    #Normaliza los datos (Atributos con valores continuos)
    entrenamientoNormalizados=preprocessing.scale(entrenamiento.iloc[:,0:numAtributos])
    pruebaNormalizados=preprocessing.scale(prueba.iloc[:,0:numAtributos])

    #Le da menos importancia a las posiciones HOLD
    modelo=svm.SVC(C=0.7,class_weight={0:0.1,1:0.45,-1:0.45})
    modelo.fit(entrenamientoNormalizados,entrenamiento['Clase'])

    #realiza las predicciones
    prueba['Clase']=modelo.predict(pruebaNormalizados)

    return prueba

##==============================================================================
##                          ÁRBOL DE DECISIÓN C4.5
##==============================================================================
def clasificadorArbol(entrenamiento,prueba):
        '''
        ENTRADA
        entrenamiento: Pandas DataFrame. Conjunto de entrenamiento con últimas
        tres columnas Clase, Date y Adj Close

        prueba: Pandas DataFrame. Conjunto de prueba con últimas dos columnas
        Date y Adj Close

        SALIDA
        prueba: Pandas DataFrame. Conjunto de prueba con la nueva columna Clase
        '''
        #Atributos utilizados para ajustar el modelo
        numAtributos=entrenamiento.shape[1]-3

        #Ajusta modelo
        modelo=tree.DecisionTreeClassifier(random_state=0,max_depth=3)
        modelo.fit(entrenamiento.iloc[:,0:numAtributos],entrenamiento['Clase'])

        #Realiza las predicciones
        prueba['Clase']=modelo.predict(prueba.iloc[:,0:numAtributos])

        return prueba
