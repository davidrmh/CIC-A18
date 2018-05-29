# coding: utf-8
import pandas as pd
import numpy as np
import copy as cp
import matplotlib.pyplot as plt
import etiqueta as etiqueta # :P
reload(etiqueta)
from sklearn import preprocessing
from sklearn import svm
from sklearn import tree
from sklearn.neural_network import MLPClassifier
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
##                          EVALUA MODEL SVM
##                      CON MÉTODO DE ETIQUETADO 1
##==============================================================================
def evaluaEstrategia(nombreArchivo="naftrac.csv",archivoFechas="fechas.csv",hforw=10,hback=7,umbral=0.015,estrategia="svm"):
    '''
    ENTRADA
    nombreArchivo. String. Nombre del archivo csv con los precios

    archivoFechas. String. Nombre del archivo csv con las fechas para separar
    los periodos de entrenamiento y prueba

    hforw: Entero. Número de periodos que "se mira" hacia el futuro

    hback: Entero. Número de periodos que "se mira" hacia el pasado

    umbral: Float. Umbral para considerar que hubo un tendencia a la alza
    '''

    #Lee los precios
    datos=etiqueta.leeTabla(nombreArchivo)

    #Lee las fechas
    fechas=pd.read_csv(archivoFechas)

    #Aquí guardo las ganancias de cada periodo
    gananciasEstrategia=[]
    gananciasBH=[]

    #Comienza la evaluación de los modelos
    for i in range(0,fechas.shape[0]):

        #Fecha de inicio para el conjunto de entrenamiento
        inicioEntrena=fechas.iloc[i,0]

        #Fecha fin del conjunto de entrenamiento
        finEntrena=fechas.iloc[i,1]

        #Fecha inicio del conjunto de prueba
        inicioPrueba=fechas.iloc[i,2]

        #Fecha fin del conjunto de prueba
        finPrueba=fechas.iloc[i,3]

        #Etiqueta el conjunto de entrenamiento
        entrenaDis,entrenaCon,percentiles=etiqueta.etiquetaMetodo1(datos,inicioEntrena,finEntrena,hforw,hback,umbral)

        #Revisa que se tenga al menos una observación de cada clase
        #Sólo aplica para SVM
        if estrategia=="svm" and (len(entrenaDis[entrenaDis['Clase']==0].index)==0 or len(entrenaDis[entrenaDis['Clase']==1].index)==0 or len(entrenaDis[entrenaDis['Clase']==-1].index)==0):
            print "Para el conjunto de entrenamiento que inicia el " + str(inicioEntrena) +" no se tienen las tres clases"
            print "\n"
            continue

        #Crea el conjunto de prueba
        pruebaCon,pruebaDis=etiqueta.conjuntoPruebaMetodo1 (datos,inicioPrueba,finPrueba,percentiles,hback)

        #Ajusta el modelo y obtiene las predicciones
        if estrategia=="svm":
            pruebaCon=clasificadorSVM(entrenaCon,pruebaCon)
        elif estrategia=="c4.5":
            pruebaCon=clasificadorArbol(entrenaDis,pruebaDis)
        elif estrategia=="mlp":
            pruebaCon=clasificadorMLP(entrenaCon,pruebaCon)


        #Califica sobre el conjunto de prueba
        gananciaEstrategia,gananciaBH=etiqueta.evaluaMetodo1(datos,pruebaCon,hforw,umbral)

        if gananciaEstrategia>gananciaBH:
            gananciasEstrategia.append(gananciaEstrategia)
            gananciasBH.append(gananciaBH)

        #Imprime resultados
        print "Para el conjunto de prueba del " + str(inicioPrueba) + " al " + str(finPrueba)
        print "Se tiene que: Ganancia estrategia = " + str(gananciaEstrategia)
        print "Ganancia buy and hold = " + str(gananciaBH)
        print "\n"

    return gananciasEstrategia,gananciasBH




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
        modelo=tree.DecisionTreeClassifier(random_state=0,max_depth=5)
        modelo.fit(entrenamiento.iloc[:,0:numAtributos],entrenamiento['Clase'])

        #Realiza las predicciones
        prueba['Clase']=modelo.predict(prueba.iloc[:,0:numAtributos])

        return prueba

##==============================================================================
##                          MULTI-LAYER PERCEPTRON
##==============================================================================
def clasificadorMLP(entrenamiento,prueba):
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

        #ajusta modelo
        modelo=MLPClassifier(solver="lbfgs",alpha=1e-5,random_state=0,hidden_layer_sizes=(10,numAtributos))
        modelo.fit(entrenamiento.iloc[:,0:numAtributos],entrenamiento['Clase'])

        #Realiza las predicciones
        prueba['Clase']=modelo.predict(prueba.iloc[:,0:numAtributos])

        return prueba
