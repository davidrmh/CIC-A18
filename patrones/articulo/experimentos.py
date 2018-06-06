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

        #Normaliza los datos (Atributos con valores continuos)
        entrenamientoNormalizados=preprocessing.scale(entrenamiento.iloc[:,0:numAtributos])
        pruebaNormalizados=preprocessing.scale(prueba.iloc[:,0:numAtributos])

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

        #Normaliza los datos (Atributos con valores continuos)
        entrenamientoNormalizados=preprocessing.scale(entrenamiento.iloc[:,0:numAtributos])
        pruebaNormalizados=preprocessing.scale(prueba.iloc[:,0:numAtributos])

        #ajusta modelo
        modelo=MLPClassifier(solver="lbfgs",alpha=1e-5,random_state=0,hidden_layer_sizes=(10,numAtributos))
        modelo.fit(entrenamiento.iloc[:,0:numAtributos],entrenamiento['Clase'])

        #Realiza las predicciones
        prueba['Clase']=modelo.predict(prueba.iloc[:,0:numAtributos])

        return prueba


##==============================================================================
##                              EVALUA MÉTODO 2
##==============================================================================
def evaluaMetodo2 (archivoDatos,inicioEntrena,finEntrena,inicioPrueba,finPrueba,metodo="svm",hback=21,features=1):
    '''
    ENTRADA
    archivoDatos: String. Nombre del archivo csv de Yahoo Finance

    inicio*,fin*: String. Fechas de inicio y fin para cada conjunto

    metodo: String. Que método se utiliza para aprender

    hback: Entero. Número de periodos hacia atrás

    SALIDA
    exceso: Float. Exceso de ganancia en el conjunto de prueba
    '''

    #Obtiene el prefijo del nombre e.g. naftrac
    prefijo=archivoDatos.split(".csv")[0]

    #Archivo en donde se guardan los resultados
    archivoResultados= prefijo + "-" + "resultados-" + metodo + ".txt"
    f=open(archivoResultados,"a") #append

    #Lee el csv con todos los datos
    datos=etiqueta.leeTabla(archivoDatos)

    #Lee el archivo de entrenamiento correspondiente
    archivoEntrena=prefijo + "-" + inicioEntrena + ".csv"
    entrenamiento=pd.read_csv(archivoEntrena)

    #Crea los atributos para el conjunto de entrenamiento
    #de acuerdo al método especificado
    if features==1:
        entrenaDis,entrenaCon,percentiles=etiqueta.featuresModelo2(datos,inicioEntrena,finEntrena,hback,percentiles=False)

        #Añade la columna clase
        entrenaDis.loc[:,('Clase')]=entrenamiento['Clase']
        entrenaCon.loc[:,('Clase')]=entrenamiento['Clase']

        #Crea los atributos para el conjunto de prueba
        pruebaDis,pruebaCon,percentiles=etiqueta.featuresModelo2(datos,inicioPrueba,finPrueba,hback,percentiles=percentiles)

    elif features==2:
        entrenaCon=etiqueta.featuresVer2Modelo2(datos,inicioEntrena,finEntrena)
        entrenaCon.loc[:,('Clase')]=entrenamiento['Clase']
        pruebaCon=etiqueta.featuresVer2Modelo2(datos,inicioPrueba,finPrueba)

    #Obtiene el data frame que utiliza la función fitnessMetodo2
    #del archivo etiqueta
    prueba=etiqueta.subconjunto(datos,inicioPrueba,finPrueba)

    #Entrena el modelo y obtiene la estrategia para el conjunto de prueba
    if metodo=="c4.5":
        #C4.5 utiliza atributos continuos
        pruebaCon=clasificadorArbol(entrenaCon,pruebaCon)
        prueba.loc[:,('Clase')]=pruebaCon['Clase']

    elif metodo=="svm":
        #SVM utiliza atributos continuos
        pruebaCon=clasificadorSVM(entrenaCon,pruebaCon)
        prueba.loc[:,('Clase')]=pruebaCon['Clase']

    elif metodo=="mlp":
        #MLP utiliza atributos continuos
        pruebaCon=clasificadorMLP(entrenaCon,pruebaCon)
        prueba.loc[:,('Clase')]=pruebaCon['Clase']

    #Obtiene el exceso de ganacia sobre BH
    exceso=etiqueta.fitnessMetodo2(prueba)
    if features==1:
        f.write("Parametro hback = " + str(hback) + "\n")
    print "Entrenamiento " + inicioEntrena + " a " + finEntrena
    f.write("Entrenamiento " + inicioEntrena + " a " + finEntrena + "\n")
    print "Prueba " + inicioPrueba + " a " + finPrueba
    f.write("Prueba " + inicioPrueba + " a " + finPrueba + "\n")
    print "Para el modelo " + metodo
    f.write("Para el modelo " + metodo + "\n")
    print "El exceso sobre BH fue de " + str(round(exceso,6))
    f.write ("El exceso sobre BH fue de " + str(round(exceso,6)) + "\n")
    print "\n"
    f.write("\n")

    f.close()
    return exceso
