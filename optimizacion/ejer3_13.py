import matplotlib.pyplot as plt
import numpy as np
from copy import deepcopy

##================================================================
## Funcion objetivo
##================================================================
def objetivo(x):
    '''
    Calculua la funcion objetivo
    ENTRADA:
    x: ndarray
    SALIDA:
    sum xi^2
    '''
    return np.sum(x**2)

##================================================================
## Funcion de fitness
##================================================================
def fitness(x):
    '''
    Calculua la aptitud de un individuo
    ENTRADA:
    x: ndarray
    SALIDA:
    1/sum xi^2
    '''
    if objetivo(x)!=0:
        return 1/objetivo(x)
    else:
        print "Division entre cero"

##=================================================================
## Funcion de cruza
##=================================================================
def cruza(padre1,padre2):
    '''
    Realiza la cruza de dos cromosomas (padre1 y padre2)
    para generar dos hijos
    ENTRADA
    padre1,padre2: ndarray
    SALIDA
    hijo1,hijo2: ndarray
    '''
    minimo=0
    maximo=len(padre1)-1 #-1 por que los indices inician en 0
    hijo1=np.zeros(len(padre1))
    hijo2=np.zeros(len(padre1))

    #Genera el indice de corte
    indice=np.random.randint(minimo,maximo,1)[0]

    #Si el corte fue en el ultimo indice
    if indice==maximo:
        hijo1=padre1
        hijo2=padre2
    #Si el corte fue en otro indice
    else:
        hijo1[minimo:(indice+1)]=padre1[minimo:(indice+1)]
        hijo1[indice+1:]=padre2[indice+1:]
        hijo2[minimo:(indice+1)]=padre2[minimo:(indice+1)]
        hijo2[indice+1:]=padre1[indice+1:]
    return hijo1,hijo2

##================================================================
## Funcion de mutacion
##================================================================
def mutacion(x,pmuta):
    '''
    Muta con probabilidad pmuta el gen de x
    ENTRADA:
    pmuta: numero que representa la probabilidad de mutacion
    x: ndarray (individuo sin mutar)
    SALIDA:
    y: ndarray (individuo posiblemente mutado)
    '''
    y=np.zeros(len(x))

    for i in range(0,len(x)):
        u=np.random.uniform(0,1,1)
        if u<pmuta:
            y[i]=np.random.uniform(-5,5,1) #muta
        else:
            y[i]=x[i] #queda igual
    return y




##================================================================
## Funcion para inicializar la poblacion
##================================================================
def inicializaPoblacion(numInd,dimension,limInf,limSup):
    '''
    ENTRADA:
    numInd: Numero de individuos en la poblacion
    dimension: Dimension del espacio de busqueda
    limInf: Limite inferior en una dimension del espacio de busqueda
    limSup: Limite superior en una dimension del espacio de busqueda
    SALIDA:
    poblacion: lista cuyas entradas representan a los individuos
    '''

    poblacion=[]
    for i in range(0,numInd):
        poblacion.append(np.random.uniform(limInf,limSup,dimension))
    return poblacion


##================================================================
## Parametros
##================================================================
generaciones=100 #Numero de generaciones
numInd=20 #Numero de individuos por generacion
dimension=20 #Dimension del espacio de busqueda
limInf=-5 #Limite inferior en una dimension del espacio de busqueda
limSup=5 #Limite superior en una dimension del espacio de busqueda
pmuta=0.01 #Probabilidad de mutacion

##================================================================
## Pregunta A
##================================================================
def preguntaA():
    nuevaPoblacion=[]
    fitnessPob=[] #fitness de cada individuo
    probabilidades=[] #Probabilidades de seleccion
    costoPob=[] # Costo de cada individuo
    costoPromedio=[] #Costo promedio de la poblacion por cada generacion
    costoMejor=[] #Costo del mejor individuo por cada generacion

    #Inicializa poblacion
    poblacion=inicializaPoblacion(numInd,dimension,limInf,limSup)

    for i in range(0,generaciones):

        #Calcula fitness y costos
        for ind in poblacion:
            fitnessPob.append(fitness(ind))
            costoPob.append(objetivo(ind))

        #Registra el costo promedio y el del mejor individuo de la generacion
        costoPromedio.append(np.mean(costoPob))
        costoMejor.append(np.min(costoPob))

        #Probabilidades de seleccion
        probabilidades=fitnessPob/np.sum(fitnessPob)

        #Genera nueva poblacion
        while len(nuevaPoblacion)!=len(poblacion):
            #Primero cruza
            indices=np.random.choice(range(0,numInd),size=2,p=probabilidades,replace=False)
            padre1=poblacion[indices[0]]
            padre2=poblacion[indices[1]]
            hijo1,hijo2=cruza(padre1,padre2)
            #Despues muta
            hijo1=mutacion(hijo1,pmuta)
            hijo2=mutacion(hijo2,pmuta)
            nuevaPoblacion.append(hijo1)
            nuevaPoblacion.append(hijo2)

        #Prepara las variables para la proxima generacion
        poblacion=deepcopy(nuevaPoblacion)
        nuevaPoblacion=[]
        probabilidades=[]
        fitnessPob=[]
        costoPob=[]
    return costoMejor,costoPromedio    
