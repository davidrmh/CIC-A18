# coding: utf-8
import numpy as np
import matplotlib.pyplot as plt

##==============================================================================
## Sphere function
##==============================================================================
def sphere (x):
    '''
    Benchmark que se trata de minimizar
    ENTRADA
    x: es un arreglo de numpy con cada entrada x_i en [-5.12,5.12]
    SALIDA
    número real
    '''
    return np.sum(x**2) #Suma de cuadrados

##==============================================================================
## Adaptive (1+1)ES
##==============================================================================

def ES (numGen=500,c=0.6):
    '''
    Estrategia de evolución (1+1) adaptada con la regla de 1/5
    ENTRADA:
    numGen: Número de generaciones
    c: Factor para ajustar la varianza
    SALIDA:
    costo: Vector cuya entranda i es el valor de la función objetivo
    en la generación i
    '''

    #inicializa sigma
    sigma=0.1/(2*np.sqrt(3))

    #Genera un individuo inicial
    padre=np.random.uniform(-5.12,5.12,10)

    #Parámetro G
    g=min(len(padre),30)

    #Contador de mutaciones exitosas
    contMutExito=0

    #Proporción de mutaciones exitosas
    propMutExito=float(contMutExito)/g

    #Arreglo en donde se almacena el costo de cada generación
    costo=np.zeros(numGen)

    #Auxiliar para llevar el conteo de cada g generaciones
    auxCont=0

    for i in range(0,numGen):
        #Genera un vector r con componentes normales(0,sigma**2)
        r=np.random.normal(0,sigma,10)

        #Calcula el costo del padre
        costoPadre=sphere(padre)

        #Genera un hijo y calcula su costo
        hijo=padre + r
        costoHijo=sphere(hijo)

        #Decide quien es mejor
        #cuenta mutación exitosa
        if costoHijo<costoPadre:
            padre=hijo
            contMutExito=contMutExito+1

        propMutExito=float(contMutExito)/g

        #Actualiza la desviación estándar
        #primero deben pasar g generaciones
        if auxCont>=g:
            auxCont=0
            if propMutExito<(1/5):
                sigma = (c**2)*sigma
            else:
                sigma=sigma/(c**2)

        auxCont=auxCont+1
        costo[i] = min(costoPadre,costoHijo)

    return costo


##==============================================================================
## Calcula el costo promedio por generación
##==============================================================================
def costoPromedio(simulaciones):
    '''
    ENTRADA
    simulaciones: Lista con los resultados de las simulaciones
    SALIDA:
    promedios: Arreglo de numpy cuya entrada i es el costo promedio
    de la generación i
    '''

    #número de simulaciones
    numSim=len(simulaciones)

    #número de generaciones
    numGen=len(simulaciones[0])

    promedios=np.zeros(numGen)
    suma=0

    for i in range(0,numGen):
        for j in range(0,numSim):
            #Suma de costos de la generación i
            suma=suma+simulaciones[j][i]
        promedios[i]=float(suma)/numSim
        suma=0
    return promedios

##==============================================================================
## Grafica los resultados
##==============================================================================
def grafica (costos,c):
    plt.clf()
    for i in range(0,len(c)):
        plt.plot(costos[i],label="c=" + str(c[i]))
    plt.xlabel("Numero de generacion")
    plt.ylabel("Costo")
    plt.title("Comparaciones ejercicio 6.6")
    plt.legend(loc="best")
    plt.show()

##==============================================================================
## Main
##==============================================================================
def main (numGen=500,numSim=50,c=[0.6,0.8,1]):
    '''
    ENTRADA
    numGen: Número de generaciones
    numSim: Número de simulaciones
    c: Parámetros para escalar la desviación estándar

    SALIDA
    Gráfica con los costos promedios para cada generación
    '''

    simulaciones=[]
    costos=[]
    for valor in c:
        for i in range(0,numSim):
            simulaciones.append(ES(numGen,valor))
        costos.append(costoPromedio(simulaciones))

    grafica(costos,c)
