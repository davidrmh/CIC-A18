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
