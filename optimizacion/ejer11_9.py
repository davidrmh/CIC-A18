# coding: utf-8

##==============================================================================
## Capítulo 11 ejercicio 11.9
## Problema de minimización
##==============================================================================
import numpy as np
import copy as cp
import matplotlib.pyplot as plt

##==============================================================================
## Parámetros (variables globales)
##==============================================================================
numGeneraciones=40
dimension=10
popSize=20
fi1Max=2
fi2Max=2
fi3Max=2
alfa=0.9
numSim=20
k=2*alfa/(np.sum([fi1Max,fi2Max,fi3Max]) - 2)

##==============================================================================
## Función objetivo (se busca minimizar)
##==============================================================================
def objetivo(x):
    '''
    ENTRADA:
    x: arreglo 1D de numpy

    SALIDA
    suma de los componentes de x elevados al cuadrado
    '''

    return np.sum(x**2)

##==============================================================================
## Función de distancia (distancia euclidiana) entre dos partículas
##==============================================================================
def distancia(x,y):
    '''
    ENTRADA
    x,y: Arreglos 1D de numpy

    SALIDA
    distancia entre x y y
    '''
    return np.sqrt(np.sum((x-y)**2))

##==============================================================================
## Función para generar la población inicial
##==============================================================================
def generaPoblacionInicial(dimension=10,individuos=popSize):
    '''
    ENTRADA
    dimension: Entero. Número de componentes de cada partícula
    individuos: Entero. Número de individuos de la población

    SALIDA
    poblacion: Lista. Una lista que en cada posición
    contiene un arreglo 1D de numpy representando a una partícula

    velocidades. Arreglo 1D numpy. Arreglo que en cada índice j contiene
    la velocidad inicial del individuo j
    '''

    poblacion=[]

    #crea las partículas
    #utilizando una distribución uniforme (0,1)
    for i in range(0,individuos):
        poblacion.append(np.random.uniform(size=dimension))

    #Crea las velocidades iniciales
    #utilizando una distribución uniforme (0,1)
    velocidades=np.random.uniform(size=individuos)

    return poblacion,velocidades

##==============================================================================
## Función para encontrar la posición del mejor vecino entre los k mejores.
##==============================================================================
def mejorVecino(particula,poblacion,k):
    '''
    ENTRADA
    particula: Arreglo 1D de numpy
    poblacion: Lista. Una lista que en cada posición
    contiene un arreglo 1D de numpy representando a una partícula
    k: Entero>=1. Número de vecinos

    SALIDA
    mejor: Arreglo 1D numpy. La posición del mejor vecino (el que tiene mejor
    aptitud de acuerdo a la función objetivo)
    '''

    #Número de partículas
    numParticulas=len(poblacion)

    #Aquí guardaré los k-vecinos más cercanos
    kvecinos=[]

    #Distancia entre la partícula y las demás
    distancias=np.zeros(numParticulas)

    #Calcula las distancias
    #Incluye la distancia entre la particula de interés y ella misma
    for i in range(0,numParticulas):
        vecino=poblacion[i]
        distancias[i]=distancia(particula,vecino)

    #Índices de las distancias ordenadas de menor a mayor
    #Se quita el índice cero ya que este corresponde a la misma
    #partícula (distancia 0)
    indicesDistanciasOrdenadas=np.argsort(distancias)
    indicesDistanciasOrdenadas=indicesDistanciasOrdenadas[1:]

    #Almacena los k-vecinos más cercanos
    for i in range(0,k):
        kvecinos.append(poblacion[indicesDistanciasOrdenadas[i]])

    #Calcula la aptitud de cada uno de los k-vecinos
    aptitudes=[]
    for vecino in kvecinos:
        aptitudes.append(objetivo(vecino))

    #Encuentra el k-vecino con la mejor aptitud
    #NOTA: Estamos ante un problema de minimización
    mejor=kvecinos[np.argmin(aptitudes)]

    return mejor


##==============================================================================
## simulaciones
##==============================================================================
def simulaciones(sigma=0):
    '''
    ENTRADA
    sigma: Entero. Número de vecinos

    SALIDA
    performance: Arreglo 2D numpy. Matriz de desempeño
    La entrada (i,j) representa el desempeño en la simulación i para la
    generación j
    '''

    #Matriz de desempeño
    #Me interesa el promedio por columna
    #La entrada (i,j) representa el desempeño en la simulación i para la
    #generación j
    performance=np.zeros((numSim,numGeneraciones))


    for simulacion in range(0,numSim):

        #Crea población y velocidades iniciales
        poblacion,velocidades=generaPoblacionInicial(dimension,popSize)

        #Mejores posiciones
        mejoresPosiciones=cp.deepcopy(poblacion)

        mejoresNuevasPosiciones=[]
        nuevaPoblacion=[]
        nuevasVelocidades=[]

        for generacion in range(0,numGeneraciones):

            for i in range(0,popSize):
                particula=poblacion[i]
                velocidad=velocidades[i]
                mejorPosicionParticula=mejoresPosiciones[i]

                #Mejor kVecino
                if sigma !=0:
                    kVecino=mejorVecino(particula,poblacion,sigma)
                else:
                    kVecino=particula

                #Mejor posición entre todas las partículas
                mejorParticula=mejorVecino(particula,poblacion,popSize-1)

                #Tasas de aprendizaje
                fi1=np.random.uniform(low=0,high=fi1Max,size=dimension)
                fi2=np.random.uniform(low=0,high=fi2Max,size=dimension)[0]
                fi3=np.random.uniform(low=0,high=fi3Max,size=dimension)[0]

                #Calcula la nueva velocidad
                #NOTA: La nueva velocidad es un arreglo
                nuevaVelocidad=k*(velocidad + fi1*(mejorPosicionParticula-particula) + fi2*(kVecino-particula) + fi3*(mejorParticula-particula) )

                #Calcula la nueva posición de la partícula
                nuevaParticula=particula + nuevaVelocidad

                #Calcula la mejor nueva posición
                argAux=np.argmin((objetivo(particula),objetivo(nuevaParticula)))

                if argAux==0:
                    mejoresNuevasPosiciones.append(particula)
                else:
                    mejoresNuevasPosiciones.append(nuevaParticula)

                #Almacena la partícula nueva así como su velocidad
                nuevaPoblacion.append(nuevaParticula)
                nuevasVelocidades.append(nuevaVelocidad)

                #Next i

            #Prepara para la siguiente generación
            poblacion=cp.deepcopy(nuevaPoblacion)
            velocidades=cp.deepcopy(nuevasVelocidades)
            mejoresPosiciones=cp.deepcopy(mejoresNuevasPosiciones)
            nuevaPoblacion=[]
            nuevasVelocidades=[]
            mejoresNuevasPosiciones=[]

            #Actualiza la matriz de desempeño
            #calcula la aptitud promedio para la generación
            aptitudes=[]

            for particula in poblacion:
                aptitudes.append(objetivo(particula))
            performance[simulacion,generacion]=np.average(aptitudes)

            #Next generacion

        #Next simulacion

    return performance

##==============================================================================
## Main
##==============================================================================
def main(sigma=[0,5,10]):
    '''
    ENTRADA
    sigma: Entero. Número de vecinos

    SALIDA:
    gráfica generación vs desempeño promedio en las simulaciones
    '''

    for sig in sigma:

        #Calcula la matriz de desempeño
        performance=simulaciones(sig)

        #Calcula los promedios por generacion
        promedios=np.average(performance,axis=0)

        #Realiza la gráfica
        numGen=len(promedios)
        plt.plot(range(1,numGen+1),promedios,'-',label="sigma= " + str(sig))

    plt.xlabel("Generacion")
    plt.ylabel("Performance promedio")
    plt.title("Ejercicio 11.9")
    plt.legend(loc="best")
    plt.show()
