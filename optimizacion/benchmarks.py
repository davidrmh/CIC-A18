# coding: utf-8
import numpy as np
import copy as cp
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
from matplotlib.ticker import LinearLocator, FormatStrFormatter
##==============================================================================
## Funciones benchmak para probar algoritmos de optimización
## Se trabajará en el plano R3
##==============================================================================

##==============================================================================
## Función para graficar las superficies
##==============================================================================
def grafica(limInf,limSup,step,funcion,puntos=[]):
    '''
    ENTRADA
    limInf: Real. Límite inferior en el dominio de búsqueda

    limSup: Real. Límite superior en el dominio de búsqueda

    step: Real. Tamaño de la división del dominio de búsqueda

    funcion: String. Nombre de la función a graficar

    puntos: Lista o numpy array. Coordenadas [x,y] de un punto a graficar sobre la superficie
    '''
    fig = plt.figure()
    ax = fig.gca(projection='3d')

    #Crea la malla
    x,y=np.meshgrid(np.arange(limInf,limSup,step),np.arange(limInf,limSup,step))

    #Calcula f(x,y)
    if funcion=='eggholder':
        z=eggholder(x,y)
        titulo='Eggholder'
        if puntos:
            pz=eggholder(puntos[0],puntos[1])

    elif funcion=='sphere':
        z=sphere(x,y)
        titulo='Sphere'
        if puntos:
            pz=sphere(puntos[0],puntos[1])

    elif funcion=='schaffer':
        z=schaffer(x,y)
        titulo='Schaffer'
        if puntos:
            pz=schaffer(puntos[0],puntos[1])

    elif funcion=='cross':
        z=cross(x,y)
        titulo='Cross-in-tray'
        if puntos:
            pz=cross(puntos[0],puntos[1])

    elif funcion=='holder':
        z=holder(x,y)
        titulo='Holder table'
        if puntos:
            pz=holder(puntos[0],puntos[1])


    #Crea la gráfica
    surf=ax.plot_surface(x,y,z,cmap=cm.coolwarm,linewidth=0,antialiased=False)
    ax.zaxis.set_major_locator(LinearLocator(10))
    ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
    fig.colorbar(surf, shrink=0.5, aspect=5)
    plt.title(titulo)
    if puntos:
        ax.scatter(puntos[0],puntos[1],pz,color="green",s=500)
    plt.show()



##==============================================================================
## Eggholder function
## Dominio de búsqueda [-512,512]
## Mínimo en x=512, y=404.2319 con valor de -959.6407
##==============================================================================
def eggholder(x,y):
    '''
    ENTRADA
    x,y:números reales o arreglos creados con np.meshgrid

    SALIDA
    número real o arreglo de numpy
    '''

    return -(y+47)*np.sin(np.sqrt(np.abs(x/2 + (y+47)))) - x*np.sin(np.sqrt(np.abs(x-(y+47))))

##==============================================================================
## sphere function
## Dominio de búsqueda [-inf,inf]
## Oṕtimo x=y=0 con valor 0
##==============================================================================
def sphere(x,y):
    '''
    ENTRADA
    x,y:números reales o arreglos creados con np.meshgrid

    SALIDA
    número real o arreglo de numpy
    '''
    aux=np.array([x,y]) #Para poder vectorizar operaciones

    if isinstance(x,float) or isinstance(x,int):
        n=2
    else:
        n=x.size

    return np.sum(aux**2,axis=0)

##==============================================================================
## Schaffer No 2
## Dominio de búsqueda [-100,100]
## Óptimo en x=y=0 con valor de 0
##==============================================================================
def schaffer(x,y):
    '''
    ENTRADA
    x,y:números reales o arreglos creados con np.meshgrid

    SALIDA
    número real o arreglo de numpy
    '''

    return 0.5 + ((np.sin(x**2-y**2))**2 - 0.5) / ((1 + 0.001*(x**2 + y**2))**2)

##==============================================================================
## Cross-in-tray function
## Dominio de búsqueda [-10,10]
## Óptimos:
##  x: 1.34941 y:-1.34941
##  x: 1.34941 y:1.34941
##  x: -1.34941 y:1.34941
##  x: -1.34941 y:-1.34941
## todos con valor de -2.06261
##==============================================================================
def cross(x,y):
    '''
    ENTRADA
    x,y:números reales o arreglos creados con np.meshgrid

    SALIDA
    número real o arreglo de numpy
    '''

    return -0.0001*(np.abs(np.sin(x)*np.sin(y)*np.exp(np.abs(100 - np.sqrt(x**2 + y**2)/np.pi))) +1)**0.1

##==============================================================================
## Hölder table function
## Dominio de búsqueda [-10,10]
## Óptimos
## x: 8.05502, y:9.66459
## x: -8.05502, y:9.66459
## x: 8.05502, y:-9.66459
## x: -8.05502, y:-9.66459
##==============================================================================
def holder(x,y):
    '''
    ENTRADA
    x,y:números reales o arreglos creados con np.meshgrid

    SALIDA
    número real o arreglo de numpy
    '''

    return -np.abs(np.sin(x)*np.cos(y)*np.exp(np.abs(1 - np.sqrt(x**2 + y**2)/np.pi)))

##==============================================================================
##                           ALGORITMOS DE OPTIMIZACIÓN
##==============================================================================

##==============================================================================
##                          VARIABLES GLOBALES
##==============================================================================
beta=1 #Para EP
gamma=0 #Para EP
fi1max=2.05 #Para PSO
fi2max=2.05 #Para PSO

##==============================================================================
## Función para crear poblaciones iniciales
## La población será representada por una matriz de numIndividuos x 2
##==============================================================================
def generaPoblacion(numIndividuos,limInf,limSup):
    '''
    ENTRADA
    numIndividuos: Entero. Número de individuos en la población
    limInf: Real. Límite inferior del dominio de búsqueda
    limSup: Real. Límite superior del dominio de búsqueda

    SALIDA
    poblacion. Numpy Array 2D. Matriz cuyo elemento [i,j] representa
    el valor del individuo i en el componente j
    '''

    poblacion = np.random.uniform(limInf,limSup,size=(numIndividuos,2))

    return poblacion

##==============================================================================
## Algoritmo de optimización utilizando programación evolutiva
## OBSERVACIÓN: Este algoritmo funciona para funciones objetivo >= 0
##==============================================================================
def EP(objetivo,numIndividuos,numGeneraciones,limInf,limSup):
    '''
    ENTRADA
    objetivo: Funcion. Alguno de los benchmarks

    numIndividuos: Entero. Número de individuos de la población

    numGeneraciones: Entero. Número de generaciones

    limInf: Real. Límite inferior del dominio de búsqueda

    limSup: Real. Límite superior del dominio de búsqueda

    SALIDA
    mejorIndividuo: Arreglo con dos componentes representando la mejor solución
    '''

    #Genera población inicial
    poblacion=generaPoblacion(numIndividuos,limInf,limSup)

    mejorFitness=10000

    for i in range(0,numGeneraciones):

        for j in range(0,numIndividuos):
            #Individuo actual
            #y su aptitud
            individuoActual=poblacion[j]
            fitnessActual=objetivo(individuoActual[0],individuoActual[1])

            if fitnessActual < mejorFitness:
                mejorFitness=fitnessActual
                mejorIndividuo=individuoActual

            #Crea un nuevo individuo
            r=np.random.normal(size=(1,2))

            #Utilizo max para evitar argumentos negativos
            nuevoIndividuo=individuoActual + r*np.sqrt(max(beta*fitnessActual + gamma,0))

            #Restringe al dominio de búsqueda
            if nuevoIndividuo[0][0]<limInf:
                nuevoIndividuo[0][0]=limInf
            if nuevoIndividuo[0][0]>limSup:
                nuevoIndividuo[0][0]=limSup

            if nuevoIndividuo[0][1]<limInf:
                nuevoIndividuo[0][1]=limInf
            if nuevoIndividuo[0][1]>limSup:
                nuevoIndividuo[0][1]=limSup

            nuevoFitness=objetivo(nuevoIndividuo[0][0],nuevoIndividuo[0][1])

            #Compara individuos
            if nuevoFitness < fitnessActual:
                poblacion[j]=nuevoIndividuo

            if nuevoFitness < mejorFitness:
                mejorFitness=nuevoFitness
                mejorIndividuo=nuevoIndividuo

        print "Fin de la generación " + str(i+1)
        print "Mejor aptitud hasta el momento " + str(mejorFitness)

    return mejorIndividuo

##==============================================================================
## Algoritmo para la estrategia (1 + 1)
##==============================================================================
def onePlusOne(objetivo,numGeneraciones,limInf,limSup):
    '''
    ENTRADA
    objetivo: Funcion. Alguno de los benchmarks

    numGeneraciones: Entero. Número de generaciones

    limInf: Real. Límite inferior del dominio de búsqueda

    limSup: Real. Límite superior del dominio de búsqueda

    SALIDA
    mejorIndividuo: Arreglo con dos componentes representando la mejor solución
    '''
    #la población sólo consiste de un individuo
    poblacion=generaPoblacion(1,limInf,limSup)

    mejorFitness=10000

    contadorExito=0
    sigma=1.0
    c=0.817
    G=2 #min(n,30)

    for i in range(0,numGeneraciones):

        for j in range(0,1):
            #Individuo actual
            #y su aptitud
            individuoActual=poblacion[j]
            fitnessActual=objetivo(individuoActual[0],individuoActual[1])

            if fitnessActual < mejorFitness:
                mejorFitness=fitnessActual
                mejorIndividuo=individuoActual

            #Crea un nuevo individuo
            r=np.random.normal(scale=sigma,size=(1,2))
            nuevoIndividuo=individuoActual + r

            #Restringe al dominio de búsqueda
            if nuevoIndividuo[0][0]<limInf:
                nuevoIndividuo[0][0]=limInf
            if nuevoIndividuo[0][0]>limSup:
                nuevoIndividuo[0][0]=limSup

            if nuevoIndividuo[0][1]<limInf:
                nuevoIndividuo[0][1]=limInf
            if nuevoIndividuo[0][1]>limSup:
                nuevoIndividuo[0][1]=limSup

            nuevoFitness=objetivo(nuevoIndividuo[0][0],nuevoIndividuo[0][1])

            #Compara individuos
            if nuevoFitness < fitnessActual:
                poblacion[j]=nuevoIndividuo
                contadorExito=contadorExito+1

            if nuevoFitness < mejorFitness:
                mejorFitness=nuevoFitness
                mejorIndividuo=nuevoIndividuo

            #Actualiza sigma
            if i >= G:
                if float(contadorExito)/G < 1.0/5:
                    sigma=sigma*c**2
                elif float(contadorExito)/G > 1.0/5:
                    sigma = sigma / (c**2)
                contadorExito=0

        print "Fin de la generación " + str(i+1)
        print "Mejor aptitud hasta el momento " + str(mejorFitness)

    return mejorIndividuo

##==============================================================================
## Algoritmo para la estrategia (1 + 1)
## sin ajuste del parámetro sigma
##==============================================================================
def onePlusOneVer2(objetivo,numGeneraciones,limInf,limSup):
    '''
    ENTRADA
    objetivo: Funcion. Alguno de los benchmarks

    numGeneraciones: Entero. Número de generaciones

    limInf: Real. Límite inferior del dominio de búsqueda

    limSup: Real. Límite superior del dominio de búsqueda

    SALIDA
    mejorIndividuo: Arreglo con dos componentes representando la mejor solución
    '''
    #la población sólo consiste de un individuo
    poblacion=generaPoblacion(1,limInf,limSup)

    mejorFitness=10000

    contadorExito=0
    sigma=1.0

    for i in range(0,numGeneraciones):

        for j in range(0,1):
            #Individuo actual
            #y su aptitud
            individuoActual=poblacion[j]
            fitnessActual=objetivo(individuoActual[0],individuoActual[1])

            if fitnessActual < mejorFitness:
                mejorFitness=fitnessActual
                mejorIndividuo=individuoActual

            #Crea un nuevo individuo
            r=np.random.normal(scale=sigma,size=(1,2))
            nuevoIndividuo=individuoActual + r

            #Restringe al dominio de búsqueda
            if nuevoIndividuo[0][0]<limInf:
                nuevoIndividuo[0][0]=limInf
            if nuevoIndividuo[0][0]>limSup:
                nuevoIndividuo[0][0]=limSup

            if nuevoIndividuo[0][1]<limInf:
                nuevoIndividuo[0][1]=limInf
            if nuevoIndividuo[0][1]>limSup:
                nuevoIndividuo[0][1]=limSup
            

            nuevoFitness=objetivo(nuevoIndividuo[0][0],nuevoIndividuo[0][1])

            #Compara individuos
            if nuevoFitness < fitnessActual:
                poblacion[j]=nuevoIndividuo
                contadorExito=contadorExito+1

            if nuevoFitness < mejorFitness:
                mejorFitness=nuevoFitness
                mejorIndividuo=nuevoIndividuo


        print "Fin de la generación " + str(i+1)
        print "Mejor aptitud hasta el momento " + str(mejorFitness)

    return mejorIndividuo

##==============================================================================
## Evolución diferencial
##==============================================================================
def DE(objetivo,numIndividuos,numGeneraciones,limInf,limSup):

    '''
    ENTRADA
    objetivo: Funcion. Alguno de los benchmarks

    numIndividuos: Entero. Número de individuos de la población

    numGeneraciones: Entero. Número de generaciones

    limInf: Real. Límite inferior del dominio de búsqueda

    limSup: Real. Límite superior del dominio de búsqueda

    SALIDA
    mejorIndividuo: Arreglo con dos componentes representando la mejor solución
    '''

    #Step size
    F=(0.9+0.4)/2.0

    #Crossover rate
    c=(1.0+0.1)/2

    #Población inicial
    poblacion=generaPoblacion(numIndividuos,limInf,limSup)

    #Mejor fitness
    mejorFitness=100000

    for i in range(0,numGeneraciones):

        for j in range(0,numIndividuos):
            individuoActual=poblacion[j]
            fitnessActual=objetivo(individuoActual[0],individuoActual[1])

            if fitnessActual < mejorFitness:
                mejorFitness=fitnessActual
                mejorIndividuo=individuoActual

            #Construye los individuos auxiliares para crear
            #un nuevo individuo
            r=np.random.choice(np.arange(0,numIndividuos),size=3,replace=False)
            while j in r:
                r=np.random.choice(np.arange(0,numIndividuos),size=3,replace=False)
            r1=r[0]
            r2=r[1]
            r3=[2]
            indAux1=poblacion[r1]
            indAux2=poblacion[r2]
            indAux3=poblacion[r3]

            candidato=indAux1 + F*(indAux2 - indAux3)

            #Restringe al dominio de búsqueda
            if candidato[0][0]<limInf:
                candidato[0][0]=limInf
            if candidato[0][0]>limSup:
                candidato[0][0]=limSup

            if candidato[0][1]<limInf:
                candidato[0][1]=limInf
            if candidato[0][1]>limSup:
                candidato[0][1]=limSup

            #Muta la dimension k
            nuevoIndividuo=np.array([0,0]) #Este será el nuevo individuo
            indiceJ=np.random.choice([0,1],size=1)[0]
            for k in range(0,2):
                u=np.random.uniform(size=1)[0]
                if u < c or k==indiceJ:
                    nuevoIndividuo[k]=candidato[0][k]
                else:
                    nuevoIndividuo[k]=individuoActual[k]

            #Calcula el fitness del individuoActual y el nuevoIndividuo
            fitnessNuevo=objetivo(nuevoIndividuo[0],nuevoIndividuo[1])

            #Guarda el mejor
            if fitnessNuevo < fitnessActual:
                poblacion[j]=nuevoIndividuo

            if fitnessNuevo < mejorFitness:
                mejorFitness=fitnessNuevo
                mejorIndividuo=nuevoIndividuo

        print "Fin de la generación " + str(i+1)
        print "Mejor aptitud hasta el momento " + str(mejorFitness)

    return mejorIndividuo
