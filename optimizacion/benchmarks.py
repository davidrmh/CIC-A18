# coding: utf-8
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
from matplotlib.ticker import LinearLocator, FormatStrFormatter
##==============================================================================
## Funciones bencharmk para probar algoritmos de optimización
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

    elif funcion=='schwefel':
        z=schwefel(x,y)
        titulo='Schwefel'
        if puntos:
            pz=schwefel(puntos[0],puntos[1])

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
## Schwefel
##  - \sum x_{i} sin (sqrt (abs (x_{i})))
## Dominio de búsqueda [-500,500]
## Óptimo en x=y=420.9687 con valor de 0
##==============================================================================
def schwefel(x,y):
    '''
    ENTRADA
    x,y:números reales o arreglos creados con np.meshgrid

    SALIDA
    número real o arreglo de numpy
    '''
    aux=np.array([x,y]) #Para poder vectorizar operaciones

    return -1*np.sum(aux * np.sin(np.sqrt(np.abs(aux))),axis=0)

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
