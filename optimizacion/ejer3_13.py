import numpy as np

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
## Variables globales
##================================================================
