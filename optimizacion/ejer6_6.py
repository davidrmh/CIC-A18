import numpy as np
import matplotlib.pyplot as plt

##==============================================================================
## Sphere function
##==============================================================================
def sphere (x):
    '''
    Benchmark que se trata de minimizar
    x es un arreglo de numpy con cada entrada x_i en [-5.12,5.12]
    '''
    return np.sum(x**2) #Suma de cuadrados
    
