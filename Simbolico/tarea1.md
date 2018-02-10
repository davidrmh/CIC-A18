# Tarea 1 Aprendizaje Simbólico

Para aprender el marco, $\mathcal{M}$, formado por dos rectángulos, necesitamos el siguiente conjunto de datos:

 + $y_{1l}=$ Ordenada más grande los ejemplos positivos
 + $y_{2l}=$ Ordenada más grnade de los  ejemplos negativos tal que $y_{2l} < y_{1l}$
 + $y_{1r}=$ Ordenada más pequeña de los ejemplos positivos.
 + $y_{2r}=$ Ordenada más pequeña de ejemplos negativos tal que $y_{2r}>y_{1r}$
 + $x_{1l}=$ Abscisa más pequeña de los ejemplos positivos.
 + $x_{2l}=$ Abscisa más pequeña de los ejemplos negativos tal que $x_{2l} > x_{1l}$
 + $x_{1r}=$ Abscisa más grande de los ejemplos positivos.

 + $x_{2r}=$ Abscisa más grande de los ejemplos negativos tal que $x_{2r} < x_{1r}$

Utilizando los datos anteriores se forman los puntos $(x_{1l},y_{1l}),\,(x_{1r},y_{1r}),\,(x_{2l},y_{2l}) \,(x_{2r},y_{2r})$, los cuales corresponden a las esquinas superiores izquierdas y esquinas inferiores derechas de los rectángulos mayor y menor respectivamente.

 Un punto $(x,y)$ pertenece a $\mathcal{M}$ si alguna de las siguientes condiciones se cumple:

+ $x_{1l} \leq x \leq x_{ir}$ y además ( $y_{1r} \leq y \leq y_{2r}$ o $y_{2l} \leq y \leq y_{1l}$ )
+ $x_{1l} \leq x \leq x_{2l}$ y además $y_{1r} \leq y \leq y_{il}$
+ $x_{2r} \leq x \leq x_{1r}$ y además $y_{1r} \leq y \leq y_{1l}$

### Algoritmo

~~~~
1. Usuario introduce los puntos
~~~~
