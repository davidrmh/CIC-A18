## Reducción de reglas lógicas.

Sean
+ $n$ el número de semillas necesarias para obtener el conjunto de reglas que defina una clase.

+ $m_{i}$ el número de premisas para cubrir a la semilla $i$.

+ $P_{ij}$ la premisa $j$ de la semilla $i$. $i \in {1,\ldots,n}$ $j \in {1,\ldots,m_{i}}$.

Cada semilla tiene una estrella asociada, la cual puede expresarse como una disyunción de conjunciones, es decir,

$$P_{11}P_{12}+\ldots+P_{1m_{i}-1}P_{1m_{i}}$$

Utilizando lo anterior, podemos crear una regla $R$ la cual toma el valor de $1$ si la observación es de la clase positiva y $0$ en otro caso.

$$R=(P_{11}P_{12}+\ldots+P_{1m_{i}-1}P_{1m_{i}}) + (P_{21}P_{22}+\ldots+P_{2m_{i}-1}P_{2m_{i}})+\ldots+
(P_{n1}P_{n2}+\ldots+P_{nm_{i}-1}P_{nm_{i}}) $$

Cada expresión en paréntesis representa una estrella relacionada a una semilla.

Finalmente, utilizando las conjunciones de la regla $R$, podemos construir una matriz de cobertura para las $N$ observaciones de la clase positiva.

|Mintérmino|Obs1|Obs2|$\ldots$|ObsN|
|----------|----|----|--------|----|
|$P_{11}P_{12}$|X|$\ldots$|$\ldots$|x|
|$\vdots$|$\vdots$|$\vdots$|$\vdots$|$\vdots$|
|$P_{1m_{i}-1}P_{1m_{i}}$|X|$\ldots$|$\ldots$|x|
|$\vdots$|$\vdots$|$\vdots$|$\vdots$|$\vdots$|
|$P_{nm_{i}-1}P_{nm_{i}}$|X|$\ldots$|$\ldots$|x|

Utilizando esta matriz es posible aplicar el método de reducción de Petrick.
