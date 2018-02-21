# Tarea 2
## Aprendizaje simbólico
### David Ricardo Montalván Hernández
---
* **Sabores**

El dominio está dado por
$$Dom(sabor)=\{dulce,salado,agrio,ácido\}$$

En esta caso la **distancia** (una semi métrica) a utilizar podría ser:

$$
d(a,b)=
\begin{cases}
0, & \text{Si $a=b$} \\
1, & \text{Si $a \neq b$}
\end{cases}
$$
Se define la **semejanza** como una función de la distancia de la siguiente forma:
$$s(a,b) = 1- d(a,b)$$

* **Edad**

El dominio es
$$Dom(edad)=\{0,1,2,\ldots,120\}$$

La **distancia** (métrica) a utilizar sería el valor absoluto
$$d(a,b) = |a-b|$$

Se define la función de **semejanza** como una función de la distancia observando lo siguiente:

* La función $d$ está acotada en $[0,120]$
* *Condiciones frontera:* Buscamos una semejanza, $s$, tal que si $d=0$, entonces $s=1$ y si $d=120$, entonces $s=0$

Dado lo anterior, se propone $s$ como

$$s(a,b)=p +qd(a,b)$$

además utilizando las *condiciones frontera* podemos obtener los valores de $p=1$ y $q=-\dfrac{1}{120}$, con lo que obtenemos

$$s(a,b)=1 - \dfrac{1}{120}d(a,b)$$

* **Dia**

El dominio es

$$Dom(día)=\{Lunes,martes,miércoles,jueves,viernes,sábado,domingo\}$$

La **distancia** (semi métrica) se define como

$$
d(a,b)=
\begin{cases}
0, & \text{Si $a=b$}\\
min*, & \text{Si $a \neq b$}
\end{cases}
$$
En donde $min*$ está dado por el más pequeño entre:

* Días en orden cronológico entre $a$ y $b$ iniciando en $a$.
* Días en orden cronológico entre $a$ y $b$ iniciando en $b$.

Por ejemplo $d(sábado,jueves)=min(5,2)=2$.

La función de **semejanza** se define en función de $d$ observando lo siguiente:

* $d$ está acotada entre $0$ y $3$.
* *Condiciones frontera* si $d=0$, entonces necesitamos $s=1$ y si $d=3$, necesitamos $s=0$.

Proponiendo $s(a,b)=p+qd(a,b)$ y utilizando las *condiciones frontera* la función de semejanza está dada por:

$$s(a,b)=1-\dfrac{1}{3}d(a,b)$$

* **Tamaño**

El dominio es
$$Dom(tamaño)=\{chico,mediano,grande\}$$

La función de **distancia** (semi métrica) se define como

$$d(a,b)=
\begin{cases}
0,& \text{Si $a=b$}\\
2,& \text{Si $a,b \in \{chico,mediano\}$ y $a \neq b$}\\
5, & \text{Si $a,b \in \{chico,grande\}$ y $a \neq b$}\\
1,& \text{Si $a,b \in \{mediano,grande\}$ y $a \neq b$}
\end{cases}
$$

Para definir la función de **semejanza** se procede como en los casos anteriores, observando que $d$ está acotada entre $0$ y $5$, por lo tanto

$$s(a,b) = 1 - \dfrac{1}{5}d(a,b)$$
