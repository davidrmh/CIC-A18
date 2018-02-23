# Tarea 2

## Aprendizaje simbólico

David Ricardo Montalván Hernández
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

Considerando lo siguiente, se define la función de **semejanza** como una función de la distancia:

* La función $d$ está acotada en $[0,120]$.
* *Condiciones frontera:* Buscamos una semejanza, $s$, tal que si $d=0$, entonces $s=1$ y si $d=120$, entonces $s=0$.

Dado lo anterior, se propone $s$ como

$$s(a,b)=p +qd(a,b)$$

además utilizando las *condiciones frontera* podemos obtener los valores de $p=1$ y $q=-\dfrac{1}{120}$, en consecuencia

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
* *Condiciones frontera:* Si $d=0$, entonces necesitamos $s=1$ y si $d=3$, necesitamos $s=0$.

Proponiendo $s(a,b)=p+qd(a,b)$ y utilizando las *condiciones frontera*, $s$ está dada por:

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

Para definir la función de **semejanza** se procede como en los casos anteriores y de esta manera

$$s(a,b) = 1 - \dfrac{1}{5}d(a,b)$$

* **Caso general**

Dada una función de distancia, $d$, acotada superiormente por $L>0$, se puede definir una función de semejanza, $s$, de la siguiente manera

$$s(a,b)=1 - \dfrac{1}{L}d(a,b)$$

* **Cálculo de distancia/semejanza sintáctica**

Para los patrones $P_{1}=(dulce,27,sábado,grande)$, $P_{2}=(salado,25,jueves,mediano)$, la distancia/semejanza sintáctica está dada por:

$D_{s}(P_{1},P_{2})=1+2+2+1=6\\
S_{s}(P_{1},P_{2})=\dfrac{1}{4}(0+\dfrac{59}{60}+\dfrac{1}{3}+\dfrac{4}{5})=\dfrac{127}{240}=0.5291$
