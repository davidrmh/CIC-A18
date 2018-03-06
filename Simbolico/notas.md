# Notas Aprendizaje Simbólico

## Presentación 01

**¿Qué es el apredizaje?**

En términos simples, aprender significa usar la experiencia del pasado como guía para la toma de decisiones actuales/futuras.

Aprender implica un cambio de largo plazo provocado por la experiencia del agente en su entorno, este apredizaje permitirá el desarrollo de nuevas habilidades con el fin de ejecutar tareas que antes no se podían ejecutar o mejorar el desempeño en estas.

**Definición (Información de aprendizaje)**

Es la información *útil* que se almacena en el proceso de apredizaje.

**Aprendizaje automático**

El aprendizaje automático busca construir programas de computadoras capaces de *aprender*, es decir, capaces de analizar datos (experiencia) y a partir de ellos realizar pronósticos.

**Definición (Aprendizaje en un programa)**

Se dice que un programa de computadora aprende de la experiencia **E**, con respecto a alguna clase de tareas o actividades **T** y con respecto a una medida de desempeño **M**, cuando su desempeño en las tareas **T** mejora, según la medida **M**, después de experimentar la experiencia **E**.

De acuerdo a la definición anterior, el apredizaje en las comptuadoras depende, al igual que en los humanos, de las tareas a realizar, la experiencia adquirida y la medida de desempeño.

**Definición (Machine Learning)**

Es la **rama** de la inteligencia artificial que busca desarrollar técnicas para que las computadoras puedan aprender.

**Paradigmas de aprendizaje automático**

* Simbólico (inducción).

* Estadístico (inferencia bayesiana).

* Conexionista (back propagation).

* Evolutivo (programación genética).

* Analógico (Support Vector Machines).

**Sesgo inductivo**

La incorporación de conocimiento previo genera un sesgo hacia este, por ejemplo, si una persona aprende a realizar un proceso de cierta manera y realiza este mismo proceso durante años, cuando se le quiera enseñar un nuevo proceso, su conocimiento va a estar sesgado al procedimiento anterior lo que hará el menos flexible el proceso de aprendizaje.

## Prensetación 02 - Conceptos fundamentales

**Definición (problema intratable)**

Un problema intratable es un problema:

* Informalmente definido.

* Sin solución algorítmica.

* Con complejidad excesiva.

Una de las propuestas más relevantes de la I.A. es que algunos problemas intratables pueden ser solucionados al incluir y aprovechar el **conocimiento previo** sobre ellos, esto conlleva a aprender los **conceptos** involucrados.

**Definición (Concepto)**

Un concepto es una abstracción de un **conjunto** de objetos (entidades) con propiedades **comunes** y que se pueden **diferenciar** de otros conjuntos.

Por ejemplo, un concepto (una abstracción) de persona podría ser la siguiente:

*Tiene dos ojos* y *Tiene dos manos* y *Tiene dos pies* y *es bípedo* y *es inteligente*.

Cabe señalar que existen distintas formas de representar conceptos.

* Patrones.

* Árboles.

* Proposiciones lógicas, etc...

Al momento de representar un objeto debemos de cuidar los siguientes detalles:

* **Nivel de abstracción** y generalidad de detalle.

* **Validez**: Grado de apego a la realidad.

* **Efectividad**: El desempeño que puede lograr un sistema al explotar esa representación.

**Propiedades de los conceptos**

* Nunca existen de forma aislada.

* Se relacionan con otros conceptos en forma jerárquica.

* No necesariamente son disjuntos.

* Comunmente son difusos.

* Dependen del contexto (dependencia contextual).

**Definición (Lenguaje de descripción)**

Es el mecanísmo explícito para representar **conocomiento**

* Tuplas con parejas atributo-valor.

* Predicados lógicos relacionales.

* Lenguaje natural, etc ...

**Definición (Descripción completa)**

Una descripción de un concepto es **completa** si **cubre** todos los ejemplos **positivos**

**Definición (Descripción consistente)**

Una descripción de un concepto es **consistente** si **no cubre** ejemplo **negativo** alguno.

**Descripción de conceptos, forma caracterizante y forma discriminante**

Existen dos formas básicas para describir los conceptos **aprendidos**.

* **Forma caracterizante**: Identifican las propiedades específicas que satisfacen las instancias de algún concepto.

* **Forma discriminante**: Conjunto de pruebas que permiten distinguir (discriminar) a las instancias que pertenecen a un concepto y las de los otros conceptos.

**Inducción**

Los métodos simbólicos utilizan un **ranzonamiento inductivo**, es decir, partiendo de ejemplos específicos se busca llegar a teorías o leyes generales.

El **ranzonamiento inductivo** se caracteriza por **preservar la falsedad**, esto es:

Si alguno de los ejemplos está mal entiquetado, entonces cualquier generalización obtenida a partir de los ejemplos preserva el error.

Incluso si todos los ejemplos están correctamente etiquetados, cualquier nuevo elemento que no sea cubierto por la generalización aprendida será clasificado incorrectamente.

**Tipos de ranzonamiento**

* **Deductivo**: De lo general a lo particular (preserva verdad).

* **Inductivo**: De lo particular a lo general (preserva falsedad).

* **Abductivo**: La explicación más plausible (preserva falsedad).

En los sistemas inductivos, las **generalizaciones** se deben de representar en la **misma forma** que los ejemplos de donde se aprendió.

A pesar de sus deficiencias, los sistemas inductivos son útiles en los casos en los que no se dispone de suficiente **conocimiento previo** o teoría al respecto del problema a tratar.


## Presentación 03 - Relación con reconomiciento de patrones.

Como se mencionó en la *presentación 01*, aprender es utilizar la información del pasado con el fin de guiar las nuevas decisiones.

Es decir, para aprender (cognición) es necesario **percibir** y **retener** los **patrones** (representación del objeto).

Cada atributo que se percibe es un **concepto abstracto** con un conjunto de valores específicos.

**Definición (Espacio)**

Un espacio es el **producto cruz** entre dos o más conjuntos cualesquiera.

**Definición (Rasgo descriptivo)**

Variable que representa cada uno de los **atributos percibidos** en un objeto.

**Definición (Dominio)**

Es el conjunto de posibles valores que puede tomar un **rasgo descriptivo**.

**Definición (patrón)**

Un patrón es un elemento del espacio formado por el producto cruz de los dominios de todos sus rasgos descriptivos.

Cada objeto de estudio se representa con un patrón único.

**Comparación de patrones**

Para comparar dos patrones, es posible utilizar una **función de semejanza** o una **función de diferencia**

* Rango de una **función de semejanza** $[0,1]$.

* Rango de una **función de diferencia** $[0,\infty)$.

Cuando tenemos dos patrones en **el mismo espacio**, la comparación puede realizarse rasgo a rasgo, esto es, definiendo **funciones de semejanza** o **funciones de diferencia** por cada **rasgo descriptivo** y agregando cada una de estas funciones en una **función de semejanza sintáctica** o una **función de distancia sintáctica**

**Definición (Función de distancia sintáctica)**
Sea $g_{i}(a_{i},b_{i})$ una función de distancia para el rasgo descriptivo $i$ entre un conjunto de $r$ rasgos, la función de distancia sintáctica entre los patrones $A$ y $B$ se define como

$$D_{S}(A,B)=\sum_{i}^{r}g_{i}(a_{i},b_{i})$$

**Definición (Función de semejanza sintáctica)**
Sea $g_{i}(a_{i},b_{i})$ una función de semejanza para el rasgo descriptivo $i$ entre un conjunto de $r$ rasgos, la función de semejanza sintáctica entre los patrones $A$ y $B$ se define como

$$D_{S}(A,B)=\dfrac{1}{r} \sum_{i}^{r}g_{i}(a_{i},b_{i})$$
