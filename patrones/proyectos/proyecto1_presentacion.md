<!-- $theme: gaia -->
<!--page_number:true-->
# Algoritmo apriori para detectar reglas de asociación en el mercado accionario mexicano.

David Ricardo Montalván Hernández

13 de marzo de 2018

---

### Objetivo

Encontrar reglas de asociación entre series accionarias que cotizan en la Bolsa Mexicana de Valores.


---

### Descripción de los datos

Para cada serie accionaria se calcularon las variaciones diarias de los precios de cierre, $\Delta_{t}$, de la siguiente manera


$$\Delta_{t}=\dfrac{P_{t}-P_{t-1}}{P_{t-1}}$$

en donde $P_{t}$ es el precio de cierre en el día $t$.

Una vez obteniéndose estas variaciones, se procedió a etiquetarlas de acuerdo al signo de $\Delta_{t}$:

---

### Descripción de los datos (Cont'd)

* Si $\Delta_{t}<0$, entonces la etiqueta para la acción $K$ es *Baja_AcciónK*.

* Si $\Delta_{t}>0$, entonces la etiqueta para la acción $K$ es *Sube_AcciónK*.

* Si $\Delta_{t}=0$, entonces la etiqueta para la acción $K$ es *Neutral_AcciónK*.

---

### Descripción de los datos (Cont'd)

Por ejemplo:

|Día|AMXL|CEMEX|
|---|----|-----|
|1|Sube_AMXL|Baja_CEMEX
|2|Baja_AMXL|Baja_CEMEX|
|3|Sube_AMXL|Sube_CEMEX|

Cada día es una "transacción".

---

### Descripción de los datos (Cont'd)

Finalmente, el periodo de entrenamiento fue del 2 de enero de 2014 al último día hábil de diciembre de 2017.

El periodo de prueba abarcó del 2 de enero al 7 de marzo de 2018.
