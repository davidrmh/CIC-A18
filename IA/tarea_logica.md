## Tarea
### David Ricardo Montalván Hernández

* $[(x \wedge \sim y) \rightarrow (x \rightarrow y) ]$

**Forma directa**

1. $(x \wedge \sim y) \rightarrow F$ premisa.

2. $(\sim x \vee y ) \vee F$ utilizando 1 y equivalencia lógica de la implicación.

3. $(\sim x \vee y )$ utilizando 2 y equivalencia de la disyunción.

4. $(x \rightarrow y)$ utilizando 3 y equivalencia de la implicación.


**Forma indirecta por contradicción**

1. $(x \wedge \sim y) \rightarrow F$ premisa.
2. $\sim (x \rightarrow y)$ negación de la conclusión.
3. $\sim (\sim x \vee y) \equiv (x\wedge \sim y)$ negación de 2.
4. $F$ utilizando 1.
