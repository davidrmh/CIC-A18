## Tarea
### David Ricardo Montalván Hernández

**------------------------------------------------------------------------**

$$[(x \wedge \sim y) \rightarrow F] \rightarrow (x \rightarrow y) $$

**Forma directa**

1. $(x \wedge \sim y) \rightarrow F$ premisa.

2. $(\sim x \vee y ) \vee F$ utilizando 1 y equivalencia lógica de la implicación.

3. $(\sim x \vee y )$ utilizando 2 y equivalencia de la disyunción.

4. $(x \rightarrow y)$ utilizando 3 y equivalencia de la implicación.


**Forma indirecta por contradicción**

1. $(x \wedge \sim y) \rightarrow F$ premisa.

2. $\sim (x \rightarrow y)$ premisa (negación de la conclusión)

3. $\sim (\sim x \vee y) \equiv (x\wedge \sim y)$ negación de 2.

4. $F$ utilizando 1 y 3.

**-------------------------------------------------------------------------**

$$p \wedge [(p \rightarrow q) \vee (s \wedge r)] \rightarrow (\sim q \rightarrow s)$$

**Forma directa**

1. $p$ premisa.

2. $(p \rightarrow q) \vee (s \wedge r)$ premisa.

3. $(\sim p \vee q) \vee (s \wedge r)$ utilizando 2 y equivalencia de la implicación.

4. $(F \vee q) \vee (s \wedge r)$ utilizando 1 junto con 3.

5. $q \vee (s \wedge r)$ utilizando 3 junto con propiedad de la disyunción.

6. $(q \vee s) \wedge (q \vee r)$ distribuyendo 5.

7. $(q \vee s)$ ya que para que 6 sea cierto se necesita que esto se cumpla.

8. $\sim q \rightarrow s$ utilizando 7 y equivalencia de la implicación.

**Forma indirecta por contradicción**

1. $p$ premisa.

2. $(p \rightarrow q) \vee (s \wedge r)$ premisa.

3. $\sim (\sim q \rightarrow s)$ premisa (negación de la conclusión)

4. $(q \vee s)$ utilizando 2 y el mismo desarrollo de la forma directa.

5. $\sim (\sim q \wedge \sim s)$ utilizando 4 y propiedades de la negación.

6. $\sim (q \vee s)$ utilizando 3 y equivalencia de la implicación.

7. $\sim q \wedge \sim s$ utilizando 6 y propiedades de la negación.

8. $F$ utilizando 5 y 7 (se contradicen).

**----------------------------------------------------------------------**

$$(b \rightarrow f) \wedge (r \vee f) \wedge \sim f \rightarrow (r \wedge \sim b)$$

**Forma directa**

1. $b \rightarrow f$ premisa.

2. $r \vee f$ premisa.

3. $\sim f$ premisa.

4. $f \equiv F$ utilizando 3.

5. $\sim b \vee f$ utilizando 1 y equivalencia de la implicación.

6. $\sim b \vee F$ combinando 4 y 5.

7. $\sim b$ utilizando 6 y propiedad de la disyunción.

8. $r \vee F$ utilizando 2 y 4.

9. $r$ utilizando 8 y propiedad de la disyunción.

10. $r \wedge \sim b$ combinando 7 y 9.

**Forma indirecta por contradicción**

1. $b \rightarrow f$ premisa.

2. $r \vee f$ premisa.

3. $\sim f$ premisa.

4. $\sim (r \wedge \sim b)$ premisa (negación de la conclusión)

5. $\sim r \vee b$ negación de 4.

6. $f \equiv F$ por 3.

7. $r\vee F \equiv r$ combinando 2 y 6, después utilizando propiedad de la disyunción.

8. $F \vee b \equiv b$ combinando 7 y 5, utilizando propiedad de disyunción.

9. $\sim b \vee F \equiv \sim b$ utilizando 1, equivalencia de la implicación y 6.

10. $F$ combinando 8 y 9 (se contradicen).

**--------------------------------------------------------**

$$[(p \rightarrow q) \wedge (q \rightarrow r) \rightarrow (p \rightarrow r)]$$

**Forma directa**

1. $p\rightarrow q$ premisa.

2. $q \rightarrow r$ premisa.

3. $p \rightarrow r$ silogismo hipotético utilizando 1 y 2.

**Forma indirecta por contradicción**


1. $p\rightarrow q$ premisa.

2. $q \rightarrow r$ premisa.

3. $\sim (p \rightarrow r)$ premisa (negación de la conclusión)

4. $\sim (\sim p \vee q) \equiv (p \wedge \sim r)$ negando 3, utilizando primero equivalencia de la implicación.

5. $\sim p \vee q$ utilizando 1 junto con equivalencia de la implicación.

6. $\sim q \vee r$ utilizando 2 junto con equivalencia de la implicación.

7. $p$ utilizando 4.

8. $\sim r$ utilizando 4.

9. $\sim p \vee q \equiv F \vee q \equiv q$ combinando 5 y 7.

10. $\sim q \vee r \equiv \sim q \vee F \equiv \sim q$ combinando 6 y 8.

11. $F$ combinando 9 y 10 (se contradicen)

**---------------------------------------------**

$$[(a \wedge b) \rightarrow c ] \rightarrow [a \rightarrow (b \rightarrow c)]$$

**Forma directa**

1. $(a \wedge b)\rightarrow c$ premisa.

2. $\sim (a \wedge b) \vee c$ utilizando 1 y equivalencia de la implicación.

3. $\sim a \vee \sim b \vee c$ utilizando 2 y propiedades de la negación.

4. $a \rightarrow (\sim b \vee c)$ utilizando 3 y equivalencia de la implicación.

5. $a \rightarrow (b \rightarrow c)$ utilizando 4 y equivalencia de la implicación.

**Forma indirecta por contradicción**

1. $(a \wedge b) \rightarrow c$ premisa.

2. $\sim [a \rightarrow (b \rightarrow c)]$ premisa (negación de la conclusión)

3. $\sim a \vee \sim b \vee c$ desarrollando 1 de la misma forma que en la demostración directa.

4. $\sim [a \rightarrow (\sim b \vee c)]$ utilizando 2 con equivalencia de la implicación.

5. $\sim (\sim a \vee \sim b \vee c)$ utilizando 4 y equivalencia de la implicación.

6. $F$ combinando 3 y 5 (se contradicen)
