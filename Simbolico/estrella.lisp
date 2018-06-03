;;==============================================================================
;;                            VARIABLES GLOBALES
;;==============================================================================
(defparameter *indices-atributos* '(1 2 3 4 5 6 7 8 9))
(defparameter *operador* 'equal)
(defparameter *indice-clase* 0) ;índice de la columna que tiene la clase

;;==============================================================================
;; Función para leer los datos
;; ENTRADA
;; nombre-archivo. String. Ruta de los datos
;;
;; SALIDA
;; datos. Lista. Una lista con cada elemento representando una línea del archivo
;;
;; ESTRUCTURA DEL ARCHIVO
;; El archivo debe de tener la siguiente estructura:
;; (val11 val12 val13...)
;; (val21 val22 val23...)
;;==============================================================================
(defun lee-datos (nombre-archivo)
  (let ((archivo nil) (datos nil) )
    (setq archivo (open nombre-archivo))
    (loop for linea = (read archivo nil nil) while linea do
      (setq datos (append datos (list linea)))
    );loop
    (close archivo)
    datos);let
);defun

;;==============================================================================
;; Función para obtener una lista con las clases posibles (sin repeticiones)
;;
;; ENTRADA
;; datos: Lista. Lista creada con la función lee-datos
;; indice: Entero. Índice (iniciando en 0) de la columna que tiene la clase
;;
;; SALIDA
;; clases: Lista. Lista con las clases en el conjunto de datos
;;==============================================================================
(defun obten-clases (datos indice)
  (let ((clases nil) (clase-aux nil))
    (loop for observacion in datos do
      (setq clase-aux (nth indice observacion))
      (if (not (find clase-aux clases)) (setq clases (append clases  (list clase-aux)))) );loop
  clases
  );let
);defun

;;==============================================================================
;; Función para clasificar ejemplos positivos y negativos de acuerdo a una
;; clase dada
;;
;; ENTRADA
;; datos: Lista. Lista con las observaciones
;; clase: Símbolo. Símbolo que representa la clase positiva. Debe de ser algún
;; símbolo que regresa la función obten-clases
;; indice: Entero. Índice (iniciando en 0) de la columna que tiene la clase
;;
;;
;; SALIDA
;; Una lista con los siguiente elementos
;; (first) positivas. Lista. Lista (subconjuto de datos) con las observaciones
;; que corresponden a la clase positiva
;; (second) negativas. Lista Lista (subconjuto de datos) con las observaciones
;; que corresponden a la clase negativa
;;==============================================================================
(defun separa-positivos (datos clase indice)
  (let ((positivas nil) (negativas nil) (clase-aux nil)  )
    (loop for observacion in datos do

      ;Clase de la observación actual
      (setq clase-aux (nth indice observacion))

      (if (equal clase-aux clase)
        (setq positivas (append positivas  (list observacion)))
        (setq negativas (append negativas (list observacion)))) );loop
  (list positivas negativas)
  );let
);defun

;;==============================================================================
;; Función para generar un conjunto de números (enteros) dentro un rango [0,Max]
;; sin repeticiones
;;
;; ENTRADA
;; total: Entero. Total de números a generar
;; maximo: Entero. Límite superior
;;
;; SALIDA
;; indices: Lista. Lista con los números generados
;;==============================================================================
(defun genera-indices (total maximo)
  (let ((indices nil) (numero nil) )
    (when (= maximo 0) (return-from genera-indices (list 0) ))
    (loop
      ;Genera un número aleatorio en (0,maximo)
      (setq numero (random maximo))

      ;agrega a la lista revisando primer si el número ya se encontraba en ella
      (if (not (find numero indices)) (push numero indices) )

      ;Revisa condición de paro (hasta tener el número total de elementos)
      (when (equal total (length indices)) (return-from genera-indices indices))
     );loop
  );let
);defun

;;==============================================================================
;; Función para crear los conjuntos de prueba y de entrenamiento
;;
;; ENTRADA
;; datos: Lista. Lista creada con la función lee-datos
;; proporcion: Número en (0,1). Proporción del conjunto de entrenamiento
;;
;; SALIDA
;; Lista con los siguiente componentes:
;; (first) entrenamiento: Lista. Observaciones en el conjunto de entrenamiento
;; (second) prueba: Lista. Observaciones en el conjunto de prueba
;;==============================================================================
(defun split-data (datos &optional (proporcion 0.5) )
  (let ((numEntrena 0) (numPrueba 0) (indices-entrena nil)  (entrenamiento nil) (prueba nil) )

    ;Obtiene el tamaño del conjunto de entrenamiento
    (setq numEntrena (floor  (* proporcion (length datos)) ))

    ;Obtiene el tamaño del conjunto de prueba
    (setq numPrueba (- (length datos) numEntrena ) )

    ;Obtiene aleatoriamente los índices de las observaciones del conjunto
    ;de entrenamiento
    (setq indices-entrena (genera-indices numEntrena (- (length datos) 1)  ))

    ;Crea el conjunto de entrenamiento
    (loop for i in indices-entrena do
      (push (nth i datos) entrenamiento  )
    );loop

    ;Crea el conjunto de prueba
    (loop for i from 0 to (- (length datos) 1) do
      (if (not (find i indices-entrena)) (push (nth i datos) prueba ) )
    );loop

    (list entrenamiento prueba)

  );let
);defun


;;==============================================================================
;; Función para elegir una semilla y su clase
;;
;; ENTRADA
;; datos. Lista. Lista de observaciones (idealmente las observaciones de la clase positiva)
;; indice. Entero. Índice (iniciando en 0) de la columna que tiene la clase
;;
;; SALIDA
;; Lista con los siguientes componentes:
;; (first) semilla. Lista. Una observación de la lista datos
;; (second) clase-semilla. Símbolo. La clase de la semilla
;;==============================================================================
(defun obten-semilla (datos indice)
  (let ((semilla nil) (clase-semilla nil) (aux 0) )

    ;Elige al azar una observación del conjunto de datos
    (setq aux  (genera-indices 1 (-  (length datos) 1)) )
    (setq aux (first aux))
    (setq semilla  (nth aux datos))

    ;Extrae la clase de la semilla
    (setq clase-semilla (nth indice semilla) )

    (list semilla  clase-semilla )
  );let
) ;defun

;;==============================================================================
;; Función para obtener la expresión (en sintaxis de LISP) de un selector
;; El selector tendrá la forma
;; (Operador índice-del-atributo valor-de-comparación)
;; Por ejemplo (EQUAL 1 2) => el atributo 1 es igual 2?
;;
;; ENTRADA
;; operador. Símbolo. Símbolo que representa alguna función lógica
;; indice. Entero. Índice del atributo de interés
;; valor. Valor a comparar
;;
;; SALIDA
;; expresion. Cons. Cons con la expresión representando al selector
;;==============================================================================
(defun crea-selector (operador indice valor )
  (let ((expresion nil))
    (setq expresion `(,operador ,indice ,valor)  )
    expresion
  );let
);defun

;;==============================================================================
;; Función para evaluar la expresión de un selector relativa a una observación
;; El selector tendrá la forma
;; (Operador índice-del-atributo valor-de-comparación)
;; Por ejemplo (EQUAL 1 2) => el atributo 1 es igual 2?
;;
;; ENTRADA
;; observacion. Lista. observación a comparar
;; selector. Cons. Cons con la expresión representando al selector
;;
;; SALIDA
;; T si se cumple la condición del selector, NIL en otro caso
;;==============================================================================
(defun evalua-selector (observacion selector)
  (let ((expresion nil) (valor-atributo nil) (operador nil) (indice-atributo nil)
         (valor-comparacion nil))
    (setq operador (first selector))
    (setq indice-atributo (second selector))
    (setq valor-comparacion (third selector))
    (setq valor-atributo (nth indice-atributo observacion))
    (setq expresion `(,operador ',valor-atributo ',valor-comparacion)  )
    (eval expresion)
  );let
);defun

;;==============================================================================
;; Función para crear el conjunto potencia con los elementos de una lista
;; https://rosettacode.org/wiki/Power_set#Common_Lisp
;;
;;  ENTRADA
;; lista. Lista
;;
;; SALIDA
;; Una lista con el conjunto potencia
;;==============================================================================
(defun potencia (s)
  (if s (mapcan (lambda (x) (list (cons (car s) x) x))
                (potencia (cdr s)))
      '(())))

;;==============================================================================
;; Función para crear los complejos de una observación
;;
;; ENTRADA
;; observacion: Lista. Lista que representa una observación
;; indices: Lista. Lista con los índices de los atributos
;; operador. Símbolo. Símbolo que representa alguna función lógica
;;
;; SALIDA
;; Lista anidada en la cual cada elemento representa la conjunción de selectores
;;==============================================================================
(defun crea-complejos (observacion indices operador)
  (let ((complejos nil) (selectores nil) (selector nil) (set-potencia nil)
        (valor-referencia nil)  )
    ;Obtiene el conjunto potencia utilizando la lista de índices
    (setq set-potencia (potencia indices))

    ;Construye los complejos
    (loop for combinacion in set-potencia do
      ;Aquí guardo un conjunto de selectores
      ;se interpreta como la conjunción de cada uno de ellos
      (setq selectores nil)

      (when (not (equal combinacion nil))

        ;Para cada elemento de un elemento del conjunto potencia
        (loop for i in combinacion do
          ;Extrae el valor del atributo i de la observación
          (setq valor-referencia (nth i observacion))
          (setq selector (crea-selector operador i valor-referencia))
          (push selector selectores)
        );loop i

      );when

      (push selectores complejos)
    );loop combinacion

    complejos

  );let
);defun

;;==============================================================================
;; Función para revisar la consistencia de un complejo
;;
;; ENTRADA
;; observaciones: Lista. Lista con el conjunto de observaciones
;; idealmente las observaciones de la clase negativa
;; complejo. Lista. Una lista con selectores
;;
;; SALIDA
;; T si el complejo no cubre alguna observación de la lista de observaciones
;;==============================================================================
(defun es-consistente? (observaciones complejo)
  (let ((filtrados nil) (tmp nil) )
    ;Copia las observaciones en la variable auxiliar filtrados
    (setq filtrados (copy-seq observaciones))

    ;La idea es filtrar los datos utilizando selector por selector
    (loop for selector in complejo do
      (setq tmp nil)
      (loop for observacion in filtrados do

        ;Si la observación cumple la condición del selector
        ;entonces se agrega a tmp, esta variable almacenará
        ;las observaciones que serán verificadas utilizando el siguiente selector
        (if (evalua-selector observacion selector) (push observacion tmp))

      );loop observación

      ;Si ninguna observación cumplió los criterios del selector
      ;entonces el complejo es consistente
      (if (equal tmp nil) (return-from es-consistente? t))

      ;Actualiza la variable filtrados
      ;con las observaciones que cumplen la condición del selector actual
      ;para ser utilizadas con el siguiente selector
      (setq filtrados  (copy-seq tmp) )

    );loop selector

    ;Si el la función se ejecuta hasta este punto
    ;quiere decir que se exploraron todos los selectores
    ;y se encontraron observaciones que cumplían cada una de las
    ;condiciones, es decir, no es consistente
    nil
  );let
);defun

;;==============================================================================
;; Función para obtener los complejos consistentes de una observacion
;;
;; ENTRADA
;; complejos: Lista. Lista con un conjunto de complejos relativos a una
;; observación (idealmente una semilla)
;; observaciones: Lista. Lista con un conjunto de observaciones
;; (idealmente correspondientes a la clase negativa)
;;
;; SALIDA
;; consistentes: Lista. Lista con los complejos consistentes
;;==============================================================================
(defun encuentra-consistentes (complejos observaciones)
  (let ((consistentes nil))
    (loop for complejo in complejos do
      (if (es-consistente? observaciones complejo) (push complejo consistentes) )
    );loop
    consistentes
  );let
);defun

;;==============================================================================
;; Función para encontrar la cobertura de un complejo
;;
;; ENTRADA
;; complejo: Lista. Lista cuyos elementos son selectores
;; (idealmente debe ser un complejo consistente)
;; observaciones. Lista. Lista con un conjunto de observaciones
;; (idealmente observaciones de la clase positiva)
;;
;; SALIDA
;; Lista con componentes:
;; (first) obs-cubiertas. Lista con las observaciones cubiertas por el complejo
;; (second) cobertura. Entero que representa el número de observaciones cubiertas
;;==============================================================================
(defun encuentra-cobertura (complejo observaciones)
  (let ((obs-cubiertas nil) (cobertura 0))
    (loop for observacion in observaciones do
      ;Rehusando la función es-consistente?
      ;Si no es consistente, quiere decir que el complejo cubre
      ;la observación
      (when (not (es-consistente? (list observacion) complejo) )
        (push observacion obs-cubiertas)
        (setq cobertura (1+ cobertura))
      );when
    );loop
    (list obs-cubiertas cobertura)
  );let
);defun

;;==============================================================================
;; Función para calcular el valor utilizado para el criterio LEF
;; Este criterio será Cobertura - Longitud del complejo
;;
;; ENTRADA
;; complejo. Lista. Lista cuyos elementos son selectores
;; (idealmente deber ser un complejo consistente)
;; observaciones. Lista. Lista con un conjunto de observaciones
;; (idealmente observaciones de la clase positiva)
;;
;; SALIDA
;; puntaje. Entero. Número que representa la resta de la cobertura del complejo
;; y su longitud
;;==============================================================================
(defun puntaje-lef (complejo observaciones)
  (let ((longitud 0) (cobertura 0) (puntaje 0) )
    (setq longitud (length complejo))
    (setq cobertura (second (encuentra-cobertura complejo observaciones) ) )
    (setq puntaje (- cobertura  longitud))
    puntaje
  );let
);defun

;;==============================================================================
;; Función para encontrar el mejor complejo de un conjunto de complejos
;;
;; ENTRADA
;; complejos: Lista. Lista con un conjunto de complejos relativos a una
;; observación (idealmente una semilla). Idealmente complejos consistentes
;; observaciones. Lista. Lista con un conjunto de observaciones
;; (idealmente observaciones de la clase positiva)
;;
;; SALIDA
;; mejor-complejo. Lista. Lista cuyos elementos son selectores. Este complejo
;; es el de mejor puntaje de acuerdo al criterio LEF
;;==============================================================================
(defun mejor-complejo (complejos observaciones)
  (let ((mejor-puntaje -100000) (puntaje 0) (mejor-complejo nil) )
    (loop for complejo in complejos do
      (setq puntaje (puntaje-lef complejo observaciones) )
      (when  (and (> puntaje mejor-puntaje) (not (equal complejo nil)))

        (setq mejor-puntaje puntaje)
        (setq mejor-complejo complejo)
      );when
    );loop
    mejor-complejo
  );let
);defun

;;==============================================================================
;; Función para actualizar las observaciones de la clase positiva, eliminando
;; aquellas que son cubiertas por el mejor-complejo de una semilla
;;
;; ENTRADA
;; obs-cubiertas. Lista. Lista con las observaciones que cubre el complejo
;; Se obtiene con la función encuentra-cobertura
;; observaciones. Lista. Lista con las observaciones de la clase positiva
;;
;; SALIDA
;; nuevas-positivas. Lista. Lista similar a observaciones exceptuando aquellas
;; observaciones en obs-cubiertas
;;==============================================================================
(defun actualiza-positivas (obs-cubiertas positivas)
  (let ((nuevas-positivas nil))
    (loop for observacion in positivas do
      ;Revisa si la observación no se encuentra dentro de las
      ;que fueron cubiertas
      (if (not (find observacion obs-cubiertas)) (push observacion nuevas-positivas))
    );loop
  nuevas-positivas
  );let
);defun


;;==============================================================================
;; Función para arreglar las observaciones contradictorias
;;
;; ENTRADA
;; positivas. Lista. Lista con las observaciones de la clase positiva
;; negativas. Lista. Lista con las observaciones de la clase negativa
;;
;; SALIDA
;; Lista con los siguientes elementos
;; (first) Lista con las observaciones de la clase positiva sin contradicciones
;; (second) Lista con las observaciones de la clase negativa sin contradicciones
;;==============================================================================
(defun quita-contradicciones (positivas negativas)
  (let ((positivas-filtradas nil) (negativas-filtradas nil) (atributos-pos nil)
        (atributos-neg nil) (num-pos 0) (num-neg 0) (aux nil) (obs nil))

        ;último índice válido de cada lista de observaciones
        (setq num-pos (- (length positivas) 1 ))
        (setq num-neg (- (length positivas) 1 ))

        ;Primero guarda los atributos (sin clase) de cada observación
        ;Positivas
        (loop for observacion in positivas do
          (setq aux nil)

          (loop for i in *indices-atributos* do
            ;Extrae cada valor de la observación
            (setq aux (append aux (list (nth i observacion) )))
          );loop
          (push aux atributos-pos)
        );loop

        ;negativas
        (loop for observacion in negativas do
          (setq aux nil)

          (loop for i in *indices-atributos* do
            ;Extrae cada valor de la observación
            (setq aux (append aux (list (nth i observacion) )))
          );loop
          (push aux atributos-neg)
        );loop

        ;Después filtra eliminando contradicciones
        ;Positivas
        (loop for i from 0 to num-pos do
          (setq obs (nth i positivas))
          (setq aux (nth i atributos-pos))

          ;Revisa si es una observación cuyos atributos no los
          ;tiene otra observación de la clase contraria
          (if (not (find aux atributos-neg)) (push obs positivas-filtradas)  )
        );loop

        ;Negativas
        (loop for i from 0 to num-neg do
          (setq obs (nth i negativas))
          (setq aux (nth i atributos-neg))

          ;Revisa si es una observación cuyos atributos no los
          ;tiene otra observación de la clase contraria
          (if (not (find aux atributos-pos)) (push obs negativas-filtradas) )
        );loop
    (list positivas-filtradas negativas-filtradas)

  );let
);defun


;;==============================================================================
;; Función para obtener la estrella de una clase a partir de las observaciones
;; de la clase positiva y la clase negativa
;;
;; ENTRADA
;; positivas: Lista. Lista que contiene las observaciones de la clase positiva
;; negativas: Lista. Lista que contiene las observaciones de la clase negativa
;;
;; SALIDA
;; Estrella: Lista de complejos. Se puede interpretar como la disyunción de cada uno de
;; ellos
;;==============================================================================
(defun obten-estrella (positivas negativas)
  (let ((estrella nil) (semilla nil) (obs-cobertura nil) (complejos nil)
        (mejor-complejo nil) )
      (loop

        ;Encuentra semilla
        (setq semilla (first (obten-semilla positivas *indice-clase*)))

        ;Encuentra los complejos que cubren a la semilla
        (setq complejos (crea-complejos semilla *indices-atributos* *operador*))

        ;Encuentra los complejos consistentes
        (setq complejos (encuentra-consistentes complejos negativas))

        ;Encuentra el mejor complejo (consistente) de acuerdo al criterio LEF
        (setq mejor-complejo (mejor-complejo complejos positivas))

        ;Obtiene las observaciones que son cubiertas por el mejor-complejo
        (setq obs-cobertura (first (encuentra-cobertura mejor-complejo positivas) ))

        ;Añade el mejor complejo a la estrella

        (if (not (equal mejor-complejo nil)) (push mejor-complejo estrella) )

        ;Actualiza las observaciones de la clase positiva
        (setq positivas (actualiza-positivas obs-cobertura positivas))

        (when (equal positivas nil) (return-from obten-estrella estrella) )
      );loop
    estrella
  );let
);defun


;;==============================================================================
;; Función para evalauar la cobertura de una estrella para una observación
;;
;;  ENTRADA
;;  observacion. Lista. observación a comparar
;;  estrella. Lista creada con la función obten-estrella
;;
;;  SALIDA
;;  Booleano. T si la estrella cubre la observación, nil en otro caso.
;;
;;==============================================================================
(defun evalua-observacion (observacion estrella)
  (let ((longitud-complejo 0) (contador-positivos 0))
      (loop for complejo in estrella do
          (setq longitud-complejo (length complejo))
          (setq contador-positivos 0)

          (loop for selector in complejo do
            (if (evalua-selector observacion selector) (setq contador-positivos (1+ contador-positivos)))
          );loop selector

          (if (= contador-positivos longitud-complejo)
             (return-from evalua-observacion t) )
      );loop complejo
    nil
  );let
);defun

;;==============================================================================
;; PENDIENTE
;; Simplificar reglas
;; Main
;; Evaluar conjunto de prueba y medidas de desempeño
;;==============================================================================

;(H D C B E F) (problemas con clase D)
;lee-datos
;(setq split (split-data datos))
;(setq entrenamiento (first split))
;(setq prueba (second split))
;(setq separacion (separa-positivos entrenamiento 'F 0))
;(setq positivas (first separacion))
;(setq negativas (second separacion))
;(setq filtradas (quita-contradicciones positivas negativas))
;(setq positivas (first filtradas))
;(setq negativas (second filtradas))
;(obten-estrella positivas negativas)

;H (((EQUAL 2 X)))
;D (((EQUAL 4 3) (EQUAL 3 1) (EQUAL 2 I) (EQUAL 1 A)))
;C (((EQUAL 6 1) (EQUAL 3 1) (EQUAL 2 O) (EQUAL 1 A)))
;B (((EQUAL 1 X)))
;E (((EQUAL 8 1) (EQUAL 4 2) (EQUAL 1 K)) ((EQUAL 8 1) (EQUAL 2 C)))
;F (((EQUAL 4 2) (EQUAL 3 2) (EQUAL 1 A)))
