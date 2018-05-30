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
;; datos. Lista. Lista de observaciones (idealmente el conjunto de entrenamiento)
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
(defun potencia(s)
  (if s (mapcan (lambda (x) (list (cons (car s) x) x))
                (potencia (cdr s)))
      '(())))
