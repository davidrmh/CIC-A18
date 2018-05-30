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
(defun separa-clases (datos clase indice)
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
