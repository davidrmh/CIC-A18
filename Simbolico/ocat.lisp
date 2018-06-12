;;==============================================================================
;;                            VARIABLES GLOBALES
;;==============================================================================
(defparameter *indices-atributos* '(1 2 3 4 5 6))
(defparameter *operador* 'equal)
(defparameter *indice-clase* 7)

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
;; Función para crear los conjuntos val(xi)
;;
;; ENTRADA
;; entrenamiento: Lista creada con la función split-data que representa el
;; conjunto de entrenamiento
;; indice: Entero. índice del atributo.
;;
;; SALIDA
;; valxi: Lista con los valores del atributo x_indice ordenada de menor a mayor
;; sin repeticiones
;;==============================================================================
(defun val-xi (entrenamiento indice)
  (let  ((valxi nil))
    ;Obtiene los valores del atributo xi
    ;considerando que no existan repeticiones
    (loop for observacion in entrenamiento do
      (if (not (find (nth indice observacion) valxi)) (push (nth indice observacion) valxi ))
    );loop

    ;Ordena de menor a mayor
    (setq valxi (sort valxi '<))
    valxi
  );let
);defun

;;==============================================================================
;; Función para extraer los valores de un atributo
;;
;; ENTRADA
;; datos: Lista con un conjunto de observaciones
;; indice: Entero. indice del atributo del que se quiere extraer la información
;;
;; SALIDA
;; resultado. Lista con los valores del atributo
;;==============================================================================
(defun extrae-atributo (datos indice)
  (let ((resultado nil))
    (loop for observacion in datos do
      (setq resultado (append resultado (list (nth indice observacion) )))
    );loop
    resultado
  );let
);defun

;;==============================================================================
;; Función para booleanizar el valor de un atributo
;;
;; ENTRADA
;; valor: Número. Valor para comparar
;; valx: Lista creada con la función val-xi
;;
;; SALIDA
;; resultado. Lista con la misma longitud que valx cuya entrada i
;; es igual a 1 si valor >= (nth i valx) y 0 en otro caso
;;==============================================================================
(defun crea-xij (valor valx)
  (let ((resultado nil))
    (loop for val in valx do
      (if (>= valor val) (setq resultado (append resultado (list 1)))
      (setq resultado (append resultado (list 0)))  )
    );loop
    resultado
  );let
);defun

;;==============================================================================
;; Función para booleanizar los datos, se crea la tabla con los datos booleanizados
;;
;; ENTRADA
;; datos: Lista con las observaciones a booleanizar
;; indices-atributos: Lista con los índices de cada atributo en datos
;;
;; SALIDA
;; tabla: Lista. Lista con cada observación de datos booleanizada
;;==============================================================================
(defun tabla-booleana (datos indices-atributos)
  (let ((tabla nil) (aux-renglon nil) (renglon  nil) (valor nil) (ind-reng 0)
    (observacion nil) (valx nil) )
    ;Último índice de los renglones en datos
    (setq ind-reng (- (length datos) 1) )

    (loop for i from 0 to ind-reng do
      ;Extrae la i-ésima observación
      (setq observacion (nth i datos))

      (setq renglon nil)

      (loop for j in indices-atributos do
        ;Extrae el valor del atributo j en la observación
        (setq valor (nth j observacion))

        ;extrae el conjunto val(xj)
        (setq valx (val-xi datos j))

        ;Booleaniza valor de acuerdo a valx
        (setq aux-renglon (crea-xij valor valx))

        ;Agrega al renglón
        (setq renglon (append renglon aux-renglon ))

      );loop j

      ;Agrega renglón a la tabla
      (setq tabla (append tabla (list renglon) ))
    );loop i

    tabla
  );let
);defun

;;==============================================================================
;; Función para separar la tabla booleanizada en observaciones positivas
;; y observaciones negativas
;;
;; ENTRADA
;; datos: Lista con observaciones (el mismo conjunto de datos con el cual)
;; se creó la tabla booleanizada
;; tabla: Lista creada con la función tabla-booleana
;; clase: Símbolo. Símbolo que representa la clase positiva
;; indice-clase: Índice que contiene la clase en la variable datos
;;
;; SALIDA
;; Lista con los siguiente componentes
;; (first) positivas: Elementos de tabla que corresponden a la clase positiva
;; (second) negativas: Elementos de tabla que corresponden a la clase negativa
;;==============================================================================
(defun separa-tabla (datos tabla clase indice-clase)
  (let ((positivas nil) (negativas nil) (num-obs 0) (observacion nil) )
    (setq num-obs (- (length datos) 1))

    (loop for i from 0 to num-obs do
      (setq observacion (nth i datos))

      (if (equal (nth indice-clase observacion) clase) (push (nth i tabla) positivas)
      (push (nth i tabla) negativas) )

    );loop

    (list positivas negativas)
  );let
);defun

;;==============================================================================
;; Función auxiliar para el cálculo de las aptitudes
;; esta función cuenta el número de observaciones cubiertas en un conjunto si
;; se incluye o no un término especificado, es decir, calcula Pos(xi) o Neg(xi)
;;
;; ENTRADA
;; tabla: Lista. Lista creada con la función separa-tabla.
;; indice: Entero. índice del término a analizar
;; tipo: Etiqueta. :pos => se incluye el termino :neg => no se incluye (¬xi)
;;
;; SALIDA
;; Lista con los siguiente componentes
;; (first) Contador: Número de observaciones que se cubren
;; (second) Observaciones: Observaciones que se cubren
;;==============================================================================
(defun conteo (tabla indice tipo)
  (let ((contador 0) (observaciones nil))
    (loop for observacion in tabla do

      ;Se incluye el término (es 1 en la tabla)
      (when (and  (equal (nth indice observacion) 1) (equal tipo :pos))
        (push observacion observaciones)
        (incf contador)
      );when

      ;No se incluye el término (es 0 en la tabla)
      (when (and  (equal (nth indice observacion) 0) (equal tipo :neg))
        (push observacion observaciones)
        (incf contador)
      );when

    );loop
    (list contador observaciones)
  );let
);defun
