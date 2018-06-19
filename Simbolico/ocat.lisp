;;==============================================================================
;;                            VARIABLES GLOBALES
;;==============================================================================
(defparameter *indices-atributos* '(1 2 3 4 5))
(defparameter *operador* 'equal)
(defparameter *indice-clase* 7)
(defparameter *m* 10)
(defparameter *ejemplo* '((0.25 1.50 1)
                          (0.75 1.50 1)
                          (1.00 1.50 1)
                          (0.50 1.25 1)
                          (0.75 1.0 1)
                          (1.25 1.0 1)
                          (1.75 1.0 1)
                          (0.25 0.75 1)
                          (1.0 0.75 1)
                          (1.5 0.75 1)
                          (0.5 0.5 1)
                          (1.25 1.25 0)
                          (1.50 1.0 0)
                          (2.25 1.0 0)
                          (1.75 0.75 0)
                          (1.25 0.5 0)
                          (2.25 0.5 0)
                          (2.75 0.5 0)
                          (1.25 0.25 0)
                          (1.75 0.25 0)
                          (2.25 0.25 0) ) ) ;;Ejemplo visto en clase
(defparameter *max-long-clasu* 5) ;;Longitud máxima de cada cláusula

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
      (if (not (find clase-aux clases :test #'equal)) (setq clases (append clases  (list clase-aux)))) );loop
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
      (if (not (find numero indices :test #'equal)) (push numero indices) )

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
      (if (not (find i indices-entrena :test #'equal)) (push (nth i datos) prueba ) )
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
      (if (not (find (nth indice observacion) valxi :test #'equal )) (push (nth indice observacion) valxi ))
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

      (if (equal (nth indice-clase observacion) clase)
        (setq positivas (append positivas (list (nth i tabla)) ) )
      (setq negativas (append negativas (list (nth i tabla)) ) ) )

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

;;==============================================================================
;; División protegida (para evitar denominador igual a cero)
;;==============================================================================
(defun div (numerador denominador)
  (let ((resultado 0))
    (if (= denominador 0) (setq resultado 10000000) )
    (if (/= denominador 0) (setq resultado (/ numerador denominador))  )
  resultado
  );let
);defun

;;==============================================================================
;; Función para calcular aptitudes
;;
;; ENTRADA
;; tabla-positivas, tabla-negativas: Listas creadas con la función separa-tabla
;; indices: Lista de índices para incluir o excluir
;;
;; SALIDA
;; Lista con los siguientes componentes
;; (first) lista-aptitudes: Lista con la aptitud para cada índice. El i-índice
;; de esta lista se refiere a:
;; Aptitud obtenida con la fórmula |pos(xi)| / |neg(xi)| si i es par
;; Aptitud obtenida con la fórmula |pos(¬xi)| / |neg(¬xi)| si i es impar
;; (second) aux: Lista de la forma (0 0 1 1 2 2 ...) que sirve para identificar
;; que índice corresponde a cada aptitud y si se utilizó xi (i par) o ¬xi (i impar)
;;==============================================================================
(defun aptitudes (tabla-positivas tabla-negativas indices)
  (let ((lista-aptitudes nil) (aux nil) (pos nil) (neg nil)
    (pos-neg nil) (neg-neg nil) (aptitud 0) (aptitud-neg 0)  )

      (loop for indice in indices do
        ;|pos(xi)|
        (setq pos (first (conteo tabla-positivas indice :pos) ) )

        ;|neg(xi)|
        (setq neg (first (conteo tabla-negativas indice :pos) ) )

        ;|pos(¬xi)|
        (setq pos-neg (first (conteo tabla-positivas indice :neg) ) )

        ;|neg(¬xi)|
        (setq neg-neg (first (conteo tabla-negativas indice :neg) ) )

        ;|pos(xi)| / |neg(xi)|
        (setq aptitud (div pos neg) )

        ;|pos(¬xi)| / |neg (¬xi)|
        (setq aptitud-neg (div pos-neg neg-neg))

        (setq lista-aptitudes (append lista-aptitudes (list aptitud aptitud-neg)))
        (setq aux (append aux (list indice indice)))
      );loop
    (list lista-aptitudes aux)
  );let
);defun

;;==============================================================================
;; Función para encontrar los k elementos más grandes en una lista así como
;; sus índices
;;
;; ENTRADA
;; lista: Lista con números
;; k: Entero.
;;
;; SALIDA
;; Lista con las siguientes componentes
;; (first) k-max: Lista con los k números más grandes
;; (second) argumentos: Lista con los indices que contienen las k posiciones más grandes
;;==============================================================================
(defun k-argmax (lista k)
  (let ((k-max nil) (argumentos nil) (aux-lista nil) (aux-flag nil) )

    ;Copia la lista inicial y la ordena de manera decreciente
    (setq aux-lista (copy-seq lista))
    (setq aux-lista (sort aux-lista '>))

    ;Obtiene los k elementos más grandes
    (loop for i from 0 to (- k 1) do
      (setq k-max (append k-max (list (nth i aux-lista) )) )
    );loop i

    ;Obtiene los índices de los k elementos más grandes en la lista original
    (loop for elemento in k-max do
      ;Aux-flag es para evitar incluir indices relacionados al mismo elemento
      (setq aux-flag nil)

      (loop for i from 0 to (- (length lista) 1) do

        (when (and (= elemento (nth i lista) ) (not aux-flag) (not (find i argumentos :test #'equal) )   )
          (setq argumentos (append argumentos (list i)) )
          (setq aux-flag t)
        );when
      );loop i
    );loop elemento

    (list k-max argumentos)
  );let
);defun

;;==============================================================================
;; Función para actualizar una lista eliminando un elemento dado
;; esta función se utiliza para actualizar la lista de términos por analizar
;;
;; ENTRADA
;; lista: Lista de número
;; eliminar: Término a eliminar de la lista
;;
;; SALIDA
;; nueva-lista: Lista sin el término eliminar
;;==============================================================================
(defun actualiza-terminos (lista eliminar)
  (let ((nueva-lista nil))
    (loop for elemento in lista do
      (if (not (= elemento eliminar))
        (setq nueva-lista (append nueva-lista (list elemento))) )
    );loop
  nueva-lista
  );let
);defun

;;==============================================================================
;; Función para seleccionar un elemento al azar dentro de una lista
;;
;; ENTRADA
;; lista. Lista con los elementos
;;
;; SALIDA
;; elemento: Elemento de la lista seleccionado al azar
;;==============================================================================
(defun selecciona-azar (lista)
  (let ((elemento nil) (n 0) )

    (when (= (length lista) 1 )
        (setq elemento (first lista) )  )

    (when (> (length lista) 1 )
      (setq n (length lista))
      (setq elemento (nth (random n) lista)) )
  elemento
  );let
);defun

;;==============================================================================
;; Función para actualizar una tabla booleanizada quitando las observaciones
;; que cumplen cierta condicion (1 o 0) en un término dado
;;
;; ENTRADA
;; tabla: Lista que representa una tabla booleanizada
;; termino: Entero que representa un índice a comparar para cada observación
;; tipo Etiqueta: :pos => 1, :neg=>0. Para obtener la condición de comparación
;;
;; SALIDA
;; nueva-tabla: Lista similar a tabla pero quitando las observaciones que
;; cumplen la condición
;;==============================================================================
(defun actualiza-tabla (tabla termino tipo)
  (let ((condicion nil) (nueva-tabla nil) )

    ;Determina la condición
    (if (equal tipo :pos) (setq condicion 1) (setq condicion 0) )

    ;Filtra las observaciones que no cumplen la condición en el término dado
    (loop for observacion in tabla do
      (if (not (equal (nth termino observacion) condicion) )
        (setq nueva-tabla (append nueva-tabla  (list observacion ) ) ) )
    );loop
  nueva-tabla
  );let
);defun

;;==============================================================================
;; Función para determinar un selector de una claúsula
;;
;; ENTRADA
;; xi: Representa un término
;; azar: Elemento al azar que representa el índice de una aptitud
;; (algún elemento de lista-aux-mejores)
;;
;; SALIDA
;; selector: Lista de la forma (:neg  termino) o (:pos o termino)
;; dependiendo de si se niega (impar) o no (par) el término
;;==============================================================================
(defun obten-selector (xi azar)
  (let ((selector nil))
    (if (oddp azar ) (setq selector (list :neg xi))
    (setq selector (list :pos xi))  )
    selector
  );let
);defun

;;==============================================================================
;; Función para obtener el índice del atributo relacionado a un índice de la
;; tabla booleanizada
;;
;; ENTRADA
;; indice-tabla: Número que se obtiene de second de un selector
;; entrenamiento: Lista que representa el conjunto de entrenamiento
;;
;; SALIDA
;; indice: Algún número de la lista *indices-atributos*
;;==============================================================================
(defun obten-indice-atributo (indice-tabla entrenamiento)
  (let ((suma 0) (valxi nil))

    (loop for indice in *indices-atributos* do
      ;Obtiene el conjunto val(xi)
      (setq valxi (val-xi entrenamiento indice))

      ;Calcula la suma
      ;esta suma es para saber hasta que índice se a abarco con los conjuntos
      ;val(xi)
      (setq suma (+ suma (length valxi)))

      ;Revisa si ya se llegó al índice del selector
      (if (<= indice-tabla (- suma 1) )
        (return-from obten-indice-atributo indice)  )

    );loop
  );let
);defun

;;==============================================================================
;; Función para obtener el valor del atributo relacionado a un selector
;;
;; ENTRADA
;; indice-tabla: índice relacionado a un selector
;; entrenamiento: lista que representa el conjunto de entrenamiento
;;
;; SALIDA
;; valor: Valor del atributo que está relacionado al selector
;;==============================================================================
(defun obten-valor-atributo (indice-tabla entrenamiento)
  (let ((lista-valxi nil) (valxi nil) (valor nil))

    ;Primero concatena los conjuntos val(xi)
    (loop for indice in *indices-atributos* do
      (setq valxi (val-xi entrenamiento indice) )
      (setq lista-valxi (append lista-valxi valxi))
    );loop

    ;Después extrae el valor correspondiente al índice indice-tabla
    (setq valor  (nth indice-tabla lista-valxi) )
    valor
  );let
);defun


;;==============================================================================
;; Función para convertir un selector para que sea más fácil de interpretar
;;
;; ENTRADA
;; selector: Lista de la forma (:POS NUMERO) o (:NEG NUMERO)
;; entrenamiento: Lista que representa el conjunto de entrenamiento
;;
;; SALIDA
;; humano: Lista de la forma (< INDICE-ATR VALOR) o (>= INDICE-ATR VALOR)
;; en donde INDICE-ATR es un número en la lista *indices-atributos*
;;==============================================================================
(defun convierte-selector (selector entrenamiento)
  (let ((operador nil) (indice-tabla nil) (humano nil) (indice-atributo nil)
        (valor-atributo nil))

    ;Determina si el operador será >= o <
    (if (equal (first selector) :pos ) (setq operador '>=) (setq operador '<))

    ;Extrae el índice del selector
    (setq indice-tabla (second selector))

    ;Obtiene el índice del atributo relacionado al índice de la tabla booleanizada
    (setq indice-atributo (obten-indice-atributo indice-tabla entrenamiento) )

    ;Obtiene el valor del atributo relacionado al selector
    (setq valor-atributo (obten-valor-atributo indice-tabla entrenamiento))

    ;Convierte el selector
    (setq humano `(,operador ,indice-atributo ,valor-atributo))

    humano
  );let
);defun

;;==============================================================================
;; Función para actualizar la tabla de observaciones de la clase negativa (E-)
;;
;; ENTRADA
;; tabla: Lista representando una tabla booleanizada para la clase negativa
;; clausula: Lista representando una claúsula
;;
;; SALIDA
;; nueva-tabla: Lista similar a negativas pero removiendo las observaciones que
;; cumplen los criterios de la negación de la claúsula
;;==============================================================================
(defun actualiza-negativas (tabla clausula)
  (let ((condicion nil) (indice nil) (aux-filtro nil) (pasan-filtro nil)
        (nueva-tabla nil))

    ;Copia la tabla completa
    (setq pasan-filtro (copy-seq tabla))

    ;Comienza aplicar cada término de la cláusula, negándolo primero
    (loop for selector in clausula do

      ;negación del selector
      (if (equal (first selector) :pos ) (setq condicion 0) (setq condicion 1) )

      ;índice del selector
      (setq indice (second selector))

      ;Comienza a revisar cada observación en pasan-filtro y almacena
      ;las que van cumpliendo la condición negada del selector
      ;estas observaciones se quitarán de tabla
      (loop for observacion in pasan-filtro do
        (when (= (nth indice observacion) condicion )
            (setq aux-filtro (append aux-filtro (list observacion) )) )
      );loop observacion

      ;Actualiza lista pasan-filtro
      (setq pasan-filtro (copy-seq aux-filtro))

      ;Reinicia aux-filtro para el siguiente selector
      (setq aux-filtro nil)

    );loop selector

    ;Descarta las observacion en pasan-filtro de la tabla original
    (loop for observacion in tabla do
      (when (not  (find observacion pasan-filtro :test #'equal))
        (setq nueva-tabla (append nueva-tabla (list observacion) ) ) )
    );loop

    nueva-tabla

  );let
);defun

;;==============================================================================
;; Función para obtener la regla de una clase
;;
;; ENTRADA
;; tabla: Lista que representa una tabla booleanizada
;; entrenamiento: Lista que representa el conjunto de entrenamiento
;; clase: Algún elemento que regresa la función obten-clases
;;
;; SALIDA
;; Lista con los siguientes componentes
;; (first) lista-clausulas: Lista de cláusulas, se interpreta como conjunción
;; Los elementos de cada claúsula se interpretan como disyunción
;; (second) lista-clausulas-humano: Lista de claúsulas de fácil interpretación
;;==============================================================================
(defun obten-regla (tabla entrenamiento clase)
  (let ((separacion-tabla nil) (tabla-positivas nil) (tabla-negativas nil)
  (nuevas-negativas nil) (nuevas-positivas nil) (num-terminos 0)
  (indices-terminos nil) (clausula nil) (lista-clausulas nil)
  (lista-aptitudes nil) (m-mejores nil) (termino-azar nil) (selector nil)
  (selector-humano nil) (clausula-humano nil)
  (lista-clausulas-humano nil) (lista-aux-aptitudes nil)
  (lista-aux-mejores nil) (xi nil) )

  ;;Separa la tabla booleanizada en observaciones positivas y en negativas
  (setq separacion-tabla (separa-tabla entrenamiento tabla clase *indice-clase*))
  (setq tabla-positivas (first separacion-tabla))
  (setq tabla-negativas (second separacion-tabla))

  ;;Copia tabla-negativas en nuevas-negativas
  (setq nuevas-negativas (copy-seq tabla-negativas))

  ;;Mientras tabla-negativas no sea nil
  (loop
    ;;Crear una lista con los índices de los posibles términos
    (setq num-terminos (length (first tabla)) )
    (setq indices-terminos nil)
    (loop for i from 0 to (- num-terminos 1) do
      (setq indices-terminos (append indices-terminos (list i) )) )

    ;;Copia tabla-positivas en nuevas-positivas para poder modificarla sin perder los valores iniciales
    (setq nuevas-positivas (copy-seq tabla-positivas) )

    ;;Reinicia la clausula a nil
    (setq clausula nil)
    (setq clausula-humano nil)

    ;;Mientras nuevas-positivas no sea nil
    (loop

      ;;Para cada índice en la lista indices-terminos calcular la aptitud
      (setq lista-aptitudes  (first (aptitudes nuevas-positivas tabla-negativas indices-terminos) ))

      ;;Esta lista es auxiliar para determinar los *m* mejores términos.
      ;;Para cada índice en indices-terminos se obtienen dos aptitudes
      ;;una para xi y otra para ¬xi, por lo tanto esta lista tendrá
      ;;la forma (0 0 1 1 2 2 .... N N)
      (setq lista-aux-aptitudes nil)
      (loop for i in indices-terminos do
        (setq lista-aux-aptitudes (append lista-aux-aptitudes (list i i)) ) )

      ;;Obtiene los *m* mejores términos
      (setq lista-aux-mejores (second (k-argmax lista-aptitudes *m*)))
      (setq m-mejores nil)
      (loop for i in lista-aux-mejores do
        (if (not (find (nth i lista-aux-aptitudes) m-mejores :test #'equal))
         (setq m-mejores (append m-mejores (list (nth i lista-aux-aptitudes) )  ) ) ))

      ;;De la lista lista-aux-mejores se selecciona un término al azar
      (setq termino-azar (selecciona-azar lista-aux-mejores) )

      ;;Obtiene el término asociado a termino-azar
      (setq xi (nth termino-azar lista-aux-aptitudes) )

      ;;Crea el selector y lo agrega a la claúsula actual
      (setq selector (obten-selector xi termino-azar))
      (setq selector-humano (convierte-selector selector entrenamiento) )
      (if (not (find selector clausula :test #'equal) ) (push selector clausula))
      (if (not (find selector-humano clausula-humano :test #'equal))
        (push selector-humano clausula-humano)   )

      ;;Actualiza las observaciones de la clase positiva
      (setq nuevas-positivas (actualiza-tabla nuevas-positivas xi (first selector) ) )

      ;;Quita termino-azar de la lista indices-terminos
      (setq indices-terminos (actualiza-terminos indices-terminos xi))

      ;;Mensaje de información
      (format t "El conjunto E+ todavía tiene ~a observaciones ~% " (length nuevas-positivas) )

      ;;Condición de paro
      (when (equal nuevas-positivas nil) (return t) )

    );loop E+

    ;;Actualiza tabla-negativas y recupera tabla-positivas
    (setq nuevas-negativas (actualiza-negativas nuevas-negativas clausula) )
    (setq tabla-negativas nuevas-negativas)
    (setq nuevas-positivas tabla-positivas)

    ;;Agrega la claúsula a la lista de claúsulas
    (push clausula lista-clausulas)
    (push clausula-humano lista-clausulas-humano)

    ;;Mensaje de información
    (format t "El conjunto E- todavía tiene ~a observaciones ~% " (length tabla-negativas) )

    ;;Condición de paro
    (when (equal tabla-negativas nil) (return t)  )

  );loop tabla-negativas (E-)

  (list lista-clausulas lista-clausulas-humano)
  );let
);defun

;;==============================================================================
;; Función para evaluar un selector sobre una observación dada
;;
;; ENTRADA
;; selector: Una lista de la forma (>= indice-atributo valor) o (< indice-atributo valor)
;; observacion: Una lista que representa una observación
;;
;; SALIDA
;; T si el valor del atributo cumple la condición del selector, NIL en otro caso
;;==============================================================================
(defun evalua-selector (selector observacion)
  (let ((valor-selector nil) (valor-atributo nil) (operador nil) (indice nil))

    ;Obtiene el índice del atributo de interés
    (setq indice (second selector))

    ;Obtiene el valor del selector
    (setq valor-selector (third selector))

    ;Obtiene el operador del selector
    (setq operador (first selector) )

    ;Obtiene el valor del atributo en el índice correspondiente
    (setq valor-atributo (nth indice observacion) )

    (if (eval `(,operador ,valor-atributo ,valor-selector)) t nil)
  );let
);defun

;;==============================================================================
;; Función para decidir si una observación pertenece a la clase positiva o
;; a la clase negativa, de acuerdo a una regla para la clase positiva
;;
;; ENTRADA
;; regla: Regla formada por selectores de la forma
;; (>= indice-atributo valor) o (< indice-atributo valor)
;; (Se obtiene de second de la función obten-regla)
;; observación: Lista que representa una observación
;;
;; SALIDA
;; T si la observación pertenece a la clase positiva, nil en otro caso
;;
;; NOTA
;; La regla se interpreta como conjunción de disyunciones
;;==============================================================================
(defun evalua-regla (regla observacion)
  (let ((flag-clausula nil) (contador 0) )
    (loop for clausula in regla do
      (loop for selector in clausula do
        (when (and (evalua-selector selector observacion) (not flag-clausula) )
            ;flag-clausula sirve sólo contar alguna disyunción
           (setq flag-clausula t)
           ;Contador sirve para contar cuantas conjunciones se cumplen
           (setq contador (+ contador 1)) );when
      );loop selector
      (setq flag-clausula nil)
    );loop clausula

    ;Si se cumple cada una de las conjunciones entonces es de la clase
    ;positiva
    (if (= contador (length regla)) (return-from evalua-regla t)
     (return-from evalua-regla nil)  )

  );let
);defun

;; (loop for observacion in entrena-pos do
;; (format t "~a~%" (evalua-regla regla observacion)))
;;PARA DEBUG
;; (defvar datos) (defvar clases) (defvar separacion) (defvar entrenamiento)
;; (defvar prueba) (defvar tabla) (defvar separacion-tabla) (defvar tabla-positivas)
;; (defvar tabla-negativas) (defvar num-terminos) (defvar indices-terminos)
;; (defvar lista-aptitudes) (defvar m-mejores) (defvar termino-azar) (defvar selector)
;; (defvar nuevas-positivas) (defvar nuevas-negativas) (defvar clausula nil) (defvar lista-clausulas)
;; (defvar negacion)

;;cargar código
;;leer datos con función lee-datos (defvar datos (lee-datos "RUTA"))

;;obtener las clases posibles con función obten-clases
;;  (setq clases (obten-clases datos *indice-clase* ))

;;separar conjunto de prueba y entrenamiento utilizando la función split-data
;;  (setq separacion (split-data datos PROPORCION))
;;  (setq entrenamiento (first separacion))
;;  (setq prueba (second separacion))

;;Con el conjunto de entrenamiento crear la tabla booleanizada
;;  (setq tabla (tabla-booleana entrenamiento *indices-atributos*))

;;Para la clases de interés separar la tabla booleanizada en observaciones positivas y en negativas
;;en este caso es para clase 1
;;  (setq separacion-tabla (separa-tabla entrenamiento tabla 1 *indice-clase*))
;;  (setq tabla-positivas (first separacion-tabla))
;;  (setq tabla-negativas (second separacion-tabla))

;;Copia tabla-negativas en nuevas-negativas
;;  (setq nuevas-negativas (copy-seq tabla-negativas))

;;Mientras tabla-negativas no sea nil

  ;;Crear una lista con los índices de los posibles términos
  ;; (setq num-terminos (length (first tabla)) )
  ;; (setq indices-terminos nil)
  ;; (loop for i from 0 to (- num-terminos 1) do
  ;;  (setq indices-terminos (append indices-terminos (list i) ))
  ;; )

  ;;Copia tabla-positivas en nuevas-positivas para poder modificarla sin perder los valores iniciales
  ;;  (setq nuevas-positivas (copy-seq tabla-positivas) )

  ;;Reinicia la clausula a nil
  ;;   (setq clausula nil)


  ;;Mientras nuevas-positivas no sea nil

      ;;Para cada índice en la lista indices-terminos calcular la aptitud
      ;;  (setq lista-aptitudes  (first (aptitudes nuevas-positivas tabla-negativas indices-terminos) ))

      ;;De lista-aptitudes obtener los *m* mejores (sólo second)
      ;;  (setq m-mejores (second (k-argmax lista-aptitudes *m*)))

      ;;De la lista m-mejores se selecciona un término al azar
      ;; (setq termino-azar (selecciona-azar m-mejores) )

      ;;Crea el selector y lo agrega a la claúsula actual
      ;;  (setq selector (obten-selector termino-azar))
      ;;  (push selector clausula)

      ;;Actualiza las observaciones de la clase positiva
      ;;  (setq nuevas-positivas (actualiza-tabla nuevas-positivas termino-azar (first selector) ) )

      ;;Actualiza las observaciones de la clase negativa
      ;; (if (equal (first selector) :pos) (setq negacion :neg) (setq negacion :pos)  )
      ;; (setq nuevas-negativas (actualiza-tabla nuevas-negativas termino-azar negacion ) )

      ;;Quita termino-azar de la lista indices-terminos
      ;; (setq indices-terminos (actualiza-terminos indices-terminos termino-azar))

  ;;Actualiza tabla-negativas y recupera tabla-positivas
  ;;  (setq tabla-negativas nuevas-negativas)
  ;;  (setq nuevas-positivas tabla-positivas)

  ;;Agrega la claúsula a la lista de claúsulas
  ;;La lista de claúsulas se interpreta como conjunción
  ;;Los elementos de cada claúsula se interpretan como disyunción
  ;;  (push clausula lista-clausulas)
