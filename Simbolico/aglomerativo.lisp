;David R. Montalván Hernández
;Agrupamiento jerárquico aglomerativo

;Algoritmo
;1.Leer los datos (arreglo)
;2.Calcular la matriz de distancias inicial
;3.Agregar etiquetas iniciales
;4.Encontrar mínimo
;5.Actualizar dendrograma
;6.Actualizar matriz de distancias
;7.Ir al paso 4
;8.Repetir hasta que el tamaño de la matriz de distancias sea 2x2

;Distancia para atributos buying y maint
(defun dist-buy(atr1 atr2)
  "Calcula la distancia para los atributos
  buying price y maintenance price
  Los valores que pueden tomar estos atributos son:
  v-high,high,med,low
  "

  (cond
    ((or (and (equal atr1 "vhigh") (equal atr2 "high"))
        (and (equal atr2 "vhigh") (equal atr1 "high"))) 0.5)
    ((or (and (equal atr1 "vhigh") (equal atr2 "med"))
        (and (equal atr2 "vhigh") (equal atr1 "med"))) 2)
    ((or (and (equal atr1 "med") (equal atr2 "low"))
        (and (equal atr2 "med") (equal atr1 "low"))) 2)
    ((or (and (equal atr1 "vhigh") (equal atr2 "low"))
        (and (equal atr2 "vhigh") (equal atr1 "low"))) 10)
    ((or (and (equal atr1 "high") (equal atr2 "med"))
        (and (equal atr2 "high") (equal atr1 "med"))) 2)
    ((or (and (equal atr1 "high") (equal atr2 "low"))
        (and (equal atr2 "high") (equal atr1 "low"))) 5)
    (t 0)));defun

;Distancia para el atributo doors
(defun dist-doors(atr1 atr2)
  (cond
    ((or (and (equal atr1 2) (equal atr2 3))
        (and (equal atr2 2) (equal atr1 3))) 1)
    ((or (and (equal atr1 2) (equal atr2 4))
        (and (equal atr2 2) (equal atr1 4))) 2)
    ((or (and (equal atr1 2) (equal atr2 "5more"))
        (and (equal atr2 2) (equal atr1 "5more"))) 5)
    ((or (and (equal atr1 3) (equal atr2 4))
        (and (equal atr2 3) (equal atr1 4))) 1)
    ((or (and (equal atr1 3) (equal atr2 "5more"))
        (and (equal atr2 3) (equal atr1 "5more"))) 5)
    ((or (and (equal atr1 4) (equal atr2 "5more"))
        (and (equal atr2 4) (equal atr1 "5more"))) 1)
    (t 0)));defun

;Distancia para el atributo persons
(defun dist-pers(atr1 atr2)
  (cond
    ((or (and (equal atr1 2) (equal atr2 4))
        (and (equal atr2 2) (equal atr1 4))) 2)
    ((or (and (equal atr1 2) (equal atr2 "more"))
        (and (equal atr2 2) (equal atr1 "more"))) 5)
    ((or (and (equal atr1 4) (equal atr2 "more"))
        (and (equal atr2 4) (equal atr1 "more"))) 2)
    (t 0)));defun

;Distancia para el atributo lug_boot
(defun dist-lug(atr1 atr2)
  (cond
    ((or (and (equal atr1 "small") (equal atr2 "med"))
        (and (equal atr2 "small") (equal atr1 "med"))) 2)
    ((or (and (equal atr1 "small") (equal atr2 "big"))
        (and (equal atr2 "small") (equal atr1 "big"))) 5)
    ((or (and (equal atr1 "med") (equal atr2 "big"))
        (and (equal atr2 "med") (equal atr1 "big"))) 2)
    (t 0)));defun

;Distancia para el atributo safety
(defun dist-safe(atr1 atr2)
  (cond
    ((or (and (equal atr1 "low") (equal atr2 "med"))
        (and (equal atr2 "low") (equal atr1 "med"))) 2)
    ((or (and (equal atr1 "low") (equal atr2 "high"))
        (and (equal atr2 "low") (equal atr1 "high"))) 5)
    ((or (and (equal atr1 "med") (equal atr2 "high"))
        (and (equal atr2 "med") (equal atr1 "high"))) 2)
    (t 0)));defun

;Calcula la distanacia sintáctica
(defun dist-sint(obs1 obs2)
  "Calcula la distancia sintáctica entre dos observaciones
  Cada observación (obs1 y obs2) es un arreglo unidimensional
  las observaciones ya no incluyen la columna de la etiqueta
  "
  (let((lista-dist (list)) (dist 0) (atr1 nil) (atr2 nil))
    ;Calcula la distancia atributo por atributo
    (loop for i from 0 to (- (array-dimension obs1 0) 1) do
      (setq atr1 (aref obs1 i))
      (setq atr2 (aref obs2 i))
      (cond
        ((equal i 0) (setq lista-dist (append lista-dist (list (dist-buy atr1 atr2)))))
        ((equal i 1) (setq lista-dist (append lista-dist (list (dist-buy atr1 atr2)))))
        ((equal i 2) (setq lista-dist (append lista-dist (list (dist-doors atr1 atr2)))))
        ((equal i 3) (setq lista-dist (append lista-dist (list (dist-pers atr1 atr2)))))
        ((equal i 4) (setq lista-dist (append lista-dist (list (dist-lug atr1 atr2)))))
        ((equal i 5) (setq lista-dist (append lista-dist (list (dist-safe atr1 atr2)))))
      ));loop
    ;(format t "~a~%~a~%" obs1 obs2)
    (setq dist (reduce #'+ lista-dist))
    dist
  ));defun

;Crea la matriz de distancias
;Sólo se crea la matriz triangular inferior
(defun matriz-distancias-inicial(datos)
  "Crea la matriz de distancias
  Sólo se crea la matriz triangular inferior
  ENTRADA:
  datos: Arreglo creado con la función lee-separado
  SALIDA:
  mat-dist: Arreglo con las distancias entre cada observación
  sólo se crea la parte inferior de la matriz de distancias.
  "
  (let ((obs1 nil) (obs2 nil) (mat-dist nil) (ncol 0) (nreng 0))
    (setq nreng (array-dimension datos 0)) ;número de renglones
    (setq ncol (array-dimension datos 1));número de columnas
    (setq ncol (1- ncol)) ;este 1- es para no contar la última columna
    (setq mat-dist (make-array (list nreng nreng) :initial-element 0 :adjustable t)) ;dimensiona matriz de distancias

    (loop for i from 0 to (1- nreng) do
      (setq obs1 (make-array ncol))
      ;extrae observación en el renglón i
      (loop for k from 0 to (1- ncol) do
        (setf (aref obs1 k) (aref datos i k)));loop

      (loop for j from 0 to i do
        (setq obs2 (make-array ncol))
        ;extrae la observación de los renglones abajo del renglón j
        (loop for k from 0 to (1- ncol) do
          (setf (aref obs2 k) (aref datos j k)));loop
        ;calcula la distancia sintáctica
        (setf (aref mat-dist i j) (dist-sint obs1 obs2))
      );loop
    );loop
    mat-dist
  );let
);defun

(defun agrega-etiquetas-inicial(matriz)
  "Agrega una etiqueta en la matriz de distancias
  con el fin de identificar las observaciones
  esta etiquieta se agrega en la última columna de la matriz.
  No me afecta ya que sólo estoy considerando la matriz triangular
  inferior.
  "
  (loop for i from 0 to (1- (array-dimension matriz 0)) do
    (setf (aref matriz i (1- (array-dimension matriz 1)))
    (list i)));loop
  matriz
);defun

(defun encuentra-min(matriz)
  "Encuentra el valor de la distancia mínima en la matriz de
  distancias así como las observaciones que se deben de agrupar
  ENTRADA:
  matriz: matriz de distancias (triangular inferior)
  SALIDA:
  una lista con first = distancia mínima
  rest = una lista con los índices de las observaciones que se deben agrupar
  "
  (let ((n-reng 0) (dist-min 0) (cluster '()))
    (setq n-reng (array-dimension matriz 0));renglones (y columnas)

    (loop for i from 1 to (1- n-reng) do
      (setq dist-min (aref matriz 1 0)) ;arbitrario
      (loop for j from 0 to (1- i) do
        (cond
            ;Si es un nuevo mínimo
            ;reinicia el cluster
            ((< (aref matriz i j) dist-min) (setq dist-min (aref matriz i j)) (setq cluster (list i j)))
            ;Si es el mismo mínimo agrega al cluster
            ((= (aref matriz i j) dist-min) (setq cluster (append cluster (list i j))))
        );cond
      );loop
    );loop
  (list dist-min cluster)
  );let
);defun

(defun actualiza-dendro(dendro,lista)
  "Actualiza el dendrograma
  La representación es utilizando una lista de listas de la forma
  ((dist1 (Obs1-Obs2)) (dist2 (Obs3-Obs4))...)
  Cada elemento de la lista se obtiene con la función encuentra-min
  "
  (append (list dendro) (list lista))
)

(defun actualiza-matriz(datos,matriz,cluster))
