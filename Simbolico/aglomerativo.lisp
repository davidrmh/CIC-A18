;David R. Montalván Hernández
;Agrupamiento jerárquico aglomerativo

;Instrucciones
;1.Leer los datos (arreglo)
;2.Calcular la matriz de distancias

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
(defun matriz-distancias(datos)
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
    (setq mat-dist (make-array (list nreng nreng) :initial-element 0)) ;dimensiona matriz de distancias

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
    (return-from matriz-distancias mat-dist)
  );let
);defun
