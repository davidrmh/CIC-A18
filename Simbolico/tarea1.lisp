;Limites de la región
(setq lim-x 5.0)
(setq lim-y 5.0)

;Numero de puntos a generar
(format t "Cuantos puntos quieres generar? ~%")
(setq n-puntos (read))

;Esquinas de los rectángulos
;Rectángulo mayor
(format t "Introduce la abscisa de la esquina superior izquierda 1 (x1l-v)~%")
(setq x1l-v (read))
(format t "Introduce la ordenada de la esquina superior izquierda 1 (y1l-v)~%")
(setq y1l-v (read))
(format t "Introduce la abscisa de la esquina inferior derecha 1 (x1r-v)~%")
(setq x1r-v (read))
(format t "Introduce la ordenada de la esquina inferior derecha 1 (y1r-v)~%")
(setq y1r-v (read))

;Rectángulo menor
(format t "Introduce la abscisa de la esquina superior izquierda 2 (x2l-v)~%")
(setq x2l-v (read))
(format t "Introduce la ordenada de la esquina superior izquierda 2 (y2l-v)~%")
(setq y2l-v (read))
(format t "Introduce la abscisa de la esquina inferior derecha 2 (x2r-v)~%")
(setq x2r-v (read))
(format t "Introduce la ordenada de la esquina inferior derecha 2 (y2r-v)~%")
(setq y2r-v (read))

;Genera puntos aleatorios
;dentro de la región
;El simbolo puntos es una lista
;de la forma ((x1 y1) (x2 y2) ...)
(setq puntos (list))

(dotimes (i n-puntos)
  (setq x (random lim-x))
  (setq y (random lim-y))
  (setq puntos
    (append puntos (list (list x y))))
  )

;Clasifica los puntos generados
(defun es-positivo(x y x1l y1l x1r y1r x2l y2l x2r y2r)
  (or
    (and (and (>= x x1l) (<= x x1r)) (or (and (>= y y1r) (<= y y2r)) (and (>= y y2l) (<= y y1l) )))
    (and (>= x x1l) (<= x x2l) (>= y y1r) (<= y y1l))
    (and (>= x x1l) (<= x x2l) (>= y y1r) (<= y y1l))
    )
  )

(defun clasifica(puntos x1l y1l x1r y1r x2l y2l x2r y2r)
  (setq pos (list))
  (setq neg (list))
  (loop for punto in puntos
    do
        (setq x (first punto))
        (setq y (second punto))
        (if (es-positivo x y x1l y1l x1r y1r x2l y2l x2r y2r)
          (setq pos (append pos (list (list x y))))
          (setq neg (append neg (list (list x y))))
          )
    )
  (return-from clasifica (list pos neg))
)

(setq positivos-v (first (clasifica puntos x1l-v y1l-v x1r-v y1r-v x2l-v y2l-v x2r-v y2r-v)))
(setq negativos-v (second (clasifica puntos x1l-v y1l-v x1r-v y1r-v x2l-v y2l-v x2r-v y2r-v)))

;Aprende el marco
(setq x-positivos (list))
(setq y-positivos (list))

(loop for punto in positivos-v
  do
    (setq x-positivos (append x-positivos (list (first punto))))
    (setq y-positivos (append y-positivos (list (second punto))))
  )

(setq x1l-a (reduce #'min x-positivos))
(setq x1r-a (reduce #'max x-positivos))
(setq y1l-a (reduce #'max y-positivos))
(setq y1r-a (reduce #'min y-positivos))

(setq x-negativos (list))
(setq y-negativos (list))

(loop for punto in negativos-v
  do
    (setq x-negativos (append x-negativos (list (first punto))))
    (setq y-negativos (append y-negativos (list (second punto))))
  )

(defun encuentra-mayor-que(lista numero)
  ;Ordena lista de menor a mayor '#<
  ;Y compara hasta encontrar el primer elemento
  ;en la lista tal que sea mayor a numero
  (loop for i in (sort lista #'<)
    do
      (when (> i numero) (return-from encuentra-mayor-que i))
    )
  )

(defun encuentra-menor-que(lista numero)
  ;Ordena lista de mayor a menor '#>
  ;Y compara hasta encontrar el primer elemento
  ;en la lista tal que sea menor a numero
  (loop for i in (sort lista #'>)
    do
      (when (< i numero) (return-from encuentra-menor-que i))
    )
  )

(setq x2l-a (encuentra-mayor-que x-negativos x1l-a))
(setq x2r-a (encuentra-menor-que x-negativos x1r-a))
(setq y2l-a (encuentra-menor-que y-negativos y1l-a))
(setq y2r-a (encuentra-mayor-que y-negativos y1r-a))

;Clasifica de acuerdo a lo aprendido
(setq positivos-a (first (clasifica puntos x1l-a y1l-a x1r-a y1r-a x2l-a y2l-a x2r-a y2r-a)))
(setq negativos-a (second (clasifica puntos x1l-a y1l-a x1r-a y1r-a x2l-a y2l-a x2r-a y2r-a)))

;Calcula el desempeño
;Utilizando una matriz de confusión

;Verdaderos positivos
(setq verd-pos (length (intersection positivos-v positivos-a :test 'equal)))

;Falsos negativos
(setq fals-neg (length (intersection positivos-v negativos-a :test 'equal)))

;Falsos positivos
(setq fals-pos (length (intersection negativos-v positivos-a :test 'equal)))

;Verdaderos negativos
(setq verd-neg (length (intersection negativos-v negativos-a :test 'equal)))

;Accuracy
(setq accuracy (/ (+ verd-pos verd-neg) n-puntos))

;Precision Positivos
(setq precision_pos (/ verd-pos (+ verd-pos fals-pos)))

;Precision Negativos
(setq precision_neg (/ verd-neg (+ fals-neg verd-neg)))

;Recall Positivos
(setq recall_pos (/ verd-pos (+ verd-pos fals-neg)))

;Recall Negativos
(setq recall_neg (/ verd-neg (+ fals-pos verd-neg)))

(format t "Para ~D puntos simulados el desempeño fue~%" n-puntos)
(format t "Accuracy = ~F~%" accuracy)
(format t "Precision positivos = ~F~%" precision_pos)
(format t "Precision negativos = ~F~%" precision_neg)
(format t "Recall positivos = ~F~%" recall_pos)
(format t "Recall negativos = ~F~%" recall_neg)
