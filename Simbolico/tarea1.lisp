;Instrucciones
(format t "Primero introduce los límites de la región del plano cartesiano (X Y)~%")
(format t "Después introduce las esquinas del marco verdadero~%")

;Limites de la región
(format t "Introduce el valor máximo para X~%")
(defvar lim-x)
(setq lim-x (float (read)))
(format t "Introduce el valor máximo para Y~%")
(defvar lim-y)
(setq lim-y (float (read)))

;Esquinas de los rectángulos
;Rectángulo mayor
(format t "Introduce la abscisa de la esquina superior izquierda (exterior) (x1l-v)~%")
(defvar x1l-v)
(setq x1l-v (float (read)))
(format t "Introduce la ordenada de la esquina superior izquierda (exterior) (y1l-v)~%")
(defvar y1l-v)
(setq y1l-v (float (read)))
(format t "Introduce la abscisa de la esquina inferior derecha (exterior) (x1r-v)~%")
(defvar x1r-v)
(setq x1r-v (float (read)))
(format t "Introduce la ordenada de la esquina inferior derecha (exterior) (y1r-v)~%")
(defvar y1r-v)
(setq y1r-v (float (read)))

;Rectángulo menor
(format t "Introduce la abscisa de la esquina superior izquierda (interior) (x2l-v)~%")
(defvar x2l-v)
(setq x2l-v (float (read)))
(format t "Introduce la ordenada de la esquina superior izquierda (interior) (y2l-v)~%")
(defvar y2l-v)
(setq y2l-v (float (read)))
(format t "Introduce la abscisa de la esquina inferior derecha (interior) (x2r-v)~%")
(defvar x2r-v)
(setq x2r-v (float (read)))
(format t "Introduce la ordenada de la esquina inferior derecha (interior) (y2r-v)~%")
(defvar y2r-v)
(setq y2r-v (float (read)))

;Numero de puntos a generar (Entrenamiento)
(format t "Cuantos puntos quieres generar para ENTRENAR? ~%")
(defvar n-puntos)
(setq n-puntos (read))

;Genera puntos aleatorios
;dentro de la región
;El simbolo puntos es una lista
;de la forma ((x1 y1) (x2 y2) ...)
(defun genera-puntos(n lim-x lim-y)
  (let ((puntos (list)) (x nil) (y nil))
    (dotimes (i n)
      (setq x (random lim-x))
      (setq y (random lim-y))
      (setq puntos
        (append puntos (list (list x y))))
      )
    (return-from genera-puntos puntos)
    )
 )

(defvar puntos (list))
(setq puntos (genera-puntos n-puntos lim-x lim-y))

;Clasifica los puntos generados
(defun es-positivo(x y x1l y1l x1r y1r x2l y2l x2r y2r)
  (or
    (and (and (>= x x1l) (<= x x1r)) (or (and (>= y y1r) (<= y y2r)) (and (>= y y2l) (<= y y1l) )))
    (and (>= x x1l) (<= x x2l) (>= y y1r) (<= y y1l))
    (and (>= x x2r) (<= x x1r) (>= y y1r) (<= y y1l))
    )
  )

(defun clasifica(puntos x1l y1l x1r y1r x2l y2l x2r y2r)
  (let ((pos (list)) (neg (list)) (x nil) (y nil))
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
)

(defvar positivos-v)
(defvar negativos-v)
(setq positivos-v (first (clasifica puntos x1l-v y1l-v x1r-v y1r-v x2l-v y2l-v x2r-v y2r-v)))
(setq negativos-v (second (clasifica puntos x1l-v y1l-v x1r-v y1r-v x2l-v y2l-v x2r-v y2r-v)))

;Aprende el marco
(defvar x-positivos (list))
(defvar y-positivos (list))

(loop for punto in positivos-v
  do
    (setq x-positivos (append x-positivos (list (first punto))))
    (setq y-positivos (append y-positivos (list (second punto))))
  )

(defvar x1l-a (reduce #'min x-positivos))
(defvar x1r-a (reduce #'max x-positivos))
(defvar y1l-a (reduce #'max y-positivos))
(defvar y1r-a (reduce #'min y-positivos))

(defvar x-negativos (list))
(defvar y-negativos (list))

(loop for punto in negativos-v
  do
    (setq x-negativos (append x-negativos (list (first punto))))
    (setq y-negativos (append y-negativos (list (second punto))))
  )

(defun encuentra-pequeño-mayor-que(lista numero)
  ;Ordena lista de menor a mayor '#<
  ;Y compara hasta encontrar el primer elemento
  ;en la lista tal que sea mayor a numero
  (loop for i in (sort lista #'<)
    do
      (when (> i numero) (return-from encuentra-pequeño-mayor-que i))
    )
  )

(defun encuentra-grande-menor-que(lista numero)
  ;Ordena lista de mayor a menor '#>
  ;Y compara hasta encontrar el primer elemento
  ;en la lista tal que sea menor a numero
  (loop for i in (sort lista #'>)
    do
      (when (< i numero) (return-from encuentra-grande-menor-que i))
    )
  )

(defvar x2l-a (encuentra-pequeño-mayor-que x-negativos x1l-a))
(defvar x2r-a (encuentra-grande-menor-que x-negativos x1r-a))
(defvar y2l-a (encuentra-grande-menor-que y-negativos y1l-a))
(defvar y2r-a (encuentra-pequeño-mayor-que y-negativos y1r-a))

;Clasifica un nuevo conjunto de puntos de acuerdo a lo aprendido
;Numero de puntos a generar (Prueba)
(format t "Cuantos puntos quieres generar para VALIDAR? ~%")
(setq n-puntos (read))
(setq puntos (list))
(setq puntos (genera-puntos n-puntos lim-x lim-y))

(setq positivos-v (first (clasifica puntos x1l-v y1l-v x1r-v y1r-v x2l-v y2l-v x2r-v y2r-v)))
(setq negativos-v (second (clasifica puntos x1l-v y1l-v x1r-v y1r-v x2l-v y2l-v x2r-v y2r-v)))

(defvar positivos-a (first (clasifica puntos x1l-a y1l-a x1r-a y1r-a x2l-a y2l-a x2r-a y2r-a)))
(defvar negativos-a (second (clasifica puntos x1l-a y1l-a x1r-a y1r-a x2l-a y2l-a x2r-a y2r-a)))

;Calcula el desempeño
;Utilizando una matriz de confusión

;Verdaderos positivos
(defvar verd-pos (length (intersection positivos-v positivos-a :test 'equal)))

;Falsos negativos
(defvar fals-neg (length (intersection positivos-v negativos-a :test 'equal)))

;Falsos positivos
(defvar fals-pos (length (intersection negativos-v positivos-a :test 'equal)))

;Verdaderos negativos
(defvar verd-neg (length (intersection negativos-v negativos-a :test 'equal)))

;Accuracy
(defvar accuracy (/ (+ verd-pos verd-neg) n-puntos))

;Precision Positivos
(if (> (+ verd-pos fals-pos) 0)
  (defvar precision_pos (/ verd-pos (+ verd-pos fals-pos)))
  (defvar precision_pos 0)
  )

;Precision Negativos
(if (> (+ fals-neg verd-neg) 0)
  (defvar precision_neg (/ verd-neg (+ fals-neg verd-neg)))
  (defvar precision_neg 0)
  )

;Recall Positivos
(if (> (+ verd-pos fals-neg) 0)
  (defvar recall_pos (/ verd-pos (+ verd-pos fals-neg)))
  (defvar recall_pos 0)
  )

;Recall Negativos
(if (> (+ fals-pos verd-neg) 0)
  (defvar recall_neg (/ verd-neg (+ fals-pos verd-neg)))
  (defvar recall_neg 0)
  )

(format t "~%~%Para ~D puntos simulados el desempeño fue~%" n-puntos)
(format t "Accuracy = ~F~%" accuracy)
;(format t "Precision positivos = ~F~%" precision_pos)
;(format t "Precision negativos = ~F~%" precision_neg)
;(format t "Recall positivos = ~F~%" recall_pos)
;(format t "Recall negativos = ~F~%" recall_neg)
(format t "Verdaderos positivos = ~D~%De un total de ~D casos positivos~%" verd-pos (length positivos-v))
(format t "Falsos negativos = ~D~%" fals-neg)
(format t "Falsos positivos = ~D~%" fals-pos)
(format t "Verdaderos negativos = ~D~%De un total de ~D casos negativos~%" verd-neg (length negativos-v))
(format t "~%El concepto aprendido fue: ~%")
(format t "Esquina superior izquierda (exterior) x1l = ~F y1l= ~F~%" x1l-a y1l-a)
(format t "Esquina inferior derecha (exterior) x1r = ~F y1r= ~F~%" x1r-a y1r-a)
(format t "Esquina superior izquierda (interior) x2l = ~F y2l= ~F~%" x2l-a y2l-a)
(format t "Esquina inferior derecha (interior) x2r = ~F y2r= ~F~%" x2r-a y2r-a)
(format t "~%El concepto real es ~%")
(format t "Esquina superior izquierda (exterior) x1l = ~F y1l= ~F~%" x1l-v y1l-v)
(format t "Esquina inferior derecha (exterior) x1r = ~F y1r= ~F~%" x1r-v y1r-v)
(format t "Esquina superior izquierda (interior) x2l = ~F y2l= ~F~%" x2l-v y2l-v)
(format t "Esquina inferior derecha (interior) x2r = ~F y2r= ~F~%" x2r-v y2r-v)
