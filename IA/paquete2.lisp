;Paquete 2
;David Ricardo Montalván  Hernández

;EJERCICIO 1
(defun eleminpos (elem lista pos)
 (let ((contador-pos 0) (flag nil))
   (loop for elemento-lista in lista
     do
      (setq contador-pos (+ contador-pos 1))
      (when (and (equal elemento-lista elem) (= contador-pos pos))
        (format t "El elemento ~a si está en la lista en la posición ~a" elem pos)
        (setq flag t)
        )
     )
    flag
  )
)

;EJERCICIO 2

;EJERCICIO 3

;EJERCICIO 4
(defun primer-impar(lista)
  (let ((nueva-lista (list)) (indice 0) (flag t))
    (loop for elemento in lista
      do
        (when (and (not (equal (mod elemento 2) 0)) flag)
          (setq nueva-lista (list elemento indice))
          (setq flag nil)
          )
        (setq indice (+ indice 1))
      )
      nueva-lista
    )
  )

;EJERCICIO 5

(defun ultimo-elemento(lista)
  (let ((nueva-lista (list)) (conteo 0) (inicio nil) (numero nil) (flag t))
  (setq inicio (- (length lista) 1))
  ;Empieza a iterar desde el final de la lista
  (loop for i downfrom inicio to 0 do
    (setq numero (nth i lista))
    ;Encuentra el primer número que cumple las condiciones
    (when (and (realp numero) (>= numero 0) flag)
        (loop for elemento in lista do
          (when (= elemento numero)
            (setq conteo (+ conteo 1))
            )
          )
          (setq nueva-lista (list numero conteo))
          (setq flag nil)
      )
    )
    nueva-lista
  )
)

;EJERCICIO 6
(defun conteo(lista)
  (let ((conteo-numeros 0))
    (loop for elemento in lista do
      (when (numberp elemento)
        (setq conteo-numeros (1+ conteo-numeros))
        )
      )
      (cons conteo-numeros (- (length lista) conteo-numeros))
    )
  )
