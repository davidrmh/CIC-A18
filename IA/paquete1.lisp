;Ejercicio 1a)
(setq x  (list '((1 2) 3) 4 '(5 (6)) 'A '(B C) 'D '(E '(F G))))
(format t "Pregunta 1a ~% El quinto elemento se obtiene con ~s~%" '(first (cddddr x)))
(write (first (cddddr x)))
(format t "~%")

;Ejercicio 1b)
(format t "Pregunta 1b ~% El número de segundos se obtiene con ~s~%" '(setq segundos (* 366 24 60 60)))
(write (setq segundos (* 366 24 60 60)))
(format t "~%")

;Ejercicio 1c)
(setq x 1)
(setq y 2)
(AND (/= x 0) (<= x y))
(format t "Pregunta 1c ~% La expresión es ~s" '(AND (/= x 0) (<= x y)))

;Ejercicio 1d)
(list (/ (+ -7 (sqrt (- (* 7 7) (* 4 2 5)))) (* 2 2)) (/ (- -7 (sqrt (- (* 7 7) (* 4 2 5)))) (* 2 2)))

;Ejercicio 2a)
(+ (* 2 4) 6 -8)
;También
(+ (* 2 4) (+ 6 -8))

;Ejercicio 2b)
(/ (+ 5 -3 4) (+ 6 (/ 2 5)))

;Ejercicio 2c)
;numerador (+ (* -1 (+ -4 (* -1 (/ 3 8)))) 1.4502)
;denominador (* -1 (expt 1 (expt (+ 3 -5) (/ 1 3))))
(sqrt (/ (+ (* -1 (+ -4 (* -1 (/ 3 8)))) 1.4502) (* -1 (expt 1 (expt (+ 3 -5) (/ 1 3))))))

;Ejercicio 2d)
(expt (/ (expt (/ 65.402 #C(0 1)) (/ 1 5)) 0.17) (/ 1 7))

;Ejercicio 3a
(cdar '((one two) three four)) ;El resultado es (TWO)
;Ejercicio 3b)
(append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))
;El resultado es ((EVA LISA) KARL SVEN EVA LISA KARL SVEN)

;Ejercicio 3c)
(subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
;subst realiza substitución en un árbol la sintáxis es
;(subst nuevo anterior arbol)
;El resultado es (EVA GITAN LISA GITAN KARIN)

;Ejercicio 3d)
(remove 'sven '(eva sven lisa sven anna))
;El resultado es (EVA LISA ANNA)

;Ejercicio 3e)
(butlast '(karl adam nilsson gregg alisson vilma) 3)
;(KARL ADAM NILSSON)
;Excluye los últimos k elementos de una lista

;Ejercicio 3f)
(nth 2 '(a b c d e))
;La respuesta es C

;Ejercicio 3g)
(nthcdr 2 '(a b c d e))
;La función ejecuta cdr k-veces
;La respuesta es (C D E)

;Ejercicio 3h)
(intersection '(a b c) '(x b z c))
;La respuesta es (B C)

;Ejercicio 3i)
(cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8)))))
;La respuesta es (4)

;Ejercicio 4)
(setq mi-lista (list '(A . 1) '(B . 2) '(C . 3)))

(defun Recombina(lista)
  (setq x (rest (first lista)))
  (setq y (rest (second lista)))
  (setq z (rest (third lista)))
  (setq aux1 (cons (list x y) (first (first lista))))
  (setq aux2 (cons (list y z) (first (third lista))))
  (setq aux3 (cons (list z y x) (first (second lista))))
  (cons aux1 (cons aux2 (cons aux3 nil)))
)

;Ejercicio 5

(defun RealNoCero?(N)
  (AND (realp N) (/= n 0))
  )

;Ejercicio 6
(defun Analiza(x)
  (list (atom x) (numberp x) (listp x) (consp x) (null x)
    )
  )
