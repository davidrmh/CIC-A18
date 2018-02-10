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
