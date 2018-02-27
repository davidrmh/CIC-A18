;Paquete 3
;David Ricardo Montalván Hernández

;EJERCICIO 1
;El argumento pos es un número entero mayor o igual a 1
;esto es para tener una posición "natural"
(defun eleminpos(ele lista pos)
  (cond ((null lista) nil) ;caso base
        ((and (equal (first lista) ele) (= pos 1)) t)
        (t (eleminpos ele (rest lista) (- pos 1)))
    );cond
  );defun

;EJERCICIO 2
(defun inicio-en(lista elemento)
  (cond
    ((null lista) nil) ;caso base 1
    ((equal (first lista) elemento) lista) ;caso base 2
    (t (inicio-en (rest lista) elemento))
    );cond
  );defun

;EJERCICIO 3
;Los mismo que en el ejercicio 2
;pero se iniciar con la lista en reversa.
(defun termina-en(lista elemento)
  (setq lista (reverse lista))
  (setq lista (inicio-en lista elemento))
  (reverse lista)
  );defun

;EJERCICIO 4
;Se utiliza una función auxiliar para llevar
;el índice de la posición impar
(defun primer-impar-aux(lista indice)
  (cond
    ((null lista) nil)
    ((/= (mod (first lista) 2) 0) (list (first lista) indice));revisa impar
    (t (primer-impar-aux (rest lista) (+ indice 1)))
    );cond
  );defun

(defun primer-impar(lista)
  (primer-impar-aux lista 0)
  );defun

  ;EJERCICIO 5

  ;EJERCICIO 6
  ;Se utiliza una función auxiliar para llevar a cabo el conteo
  ;de los números y las listas.
  (defun aux-conteo(lista conteo-numeros conteo-listas)
  (if (numberp (first lista))
    (setq conteo-numeros (+ conteo-numeros 1))
    );if
  (if (and (listp (first lista)) (not (null (first lista))))
    (setq conteo-listas (+ conteo-listas 1))
    );if
      (cond
        ((null lista) (cons conteo-numeros conteo-listas))
        (t (aux-conteo (rest lista) conteo-numeros conteo-listas))
        );cond
    );defun

  (defun conteo(lista)
    (aux-conteo lista 0 0)
    );defun

  ;EJERCICIO 7
  ;APLANA

  ;EJERCICIO 8
  ;Determinar el mínimo entre columnas y renglones
  ;Llevar un contador para asegurarse que no se rebase este mínimo
  ;Por cada renglón extraer la posición que lleva el contador

  (defun diagonal-aux(lista contador minimo)
    (cond
      ((or (null lista) (>= contador minimo)) nil) ;Casos base
      (t (append (list (nth contador (first lista)))
         (diagonal-aux (rest lista) (+ contador 1) minimo)))
      );cond
    );defun

  (defun diagonal(lista)
    (let ((renglones 0) (columnas 0) (minimo 0) (diagonal-res (list)))
      (setq renglones (length lista))
      (setq columnas (length (first lista)))
      (setq minimo (min renglones columnas))
      (setq diagonal-res (diagonal-aux lista 0 minimo))
      );let
    );defun

  ;EJERCICIO 9
  ;Determinar el tipo de elemento del primer miembro de la lista
  ;Recursión utilizando rest
  ;Utiliza una función auxiliar (sólo por comodidad) para determinar el tipo

  (defun determina-tipo-aux(elemento)
    (typecase elemento
      (null 'N)
      (atom 'A)
      (list 'L)
    );typecase
  );defun

  (defun ejercicio9(lista)
    (cond
      ((null lista) nil)
      (t (append (list (determina-tipo-aux (first lista))) (ejercicio9 (rest lista))))
      );cond
    );defun

  ;EJERCICIO 10
  ;Caso base: si no es número regresar cero

  (defun suma-numérica(lista)
    (cond
        ((null lista) 0)
        ((not (numberp (first lista))) (+ 0 (suma-numérica (rest lista))))
        ((numberp (first lista)) (+ (first lista) (suma-numérica (rest lista))))
      );cond
    );defun

  ;EJERCICIO 11
  ;FILTRA VOCALES

  ;EJERCICIO 12
  (defun filtra-múltiplos(lista entero)
      (cond
        ((null lista) nil)
        ((= (mod (first lista) entero) 0)
          (append (list) (filtra-múltiplos (rest lista) entero))) ;Si es múltiplo
        ((/= (mod (first lista) entero) 0)
          (append (list (first lista)) (filtra-múltiplos (rest lista) entero)));No es múltiplo
      );cond
    );defun

  ;EJERCICIO 13
  ;EJERCICIO 14
  ;EJERCICIO 15

  ;EJERCICIO 16
  (defun cambia(lista elem1 elem2)
    (cond
      ((null lista) nil)
      ((equal (first lista) elem1) (append (list elem2) (cambia (rest lista) elem1 elem2)))
      ((not (equal (first lista) elem1)) (append (list (first lista)) (cambia (rest lista) elem1 elem2)))
    );cond
  );defun

  ;EJERCICIO 17
  ;FIBONACCI

  ;EJERCICIO 18
  ;MAPEA

  ;EJERCICIO 19
  ;APLANA

  ;EJERCICIO 20
  (defun elimina(lista n)
    (cond
        ((null lista) nil)
        ((and (numberp (first lista)) (<= (first lista) n)) (cons (first lista) (elimina (rest lista) n)))
        (t (elimina (rest lista) n))
      );cond
    );defun

  ;EJERCICIO 21
  ;Se utiliza la función auxiliar pega-aux para primero pegar las listas
  ;Después si utiliza la función cambia del ejercicio 16

  (defun pega-aux(lista1 lista2)
    (cond
      ((null lista1) lista2)
      (t (cons (first lista1) (pega-aux (rest lista1) lista2)))
      );cond
    );defun
  (defun pegaycambia(lista1 lista2 elem1 elem2)
    (let ((lista-combinada (list)))
        (setq lista-combinada (pega-aux lista1 lista2))
        (setq lista-combinada (cambia lista-combinada elem1 elem2))
      );let
    );defun

  ;EJERCICIO 22
  ;Primero quitar los no númericos con la función auxiliar quita-no-numéricos
  ;Después aplicar quicksort con la función quicksort-aux

  (defun quita-no-numéricos(lista)
    (cond
        ((null lista) nil)
        ((numberp (first lista)) (cons (first lista) (quita-no-numéricos (rest lista))))
        (t (quita-no-numéricos (rest lista)))
      );cond
    );defun

  (defun particion(lista inicio fin)
    (let ((izq inicio) (der fin) (pivote 0) (aux 0))
      (setq pivote (nth izq lista))

      (loop
        (when (> izq der) (return));condición de salida
        (loop
          (when (>= (nth izq lista) pivote) (return))
          (setq izq (1+ izq))
        );loop
        (loop
          (when (<= (nth der lista) pivote) (return))
          (setq der (1- der))
        );loop
        (when (<= izq der)
          (setq aux (nth izq lista))
          (setf (nth izq lista) (nth der lista))
          (setf (nth der lista) aux)
          (setq izq (1+ izq))
          (setq der (1- der))
        );when
      );loop
      (list izq lista)
    );let
  );defun

  (defun quick-sort-aux(lista inicio fin)
    (let ((izq 0) (resultado (list)))
      (cond
        ((null lista) nil) ;caso base 1
        ;((= (length (list lista)) 1) lista) ;caso base 2
      );cond
      (setq resultado (particion lista inicio fin))
      (setq izq (first resultado))
      (setq lista (second resultado))
      (when (< inicio (1- izq))
        (quick-sort-aux lista inicio (1- izq));Recursión
      );when
      (when (> fin izq)
        (quick-sort-aux lista izq fin);Recursión
      );when
    lista
    );let
  );defun

(defun quick-sort(lista)
  (let((lista-num (list)))
  ;primero quita los dato no numéricos
  (setq lista-num (quita-no-numéricos lista))
  ;después realiza el quicksort
  (setq lista (quick-sort-aux lista-num 0 (1- (length lista-num))))
  );let
);defun
