;Paquete 2
;David Ricardo Montalván  Hernández

;EJERCICIO 1
;La posición (pos) es un número mayor o igual a 1
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
;primero encuentra el índice en el cual
;se encuentra la primer ocurrencia del elemento
;Después iniciando desde el índice encontrado empieza a juntar
;los elementos
(defun inicio-en(lista elemento)
  (let ((flag t) (indice-inicio nil) (conteo-auxiliar 0) (lista-resultado (list)))
  ;Encuentra índice
  ;flag es para considerar la primer ocurrencia
  (loop for elem in lista do
    (when (and (equal elem elemento) flag)
      (setq indice-inicio conteo-auxiliar)
      (setq flag nil)
        );when
    (setq conteo-auxiliar (+ conteo-auxiliar 1))
      );loop
    ;Concatena elementos iniciando desde el índice encontrado
    (when (not (equal indice-inicio nil))
      (loop for i from indice-inicio to (- (length lista) 1) do
        (setq lista-resultado (append lista-resultado (list (nth i lista))))
        );loop
      );when
    lista-resultado
    );let
  );defun

;EJERCICIO 3
;primero encuentra el índice en el cual
;se encuentra la última ocurrencia del elemento
;Después iniciando desde el índice encontrado empieza a juntar
;los elementos
(defun termina-en(lista elemento)
  (let ((indice-fin nil) (conteo-auxiliar 0) (lista-resultado (list)))
  ;Encuentra índice de última ocurrencia
  (loop for elem in lista do
    (when (equal elem elemento)
      (setq indice-fin conteo-auxiliar)
        );when
    (setq conteo-auxiliar (+ conteo-auxiliar 1))
      );loop
    ;Concatena elementos iniciando desde 0 hasta el índice encontrado
    (when (not (equal indice-fin nil))
      (loop for i from 0 to indice-fin do
        (setq lista-resultado (append lista-resultado (list (nth i lista))))
        );loop
      );when
    lista-resultado
    );let
  );defun


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
      );when
    );loop
    nueva-lista
  );let
);defun

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

;EJERCICIO 7
;APLANA

;EJERCICIO 8
(defun diagonal(lista)
  (let ((m 0) (n 0) (diag (list)) (renglon nil))
    ;número de renglones
    (setq m (length lista))
    ;número de columnas
    (setq n (length (first lista)))
    (do ((i 0 (+ i 1)))
      ;La condición de paro es hasta que ya no haya renglones o columnas
      ((or (> i (- n 1)) (> i (- m 1))) diag)

      (setq renglon (nth i lista))
      (setq diag (append diag (list (nth i renglon))))
    );do

  );let
);defun

;EJERCICIO 9
(defun tipo-dato(lista)
  (let ((lista-resultado (list)))
    (loop for elemento in lista do
      (typecase elemento
        ;El orden en que se ponen los elementos importa
        ;Por eso puse primero null, ya que si no '() evalua a atom
        (null (setq lista-resultado (append lista-resultado (list 'N))))
        (atom (setq lista-resultado (append lista-resultado (list 'A))))
        (list (setq lista-resultado (append lista-resultado (list 'L))))
        (t (setq lista-resultado (append lista-resultado (list "Otro Tipo"))))
        );typecase
      );loop
    lista-resultado
    );let
);defun

;EJERCICIO 10
(defun suma-numérica(lista)
  (let ((suma 0))
    (loop for elemento in lista do
      (when (numberp elemento)
        (setq suma (+ suma elemento))
        );when
      );loop
    suma
    );let
  );defun

;EJERCICIO 11

;EJERCICIO 12
(defun filtra-múltiplos(lista numero)
  (let ((lista-resultado (list)))
    (loop for elemento in lista do
      (when (not (= (mod elemento numero) 0))
        (setq lista-resultado (append lista-resultado (list elemento)))
        );when
      );loop
    lista-resultado
    );let
  );defun

;EJERCICIO 13 (VERSIÓN RECURISA)
;Por cada elemento de la lista se tiene una celda de construcción
;Si la lista tiene n elementos y al menos una sublista tiene m elementos
;entonces al menos se tienen n + m celdas.
(defun celdas(lista)
  (when (atom lista)
    (return-from celdas 0)
    );when
  (let ((cuenta 0))
    (setq cuenta (length lista))
    (loop for elemento in lista do
      (setq cuenta (+ cuenta (celdas elemento)))
      );loop
    cuenta
    );let
  );defun

;EJERCICIO 14
;La implicación p->q puede ser representada como p o (not q)

;EJERCICIO 15
;La matriz resultante está formada Por
;las entradas c_{ij} = \sum_{l=1}^{n} a_{il}b_{lj}
;Creé dos funciones auxiliares
;Una para obtener la columna de la matriz derecha
;otra para realizar el producto punto
;entre el renglón de la matriz izquierda
;y la columna de la matriz derecha

(defun obten-columna(lista indice)
  (let ((columna (list)))
    (loop for renglon in lista do
      (setq columna (append columna (list (nth indice renglon))))
      );loop
    columna
    );let
  );defun

(defun producto-punto(lista1 lista2)
  (let ((resultado 0) (n 0))
    (setq n (length lista1))
    (loop for i from 0 to (- n 1) by 1 do
      (setq resultado (+ resultado (* (nth i lista1) (nth i lista2))))
      );loop
    resultado
    );let
  );defun

(defun mult(lista1 lista2)
;lista1 (mxn)
;lista2 (nxk)
;Selecciona renglón
;Selecciona columna
;producto punto
(let ((matriz-resultado (list)) (aux-res (list)) (n 0) (k 0) (renglon (list)) (columna (list)))
  ;Revisa dimensiones
  (setq renglon (nth 1 lista1))
  (setq columna (obten-columna lista2 1))
  (when (/= (length renglon) (length columna))
    (return-from mult nil)
    );when
  (setq n (length lista1))
  (setq k (length (first lista2)))
  (loop for renglon in lista1 do
    (setq aux-res (list))
    (loop for i from 0 to (- k 1) by 1 do
      (setq columna (obten-columna lista2 i))
      (setq aux-res (append aux-res (list (producto-punto renglon columna))))
      );loop
    (setq matriz-resultado (append matriz-resultado (list aux-res)))
    );loop
    matriz-resultado
  );let
);defun
