#lang racket

(define (evaluar_producto producto)
  (if (null? producto)
      '()
      (if (null? (cdr (cdr (cdr producto))))
          (list '+ (list '* (derivada (car (cdr producto))) (car (cdr (cdr producto)))) (list '* (car (cdr producto)) (derivada (car (cdr (cdr producto))))))
          (list '+ (append (list '* (derivada (car (cdr producto)))) (cdr (cdr producto))) (list '* (evaluar_producto (append '(*) (cdr (cdr producto)))) (car (cdr producto)))))))
       
(define (evaluar_cociente cociente)
  (if (null? cociente)
      '()
      (if (null? (cdr (cdr (cdr cociente))))
          (list '/ (list '- (list '* (derivada (car (cdr cociente))) (car (cdr (cdr cociente)))) (list '* (car (cdr cociente)) (derivada (car (cdr (cdr cociente)))))) (list '^ (car (cdr (cdr cociente))) 2))
          (list '/ (list '- (list '* (derivada (car (cdr cociente))) (append (list '/) (cdr (cdr cociente)))) (list '* (car (cdr cociente)) (evaluar_cociente (append '('/) (cdr (cdr cociente)))))) (list '^ (append '(/) (cdr (cdr cociente))) 2 )))))
       
(define (evaluar_s_r s_r)
  (if (null? s_r)
      '()
      (if (null? (cdr (cdr (cdr s_r))))
          (list (car s_r) (derivada (car (cdr s_r))) (derivada (car (cdr (cdr s_r)))))
          (list (car s_r) (derivada (car (cdr s_r))) (evaluar_s_r (append (list (car s_r)) (cdr (cdr s_r))))))))

(define (evaluar_potencia potencia)
  (if (null? potencia)
      '()
      (list '* (derivada (car (cdr potencia))) (car (cdr (cdr potencia))) (list '^ (car (cdr potencia)) (list '- (car (cdr (cdr potencia))) 1)))))

(define (derivada lista)
  (if (null? lista)
      '()
      (if (list? lista)
          (if (list? (car lista))
              (append (derivada (car lista)) (derivada (cdr lista)))
              (if (eq? (car lista) '*)
                  (evaluar_producto lista)
                  (if (eq? (car lista) '/)
                      (evaluar_cociente lista)
                      (if (eq? (car lista) '^)
                          (evaluar_potencia lista)
                          (if (eq? (car lista) '+)
                              (evaluar_s_r lista)
                              (if (eq? (car lista) '-)
                                  (evaluar_s_r lista)
                                  '()))))))
          (if (number? lista)
              0
              (if (eq? lista 'x)
                  1
                  '())))))

;funcion para derivar
(define (derivar lista)
  (if (null? lista)
      '()
      (limpiar (derivada lista) (- (length (derivada lista)) 1) '()) )
  ;mandamos a llamar la funcion llimpiar que recibe una lista del resultado de la derivada, con su tamaño - 1 y una lista
  ;vacia que va ser una pila
  )

;La función limpia, recibe el resultado de la derivada para que esta pueda ser simplificada, recibe un i de indice que va ser
;una especie de interador, y una pila que va guardando los resultados de las operaciones;
(define (limpiar lista i pila)
  (if (<= 0 i);paso recursivo, si es menor que 0, se acaba el ciclo
   (if (eq? '+ (list-ref lista i))
       (limpiar lista (- i 1) (suma pila) )
       (if (eq? '- (list-ref lista i))
           (limpiar lista (- i 1) (resta pila) )
           (if (eq? '* (list-ref lista i))
               (limpiar lista (- i 1) (multiplicacion pila) )
               (if (eq? '/ (list-ref lista i))
                   (limpiar lista (- i 1) (division pila) )
                   (if (eq? '^ (list-ref lista i))
                       (limpiar lista (- i 1) (potencia pila) )
                       (if (list? (list-ref lista i))
                           ;si esto el elemento actual es una lista, lo que va hacer, es hacer un llamado recursivo de esta
                           ;función para si misma, para que pueda resolver las operaciones internas dentro de los
                           ;parentesis y luego el resultado se inserta en la pila
                           (limpiar lista (- i 1) (append pila (limpiar (list-ref lista i) (- (length (list-ref lista i)) 1) '() )))
                           (limpiar lista (- i 1) (append pila (list (list-ref lista i))))
                           ;si es solo numero o un simbolo el elemento que se esta evaluando, simplemente se inserta en la
                           ;pila
                        ) 
                       )
                   )
               )
           ) 
       )
       pila
   )
)
;cada operacion tiene una función para que pueda resolver las operaciones como se corresponden, por lo general
;se detecta primero si los dos elementos a evaluar en la pila son numeros, si lo son, simplemente se hace la operación
;y el resultado se inserta en la pila, volviendo a la funcion limpia para el siguiente elemento
;si uno de los elementos no es un numero, se ve si el otro factor es simplificable, es decir, si es una multiplicacion
;1 * x, debe regresar x, o si es x^0, debe regresar 1, y así sicesivamente
;en caso de que no sea posible la simplifación, por últimp se hace una lista de esa operación y se inserta en la pila

(define (potencia pila)
  (if (number? (list-ref pila (- (length pila) 1) ) ) 
      (append (drop-right pila 2) (list (expt (list-ref pila (- (length pila) 1) ) (list-ref pila (- (length pila) 2)))))
      (if (= (list-ref pila (- (length pila) 2) ) 0)
         (append (drop-right pila 2) (list 1))
         (if (= (list-ref pila (- (length pila) 2) ) 1)
             (append (drop-right pila 2) (list (list-ref pila (- (length pila) 1) ) ))
             (append (drop-right pila 2) (list (list '^ (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2)))))
         )
      )
  )
)

;Ahora, las operaciones división, multiplicación y suma, pueden tener varios factores para un solo signo, o sea
; un (+ a b c d), y en esos casos, simplemente se hace una operación recursiva con todos los factores para obtener un resultado
; más simplificable
(define (division pila)
  (if (and (number? (list-ref pila (- (length pila) 1) ) ) (number? (list-ref pila (- (length pila) 2))))
      (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (division (append (drop-right pila 2) (list (/ (list-ref pila (- (length pila) 1) ) (list-ref pila (- (length pila) 2))))))
       (append (drop-right pila 2) (list (/ (list-ref pila (- (length pila) 1) ) (list-ref pila (- (length pila) 2)))))
      )
      (if (and (number? (list-ref pila (- (length pila) 1) ) ) (= (list-ref pila (- (length pila) 1) ) 0))
          (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (division (append (drop-right pila 2) (list 0)))
       (append (drop-right pila 2) (list 0))
      )
          
          (if (and (number? (list-ref pila (- (length pila) 2))) (= (list-ref pila (- (length pila) 2) ) 1))
              (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (division (append (drop-right pila 2) (list (list-ref pila (- (length pila) 1)))))
       (append (drop-right pila 2) (list (list-ref pila (- (length pila) 1))))
      )
              
              (if (eq? (list-ref pila (- (length pila) 2)) (list-ref pila (- (length pila) 1)))
                  (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (division (append (drop-right pila 2) (list 1)))
       (append (drop-right pila 2) (list 1))
      )
                  (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (division  (append (drop-right pila 2) (list (list '/ (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2))))))
        (append (drop-right pila 2) (list (list'/ (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2)))))
      )
                 
              )
          )
      )
  )
)

(define (multiplicacion pila)
  ;vemos si los dos elementos son numeros
  (if (and (number? (list-ref pila (- (length pila) 1) ) ) (number? (list-ref pila (- (length pila) 2))))
      ;vemos si hay un tercer factor para hacer la operación
      (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (multiplicacion (append (drop-right pila 2) (list (* (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2))))))
       (append (drop-right pila 2) (list (* (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2)))))
      ); vemos si un factor es 0
      (if (or (eq? (list-ref pila (- (length pila) 1)) 0) (eq? (list-ref pila (- (length pila) 2)) 0))
;vemos si hay un tercer factor para hacer la operación
          (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (multiplicacion (append (drop-right pila 2) (list 0)))
       (append (drop-right pila 2) (list 0))
      );vemos si el primer elemento es 1
          (if (eq? (list-ref pila (- (length pila) 1)) 1)
;vemos si hay un tercer factor para hacer la operación
              (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (multiplicacion (append (drop-right pila 2) (list (list-ref pila (- (length pila) 2)))))
       (append (drop-right pila 2) (list (list-ref pila (- (length pila) 2))))
      )
              ;vemos si el segundo elemento es 1
              (if (eq? (list-ref pila (- (length pila) 2)) 1)
;vemos si hay un tercer factor para hacer la operación
                  (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (multiplicacion (append (drop-right pila 2) (list (list-ref pila (- (length pila) 1)))))
       (append (drop-right pila 2) (list (list-ref pila (- (length pila) 1))))
      );no nos queda de otra que hacer una lista con un resultado no simplificable
                  (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
;vemos si hay un tercer factor para hacer la operación
                      (multiplicacion (append (drop-right pila 2) (list (list '* (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2))))))
       (append (drop-right pila 2) (list (list '* (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2)))))
      )
                  
              )
          )
      )
  )
)

(define (suma pila)
  (if (and (number? (list-ref pila (- (length pila) 1) ) ) (number? (list-ref pila (- (length pila) 2))))
      (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (suma (append (drop-right pila 2) (list (+ (list-ref pila (- (length pila) 1) ) (list-ref pila (- (length pila) 2))))))
       (append (drop-right pila 2) (list (+ (list-ref pila (- (length pila) 1) ) (list-ref pila (- (length pila) 2)))))
      )
      
      (if (eq? (list-ref pila (- (length pila) 1)) 0)
          (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (suma (append (drop-right pila 2) (list (list-ref pila (- (length pila) 2)))))
       (append (drop-right pila 2) (list (list-ref pila (- (length pila) 2))))
      )
          
          (if (eq? (list-ref pila (- (length pila) 2)) 0)
          (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (suma (append (drop-right pila 2) (list (list-ref pila (- (length pila) 1)))))
       (append (drop-right pila 2) (list (list-ref pila (- (length pila) 1))))
      )
          (if (and (<= 3 (length pila)) (or (number? (list-ref pila (- (length pila) 3) )) (symbol? (list-ref pila (- (length pila) 3)))))
       (suma (append (drop-right pila 2) (list (list '+ (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2))))))
       (append (drop-right pila 2) (list (list '+ (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2)))))
      )
              
          )
      )
  )
)

(define (resta pila)
  (if (and (number? (list-ref pila (- (length pila) 1) ) ) (number? (list-ref pila (- (length pila) 2))))
      (append (drop-right pila 2) (list (- (list-ref pila (- (length pila) 1) ) (list-ref pila (- (length pila) 2)))))
      (if (eq? (list-ref pila (- (length pila) 1)) 0)
          (append (drop-right pila 2) (list '- (list-ref pila (- (length pila) 2))))
          (if (eq? (list-ref pila (- (length pila) 2)) 0)
              (append (drop-right pila 2) (list (list-ref pila (- (length pila) 1))))
              (append (drop-right pila 2) (list (list '- (list-ref pila (- (length pila) 1)) (list-ref pila (- (length pila) 2)))))
          )
      )
  )
)
          