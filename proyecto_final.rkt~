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
          (list '/ (list '- (list '* (derivada (car (cdr cociente))) (list '/ (cdr (cdr cociente)))) (list '* (car (cdr cociente)) (evaluar_cociente (append '('/) (cdr (cdr cociente)))))) (list '^ (append '(/) (cdr (cdr cociente))) 2 )))))
       
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
                      
          