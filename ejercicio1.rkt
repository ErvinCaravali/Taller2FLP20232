#lang eopl
#|
 Estiven Andrés Martínez Granados:2179687
 Jhoimar Silva Torres:2177167
 Ervin Caravali Ibarra:1925648
|#

;;Implementación basada en listas
(define (fnc var clausulas)
(list 'fnc var clausulas))

(define (fnc-var expr)
(cadr expr))

(define (fnc-clausulas expr)
(caddr expr))

(define (or-vars expr)
(cond
[(list? expr) (cdr expr)]
[else (list expr)]))

(define (and cl1 cl2)
(list 'and cl1 cl2))

(define e1 (fnc 4 '((1 2 3 4)
(and (or -1 -2) (or 3))
(and (or -2 -3 -4)))))

(define e2 (fnc 3 '((1 -2)
(and (or 2) (or -1)))))

(define e3 (fnc 2 '((1))))

;;Casos de prueba

(fnc-var (fnc 4 '((1 2 3 4)
(and (or -1 -2) (or 3)))));;Resultado esperado 4

(fnc-clausulas (fnc 4 '((1 2 3 4)
(and (or -1 -2) (or 3)))));;Resultado esperado ((1 2 3 4)(and (or -1 -2) (or 3)))

(or-vars (or -1 -2));;Resultado esperado (-1)

(or-vars (or -2 -2 -2));;Resultado esperado (-2)

(fnc-var (fnc 1 '((4 3 2 1)
(and (or -2 -1) (or 1)))));;Resultado esperado 1

;;----------------------------------------------------------------------------------------

;; Implementación basada en datatypes

;; Definición de funciones para crear variables, cláusulas y expresiones FNC
(define (make-var id)
  (list 'var id))

(define (make-clause vars)
  (list 'clause vars))

(define (make-fnc num-vars clauses)
  (list 'fnc num-vars clauses))

;; Función para parsear una expresión FNC en el formato especificado
(define (parse-fnc input)
  (cond
    [(and (list? input) (eq? (car input) 'FNC))
     (make-fnc (cadr input) (map parse-clause (caddr input)))]
    [else
     ("Formato de entrada incorrecto")]))

;; Función auxiliar para parsear cláusulas
(define (parse-clause clause)
  (make-clause (map parse-var clause)))

;; Función auxiliar para parsear variables
(define (parse-var var)
  (cond
    [(positive? var) (make-var var)]
    [else (make-var (- var))]))

;; Ejemplos de instancias SAT en el formato especificado usando listas
(define instance1 '(FNC 4 ((1 -2 3 4) (-2 3) (-1 -2 -3) (3 4) (2))))
(define instance2 '(FNC 3 ((-1 2) (3) (-2 4))))
(define instance3 '(FNC 2 ((1 -2) (-1 -2) (2))))

;;Parseo de las instancias SAT
(define parsed-instance1 (parse-fnc instance1))
(define parsed-instance2 (parse-fnc instance2))
(define parsed-instance3 (parse-fnc instance3))
