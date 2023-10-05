
#lang eopl
(require "ejercicio1.rkt")

#|
 Estiven Andrés Martínez Granados:2179687
 Jhoimar Silva Torres:2177167
 Ervin Caravali Ibarra:1925648
|#

(define (EVALUARSAT instancia asignacion)
  (define (evaluar-clausula clausula asignacion)
    (cond
      [(null? clausula) #t] ; Clausula vacía es verdadera
      [(member (car clausula) asignacion) #t]
      [else (evaluar-clausula (cdr clausula) asignacion)])) ; Evaluación recursiva

  (define (evaluar-fnc fnc asignacion)
    (cond
      [(null? fnc) #t] ; Todas las cláusulas son verdaderas
      [(evaluar-clausula (car fnc) asignacion)
       (evaluar-fnc (cdr fnc) asignacion)]
      [else #f])) ; Al menos una cláusula es falsa

  (define (evaluar-instancia instancia asignacion)
    (let ((clausulas (if (list? instancia) (fnc-clausulas instancia) (make-fnc instancia))))
      (evaluar-fnc clausulas asignacion)))

  (let ((resultado (evaluar-instancia instancia asignacion)))
    (if resultado
        (begin
          (display "Satisfactible")
          (list 'respuesta resultado))
        (begin
          (display "Insatisfactible")
          (list 'respuesta resultado)))))


;;Casos prueba listas:
(display (EVALUARSAT e1 '(1 3 4))) ; Debe mostrar "Satisfactible"
(display (EVALUARSAT e1 '(1 -3 4))) ; Debe mostrar "Satisfactible"
(display (EVALUARSAT e2 '(-1 2))) ; Debe mostrar "Satisfactible"
(display (EVALUARSAT e3 '(1))) ; Debe mostrar "Insatisfactible"
(newline)

;;Casos prueba datatypes:
(newline)
(display (EVALUARSAT parsed-instance1 '(1 2 3 4))) ; Debe mostrar "Satisfactible"
(display (EVALUARSAT parsed-instance2 '(-1 2))) ; Debe mostrar "Satisfactible"
(display (EVALUARSAT parsed-instance3 '(1))) ; Debe mostrar "Satisfactible"

