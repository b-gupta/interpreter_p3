; finds a variable in the environment
; '( (a b c) (1 2 3) )
(define lookup_block
  (lambda (v state)
    (lookup_block_h v (car state) (car (cdr state)))))

(define lookup_block_h
 (lambda (v vars vals)
   (cond
     ((null? vars) 'nothere)
     ((eq? v (car vars)) (car vals))
     (else (lookup_block_h v (cdr vars) (cdr vals))))))

(define lookup
  (lambda (var state)
    (cond
      ((null? state) (error 'undefined))
      ((not (eq? (lookup_block var (car state)) 'nothere)) (lookup_block var (car state)))
      (else (lookup var (cdr state))))))


(define bind
  (lambda (var val environment)
    (cond
      ((null? environment) (error 'undefined))
      ((not (eq? (lookup_block var (car environment)) 'nothere)) (cons (bindval var val (envremove var (car environment))) (cdr environment)))
      (else (cons (car environment) (bind var val (cdr environment)))))))

(define bindval
  (lambda (var val environment)
    (list (cons var (car environment)) (cons val (car (cdr environment))))))

; (envremove 'y '(x z y) '(5 3 10) '( () () )) -> '( (y x) (5 10) )
; note i didn't maintain order within a block which to me is not really a big deal
(define envremove
  (lambda (var block)
    (envremove_h var (car block) (car (cdr block)) new_block)))
    
(define envremove_h
  (lambda (v vars vals result)
    (cond
      ((eq? v (car vars)) (list (append (cdr vars) (car result)) (append (cdr vals) (car (cdr result)))))
      (else (envremove_h v (cdr vars) (cdr vals) 
                         (list (cons (car vars) (car result)) (cons (car vals) (car (cdr result)))) ;build the resulting block
                         )))))

(define add
  (lambda (var environment)
    (cons (list (cons var (getvars environment)) (cons 0 (getvals environment))) (cdr environment))))

; returns the values in the nevironment
; '( ( (x y) (5 10) ) ) -> (5 10)
(define getvals
  (lambda (environment)
    (car (cdr (car environment)))))

; returns the vars or the first var of the first block in the environment
; '( ( (x y) (5 10) ) ) -> (x y)
(define getvars
  (lambda (environment)
    (car (car environment))))
    

(define add_block
  (lambda (environment)
    (cons '( () () ) environment)))

(define remove_block
  (lambda (environment)
    (cdr environment)))

; creates an empty environment
(define new_environment '( ( () () ) ) )
(define new_block '( () () ))