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

;for use in the circumstance when there is a return stmt
;but you want the environment when calling a function.
(define lookup_noerr
  (lambda (var state)
    (cond
      ((null? state) 'nothere)
      ((not (eq? (lookup_block var (car state)) 'nothere)) (lookup_block var (car state)))
      (else (lookup_noerr var (cdr state))))))


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
      ((null? vars) result)
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

; searches the environment to find the layer that contains a particular
; function and returns it
(define get_func_env
  (lambda (name e)
    (cond
      ((null? e) (error "function not present in environment"))
      ((not (eq? (lookup_block name (car e)) 'nothere)) e);(cons (remove_new_vars name (car e)) (cdr e)))
      (else (get_func_env name (cdr e))))))

; removes vars defined after a function with given name was created
(define remove_new_vars
  (lambda (name block)
    (cond
      ((null? block) (error "problem with func environment"))
      ((eq? name (car (car block))) block)
      (else (remove_new_vars name (envremove (car (car block)) block))))))

; creates an empty environment
(define new_environment '( ( () () ) ) )
(define new_block '( () () ))