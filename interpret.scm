;***********************
; Simple C Interpreter *
;***********************
; Bharat Gupta *********
; BXG135 ***************
; Robert Luciano *******
; RDL40 ****************
; 4/2/13 **************
; **********************
; Project Part 3
; Interpreter for C or 
; Java like language.
;***********************


(define dumb_function '())
(load "environment.scm")
;(load "functionParser.scm") can only use this if main function is defined
(load "loopSimpleParser.scm")

(define interpret
  (lambda (file)
    (call/cc (lambda (k)
               (interpret_stmt_list
                (parser file)
                 new_environment
                 k (lambda (v) (error "Illegal break")) (lambda (v) (error "Illegal continue")))))))

;returns the final environment
(define interpret_stmt_list
  (lambda (parsetree environment k b c)
               (cond
                 ((null? parsetree) environment)
                 (else (interpret_stmt_list (cdr parsetree) (interpret_stmt (car parsetree) environment k b c) k b c)))));come back here to fix continue

;interpets a given statement
(define interpret_stmt
  (lambda (stmt environment return break continue)
    (cond
      ((eq? '= (car stmt))
       (interpret_assign (cdr stmt) environment))
      
      ((eq? 'var (car stmt))
       (interpret_dec (cdr stmt) environment))
      
      ((eq? 'if (car stmt))
       (interpret_if (cdr stmt) environment return break continue))
      
      ((eq? (car stmt) 'while)
       (interpret_while (cdr stmt) environment return))
      
      ((eq? (car stmt) 'begin)
       (interpret_begin (cdr stmt) environment return break continue))
      
      ((eq? (car stmt) 'continue) (break (continue environment)))
      
      ((eq? (car stmt) 'break) (break environment)) 
      
      ((eq? (car stmt) 'return) (return (check_val (evaluate (cdr stmt) environment))))
      
      (else environment) ; not sure about here. made sense in my head thats why i put it in
    
      )))

(define check_val
  (lambda (v)
    (cond
      ((number? v) v)
      (v 'true)
      (else 'false))))

;**********************
;interpret various statements

;binds a value to a variable
;(x expr)
; (x (= y expr))
(define interpret_assign
  (lambda (stmt environment)
    (bind (car stmt) (evaluate (cdr stmt) environment) (evaluate-env (cdr stmt) environment))))

;adds the var to the environment
;(x)
;(x expr)
; (x (= y 10))
(define interpret_dec
  (lambda (stmt environment)
    (cond
      ((null? (cdr stmt)) (add (car stmt) environment))
      (else (bind (car stmt) (evaluate (car (cdr stmt)) environment) (add (car stmt) (evaluate-env (car (cdr stmt)) environment)))))))

; evaluates if statements recursively because if statements can be nested due to elseifs
; ((> x y) (return y) (return x))
; ((> (x = 5) y) (return y) (return x))
; ((> x y) (return y) (if (> (* x x) y) (return (* x x))
(define interpret_if
   (lambda (stmt environment return break continue)
    (cond
     ((evaluate (car stmt) environment) (interpret_stmt (then stmt) (evaluate-env (car stmt) environment) return break continue))
     ((pair? (cdr (cdr stmt))) (interpret_stmt (else stmt) (evaluate-env (car stmt) environment) return break continue))
     (else environment))))

; returns the then clause
; ((> x y) (return y) (return x)) -> (return y)
(define then
  (lambda (stmt)
    (car (cdr stmt))))
; returns the else clause
; ((> x y) (return y) (return x)) -> (return x)
(define else
  (lambda (stmt)
    (car (cdr (cdr stmt)))))

; returns body for a while stmt
(define getbody
  (lambda (stmt)
    (car (cdr stmt))))

; executes a while loop
(define interpret_while
  (lambda (stmt environment return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condt body environment)
                                (cond
                                  ((evaluate condt environment) 
                                   (loop condt body (interpret_stmt body (evaluate-env condt environment) return break (lambda (e) (loop condt body e)))))
                                  (else (evaluate-env condt environment))))))
                 (loop (car stmt) (getbody stmt) environment))))))

(define interpret_begin
  (lambda (stmt environment return break continue)
     (remove_block (interpret_stmt_list stmt (add_block environment) return (lambda (v) (break (remove_block v))) continue))))

;takes an expression, evaluates it, and returns the value
; no type checking is done
; (+ 5 3) -> 8
; (|| false true) -> #t
(define evaluate
  (lambda (expr environment)
    (cond
      ((null? expr) 'undefined)
      
      ((number? expr) expr)
      
      ((eq? expr 'true) #t)
      
      ((eq? expr 'false) #f)
     
      ;variable
      ((and (not (pair? expr)) (not (number? expr))) (lookup expr environment))
      
      ;ask if need to add error to lambda (v) v here to detect illegal break/continue
      ((eq? '= (car expr))
       (evaluate (op1 expr) (interpret_stmt expr (evaluate-env (op1 expr) environment) (lambda (v) v) (lambda (v) v) (lambda (v) v))))
      
      ((eq? '% (car expr))
       (modulo (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))
      
      ((eq? '* (car expr))
       (* (evaluate (op1 expr) environment) (evaluate (op2 expr)  (evaluate-env (op1 expr) environment))))
      
      ((eq? '/ (car expr))
       (/ (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))
      
      ((eq? '+ (car expr))
       (+ (evaluate (op1 expr) environment) (evaluate (op2 expr)  (evaluate-env (op1 expr) environment))))
      
      ;unary
      ((and (eq? '- (car expr)) (null? (cdr (cdr expr)))) (- 0 (evaluate (op1 expr) environment)))
      
      ((eq? '- (car expr))
       (- (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))
      
      ((eq? '== (car expr)) (= (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))
      ((eq? '!= (car expr)) (not (= (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment)))))     
      ((eq? '> (car expr)) (> (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))
      ((eq? '< (car expr)) (< (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))
      ((eq? '>= (car expr)) (>= (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))
      ((eq? '<= (car expr)) (<= (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))
      
      ((eq? '&& (car expr))
       (and (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))

      ((eq? '|| (car expr))
       (or (evaluate (op1 expr) environment) (evaluate (op2 expr) (evaluate-env (op1 expr) environment))))
      
      ((eq? '! (car expr))
       (not (evaluate (op1 expr) environment)))
      
      (else (evaluate (car expr) environment))
      )))
  
(define evaluate-env
  (lambda (expr environment)
    (cond
      ((null? expr) 'undefined)
      
      ((number? expr) environment)
      
      ((eq? expr 'true) environment)
      
      ((eq? expr 'false) environment)
      
      ; variable
      ((and (not (pair? expr)) (not (number? expr))) environment)
      
      ; ((number? (car expr)) (car expr))
      ((eq? '= (car expr))
       (evaluate-env (op1 expr) (interpret_stmt expr environment (lambda (v) v) (lambda (v) v) (lambda (v) v))))
      
      ;unary
      ((and (eq? '- (car expr)) (null? (cdr (cdr expr)))) (evaluate-env (op1 expr) environment))
      
      ((or (eq? '* (car expr)) (or (eq? '/ (car expr)) (or (eq? '+ (car expr)) (or (eq? '- (car expr)) (or (eq? '&& (car expr)) (eq? '|| (car expr)))))))
       (evaluate-env (op2 expr) (evaluate-env (op1 expr) environment)))
      ((or (eq? '> (car expr)) (or (eq? '< (car expr)) (or (eq? '>= (car expr)) (or (eq? '<= (car expr)) (or (eq? '== (car expr)) (or (eq? '% (car expr)) (eq? '!= (car expr))))))))
       (evaluate-env (op2 expr) (evaluate-env (op1 expr) environment)))
      
      ((eq? '! (car expr))
       (evaluate-env (op1 expr) environment))
      (else (evaluate-env (car expr) environment))
      )))


;returns the first operand of a given stmt
; (+ x y) -> x
(define op1
  (lambda (expr)
    (car (cdr expr))))

;returns the second operand of a given stmt
(define op2
  (lambda (expr)
    (car (cdr (cdr expr)))))
      
                     
                     
                     
                     
                     
