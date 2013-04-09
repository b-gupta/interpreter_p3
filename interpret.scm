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

(load "environment.scm")
(load "functionParser.scm")
;(load "loopSimpleParser.scm")

; now only used to set up the "global environment"
(define interpret
  (lambda (file)
    (call/cc (lambda (k)
               (interpret_stmt_list
                (parser file)
                 new_environment
                 k (lambda (v) (error "Illegal break")) (lambda (v) (error "Illegal continue")))))))

(define interpret_main
  (lambda (stmt environment k)
   ; (call/cc (lambda (k)
               (interpret_stmt_list
                stmt
                environment
                k 
                (lambda (v) (error "Illegal break"))
                (lambda (v) (error "Illegal continue")))))
              
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
      
      ((and (eq? (car stmt) 'function) (eq? (car (cdr stmt)) 'main)) 
       (interpret_main (mainbody stmt) environment return))
      
      ;(function name params body)
      ((eq? (car stmt) 'function)
       (interpret_function_dec (op1 stmt) (op2 stmt) (op3 stmt) environment))
      
      ((eq? (car stmt) 'funcall)
       ; send it the closure instead of the name to make things easier
       ; as far as extracting things from the closure itself
       ; (closure params environment)
       (interpret_function_call (lookup (car (cdr stmt)) environment) (cdr (cdr stmt)) environment))
      
      (else environment) ; not sure about here. made sense in my head thats why i put it in
    
      )))

(define check_val
  (lambda (v)
    (cond
      ((number? v) v)
      (v 'true)
      (else 'false))))

(define mainbody
  (lambda (stmt)
    (car (cdr (cdr (cdr stmt))))))

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
; ( (cond) (body) )
(define interpret_while
  (lambda (stmt environment return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (condt body environment)
                                (cond
                                  ((evaluate condt environment) 
                                   (loop condt body (interpret_stmt body (evaluate-env condt environment) return break (lambda (e) (loop condt body e)))))
                                  (else (evaluate-env condt environment))))))
                 (loop (car stmt) (getbody stmt) environment))))))
; sets aside a new environment block for a given program block
; { (body) }
(define interpret_begin
  (lambda (stmt environment return break continue)
     (remove_block (interpret_stmt_list stmt (add_block environment) return (lambda (v) (break (remove_block v))) continue))))

; essentially does the same thing as interpreter decl
; (name params body)
(define interpret_function_dec
  (lambda (name params body environment)
    (bind name (create_closure name params body environment) (add name environment))))

; (params body (lambda (e) (get-function-env e))
; uses continuations so that recursive functions can be called.
; access the environment from the closure by calling the stored
; procedure on the environment so that the function declaration is
; inside it.
(define create_closure
  (lambda (name params body environment)
    (list params body (lambda (e) (get_funenv name params body e)) environment)))

; allows for the use of recursive functions.
(define get_funenv
  (lambda (name params body e)
    (bind name (create_closure name params body e) (add name e))))

(define interpret_function_call
  (lambda (closure params environment)
    (remove_block 
     (interpret_stmt_list (op1 closure) 
                          (add_params (car closure) params (add_block (getenv_closure closure)))
                          (lambda (v) (error "Value cannot be used.")) 
                          (lambda (v) (error "Illegal break"))
                          (lambda (v) (error "Illegal continue"))))))

; returns a value
(define interpret_function_callv
  (lambda (closure params environment)
    (call/cc (lambda (k)
               (interpret_stmt_list
                (op1 closure)
                (add_params (car closure) params (add_block (getenv_closure closure)) environment)
                k
                (lambda (v) (error "Illegal break"))
                (lambda (v) (error "Illegal continue")))))))

(define getenv_closure
  (lambda (closure)
    ((op2 closure) (op3 closure))))

; takes params and their values and adds them to
; the environment
; works
; old_e corresponds to the environment stored in the closure, active when function was defined
; curr_e is the environment that was just used and stores the values for the params
(define add_params
  (lambda (vars params old_e curr_e)
    (cond
      ((null? params) old_e)
      (else
       (add_params (cdr vars) (cdr params) (bind (car vars) (evaluate (car params) curr_e) (add (car vars) old_e)) curr_e)))))

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
     
      ;variable or function. if function then evaluate its value
      ((and (not (pair? expr)) (not (number? expr))) (lookup expr environment))
      
      ; we need to perform a function call
      ; (funcall name p1 p2...pn)
      ((eq? (car expr) 'funcall) 
       (interpret_function_callv (lookup (car (cdr expr)) environment) (cdr (cdr expr)) environment))
      
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
      
      ; function
      ((eq? (car expr) 'funcall) environment)
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

; takes a list of expressions and evaluates them returning a list of values.
(define eval_params
  (lambda (params environment)
    (eval_params_h params environment '())))

;returns the first operand of a given stmt
; (+ x y) -> x
(define op1
  (lambda (expr)
    (car (cdr expr))))

;returns the second operand of a given stmt
(define op2
  (lambda (expr)
    (car (cdr (cdr expr)))))
; returns the third operand of a given stmt
; (funcall name params body) -> body
(define op3
  (lambda (expr)
    (car (cdr (cdr (cdr expr))))))
      
                     
                     
                     
                     
                     
