;***********************
; Simple C Interpreter *
;***********************
; Bharat Gupta *********
; BXG135 ***************
; Robert Luciano *******
; RDL40 ****************
; 4/17/13 **************
; **********************
; Project Part 3
; Interpreter for C or 
; Java like language.
;***********************

(load "environment.scm")
;(load "loopSimpleParser.scm")
(load "functionParser.scm")
;(load "classParser.scm")

; now only used to set up the "global environment"
(define interpret_orig
  (lambda (file)
    (call/cc (lambda (k)
               (interpret_stmt_list
                (parser file)
                 new_environment
                 k (lambda (v) (error "Illegal break")) (lambda (v) (error "Illegal continue")))))))

(define interpret
  (lambda (file classname)
    (interpret_class_list (parser file) (string->symbol classname) new_environment)))

(define interpret_class_list
  (lambda (parsetree main_class env)
    (cond
      ((null? parsetree) (find_class_main (lookup main_class env)))
      ;((eq? main_class (get_cname (car parsetree)))
       ;(find_class_main (lookup main_class (interpret_class_def (car parsetree) env))))
      (else (interpret_class_list (cdr parsetree) main_class (interpret_class_def (car parsetree) env))))))
                                            
(define get_cname
  (lambda (class_def)
    (car (cdr class_def))))

(define find_class_main
  (lambda (env)
     (interpret_main (car (cdr (lookup 'main env))) (add_block env))))

(define interpret_main
  (lambda (stmt environment)
   (call/cc (lambda (k)
               (interpret_stmt_list
                stmt
                environment
                k 
                (lambda (v) (error "Illegal break"))
                (lambda (v) (error "Illegal continue")))))))
              
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
      
      ((and (eq? (car stmt) 'return) (not (eq? (lookup_ret 'return_env environment) 'nothere))) (return (evaluate-env (cdr stmt) environment)))
      
      ((eq? (car stmt) 'return) (return (check_val (evaluate (cdr stmt) environment))))
      
      ((and (eq? (car stmt) 'function) (eq? (car (cdr stmt)) 'main)) 
       (interpret_main (mainbody stmt) (add_block environment)))
      
      ;(function name params body)
      ((eq? (car stmt) 'function)
       (interpret_function_dec (op1 stmt) (op2 stmt) (op3 stmt) environment))
      
      ((eq? (car stmt) 'funcall)
       (cond
         ((list? (car (cdr stmt))) ; eventually consider adding dot to evaluate because it will be associated with vars
          (interpret_function_call (lookup (op2 (car (cdr stmt))) (remove_block (remove_block environment))) 
                                                           (cdr (cdr stmt)) 
                                                           (cons (car environment) (remove_block (remove_block environment)))))
         (else (interpret_function_call (lookup (car (cdr stmt)) environment) (cdr (cdr stmt)) environment))))
      (else environment) ; not sure about here. made sense in my head thats why i put it in
    
      )))

(define check_val
  (lambda (v)
    (cond
      ((number? v) v)
      (v 'true)
      (else 'false))))

;gets the name of a function outof a dot 
; (dot var fname)
(define get_dfunc '())

(define mainbody
  (lambda (stmt)
    (car (cdr (cdr (cdr stmt))))))

;**********************
;interpret various statements
;----------------------

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
   ;; (list params body (lambda (e) (setup_funenv name params body e)) environment)))
    (list params body name environment)))

; applies the get_func_env function to the op2 of the closure (which is the name)
(define get_env_closure
  (lambda (closure env)
    (get_func_env (op2 closure) env)))

; deprecated
; allows for the use of recursive functions.
(define setup_funenv
  (lambda (name params body e)
    (bind name (create_closure name params body e) (add name e))))

; handles call by value and call by reference
(define interpret_function_call
  (lambda (closure params environment)
    (call_ref_env (car closure) params 
                  (call/cc (lambda (k)
                  (interpret_stmt_list (op1 closure) 
                                       ;(add_params (remove& (car closure)) params (add_block (remove_block (fix_fenv (car closure) (car (car environment)) environment))) environment)
                                       ; add return_env to env so that if we see a return statement we return environment instead of value
                                       (add 'return_env (create_func_env closure params environment))
                                       k
                                       (lambda (v) (error "Illegal break"))
                                       (lambda (v) (error "Illegal continue"))))) 
                  environment)))

; returns the value that results from a function call
(define interpret_function_callv
  (lambda (closure params environment)
    (call/cc (lambda (k)
               (interpret_stmt_list
                (op1 closure)
                ;(add_params (car closure) params (add_block (remove_block environment)) environment)
                (create_func_env closure params environment)
                k
                (lambda (v) (error "Illegal break"))
                (lambda (v) (error "Illegal continue")))))))

(define create_func_env
  (lambda (closure params env)
    (add_params (remove& (car closure)) params (add_block (get_env_closure closure env)) env)))

; takes params and their values and adds them to
; the environment
; works
; func_e corresponds to the environment when the function was defined
; curr_e is the environment that was just used and stores the values for the params
(define add_params
  (lambda (vars params func_e curr_e)
    (cond
      ((null? params) func_e)
      ;((null? vars) old_e)
      (else
       (add_params (cdr vars) (cdr params) (bind (car vars) (evaluate (car params) curr_e) (add (car vars) func_e)) curr_e)))))

; in order to implement function side effects
(define func_se
  (lambda (params env)
    (cond
      ((null? params) env)
      (else (func_se (cdr params) (evaluate-env (car params) env))))))

; (& x y)
(define call_ref_env
  (lambda (closure_params call_params call_env curr_env)
    (assign_vals 
     call_params
     (get_ref_vars closure_params)
     (get_paramvals (remove& closure_params) call_env) 
     (switch_global (get_global call_env) curr_env))))

(define get_paramvals
 (lambda (params env)
   (cond
     ((null? params) '())
     (else (cons (lookup (car params) env) (get_paramvals (cdr params) env))))))

; for the purposes of call by reference this will assign a list of vars
; in the current environment values resulting from the functional call
(define assign_vals
  (lambda (params check_params vals env)
    (cond
      ((null? params) env)
      ((eq? (car check_params) 'valonly) (assign_vals (cdr params) (cdr check_params) (cdr vals) env))
      (else (assign_vals (cdr params) (cdr check_params) (cdr vals) (bind (car params) (car vals) env))))))

;  
(define switch_global
  (lambda (f_env env)
    ;(list (car env) (car f_env))))
    (cond
      ((null? (cdr env)) (list (envremove 'return_env (car f_env))))
      (else (cons (car env) (switch_global f_env (remove_block env)))))))

; takes the function environment and removes all but the global layer
; which is the very end
(define get_global
  (lambda (fenv)
    (cond
      ((null? (remove_block fenv)) fenv)
      (else (get_global (remove_block fenv))))))

; '(& x & y) -> '(x y)
(define remove&
  (lambda (params)
    (cond
      ((null? params) '())
      ((eq? (car params) '&) (remove& (cdr params)))
      (else (cons (car params) (remove& (cdr params)))))))

; finds the variables in the input that are call by ref by seeing if they are preceded with &
; puts valonly in the spot of variables that do not have a preceding &
(define get_ref_vars
  (lambda (vars)
    (cond
      ((null? vars) '())
      ; should technically check to make sure something comes after the &
      ((eq? (car vars) '&) (cons (cadr vars) (get_ref_vars (cdr (cdr vars)))))
      (else (cons 'valonly (get_ref_vars (cdr vars)))))))

(define interpret_class_def
  (lambda (class env)
    (interpret_class (get_cname class) (get_cparent class) (get_cparams class) env)))

;takes a class definition and pops out the parent classes name
(define get_cparent
  (lambda (class)
    (cond
      ((null? (car (cdr (cdr class)))) '())
      (else (car (cdr (car (cdr (cdr class)))))))))

;takes a class definition and pops out the class params (includes funcs)
(define get_cparams
  (lambda (class)
    (car (cdr (cdr (cdr class))))))

(define interpret_class
 (lambda (name parent params environment)
   (bind name (create_class_env params (get_penv parent environment)) (add name environment))))

; if a class has a parent this will return that parents environment
; otherwise it will simply return an empty environment
(define get_penv
  (lambda (parent env)
    (cond
      ((null? parent) new_environment)
      (else (add_block (lookup parent env))))))

; takes the contents of a class and generates a closure
; for that class essentially
(define create_class_env
  (lambda (params env)
    (cond
      ((null? params) env)
      ((eq? (car (car params)) 'static-var) 
       (create_class_env (cdr params) (interpret_dec (cdr (car params)) env)))
      ((eq? (car (car params)) 'static-function) 
       (create_class_env (cdr params) (interpret_function_dec (op1 (car params)) (op2 (car params)) (op3 (car params)) env)))
      (else (create_class_env (cdr params) env)))))

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
      ((eq? (car expr) 'funcall) (interpret_function_call (lookup (car (cdr expr)) environment) (cdr (cdr expr)) environment))
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
                     
                     
                     
                     
