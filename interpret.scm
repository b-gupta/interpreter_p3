;***********************
; Simple C Interpreter *
;***********************
; Bharat Gupta *********
; BXG135 ***************
; Robert Luciano *******
; RDL40 ****************
; 4/29/13 **************
; **********************
; Project Part 5
; Interpreter for C or 
; Java like language.
;***********************

(load "environment.scm")
;(load "loopSimpleParser.scm")
;(load "functionParser.scm")
(load "classParser.scm")

; this the interpret method used for parts 1-3
(define interpret_orig
  (lambda (file)
    (call/cc (lambda (k)
               (interpret_stmt_list
                (parser file)
                 new_environment
                 k (lambda (v) (error "Illegal break")) (lambda (v) (error "Illegal continue")))))))

; this interpret will be used for parts 4 and 5 where classes are used.
; it requires both the file name and class name.
(define interpret
  (lambda (file classname)
    (interpret_class_list (parser file) (string->symbol classname) new_environment)))

; setting up the global class environment accesible to anything
(define interpret_class_list
  (lambda (parsetree main_class env)
    (cond
      ((null? parsetree)
       ; go through all of the classes and add the global class environment?
       ;env)
        (execute_main (unbox (lookup 'static (lookup main_class (eval_fields (add_class_ref (getvals env) env) env))))))
       ;(execute_main (append_block (unbox (lookup 'static (lookup main_class env))) (bind 'class env (add 'class new_environment)))))
      (else (interpret_class_list (cdr parsetree) main_class (interpret_class_def (car parsetree) env))))))
                 
; this method will go through the static environments in each class
; and append a reference to all classes using a variable called class
; envs -> ( (class A closure) (class B closure) etc )
; glob_env -> ( ( (A B etc) ( (class A closure) (class B closure) ) etc..
; this is indeed weird looking but it seems to work
(define add_class_ref
  (lambda (envs glob_env)
    (cond
      ((null? envs) glob_env)
      ;(else (add_class_ref (cdr envs) (bind 'class glob_env (add 'class (unbox (lookup 'static (car envs))))))))))
      (else (set-box! (lookup 'static (car envs)) (bind 'class glob_env (add 'class (unbox (lookup 'static (car envs))))))
            (add_class_ref (cdr envs) glob_env)))))

;goes through all of the static fields and determines there value
; initially static veales are forcefully bound to the unevaluated expression
; to prevent errors when referencing vars inherited
(define eval_fields
  (lambda (envs glob_env)
    (cond
      ((null? env) glob_env)
      (else (set-box! (lookup 'static (car envs)) (unbox (lookup 'static (car envs)))
            (add_class_ref (cdr envs) glob_env))))

; essentially it is required that main does not have any parameters
; so we execute the body of the main and ignore the parameters
(define execute_main
  (lambda (env)
     (interpret_main (cadr (lookup 'main env)) (add_block env))))

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
    (begin (display environment) (newline) (newline)
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
      
      ((and (eq? (car stmt) 'return) (not (eq? (lookup_noerr 'return_env environment) 'nothere))) (return (evaluate-env (cdr stmt) environment)))
      
      ((eq? (car stmt) 'return) (return (check_val (evaluate (cdr stmt) environment))))
      
      ((and (eq? (car stmt) 'function) (eq? (car (cdr stmt)) 'main)) 
       (interpret_main (mainbody stmt) (add_block environment)))
      
      ;(function name params body)
      ((eq? (car stmt) 'function)
       (interpret_function_dec (op1 stmt) (op2 stmt) (op3 stmt) environment))
      
      ; (funcall fname p1 p2 .. pn)
      ; (funcall (dot class/object/super fname) p1 p2 .. pn)
      ((eq? (car stmt) 'funcall)
       (cond
         ;dot operator
         ((and (list? (cadr stmt)) (eq? (op1 (cadr stmt)) 'super)) 
          (interpret_dot_call (replace_super (cadr stmt) environment) (cddr stmt) environment #f))
         ((list? (cadr stmt)) (interpret_dot_call (cadr stmt) (cddr stmt) environment #f))
         ;((list? (car (cdr stmt))) ; eventually consider adding dot to evaluate because it will be associated with vars
         ; (interpret_function_call (lookup (op2 (car (cdr stmt))) (remove_block (remove_block environment))) 
         ;                                                  (cdr (cdr stmt)) 
         ;                                                  (cons (car environment) (remove_block (remove_block environment)))))
         (else (find_method (cadr stmt) (cddr stmt) environment #f))))
      (else environment) ; not sure about here. made sense in my head thats why i put it in
    
      ))))

; before returning converts the value checks if it is a bool and returns true/false instead of #t/#f
(define check_val
  (lambda (v)
    (cond
      ((number? v) v)
      ((list? v) v)
      (v 'true)
      (else 'false))))

; used to replace super with the actual class def in the dot expression
; (dot super fname)
(define replace_super
  (lambda (dot environment)
    (list 'dot (lookup 'parent environment) (op2 dot))))

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
; if bool_val is true then it will try to return a value else environment
(define interpret_function_call
  (lambda (closure params environment bool_val)
    (call_ref_env (car closure) params 
                  (call/cc (lambda (k)
                             (interpret_stmt_list (op1 closure) 
                                                  ;(add_params (remove& (car closure)) params (add_block (remove_block (fix_fenv (car closure) (car (car environment)) environment))) environment)
                                                  ; add return_env to env so that if we see a return statement we return environment instead of value
                                                  (cond
                                                    (bool_val (create_func_env closure params environment))
                                                    (else (add 'return_env (create_func_env closure params environment)))
                                                    )
                                                  k
                                                  (lambda (v) (error "Illegal break"))
                                                  (lambda (v) (error "Illegal continue"))))) 
                  environment)))

;(dot class/object fname) (p1 p2 .. pn) (env) bool
(define interpret_dot_call
  (lambda (dot params env bool_val)
    (cond
      ; object
      ((eq? (lookup_noerr (op1 dot) (lookup 'class env)) 'nothere)
      ; get the instance environment and execute the method
       (lookup (op1 dot) env))
      (else (static_method_call (op2 dot) params (lookup 'static (lookup (op1 dot) (lookup 'class env))) env bool_val)))))

(define static_method_call
  (lambda (fname params box_env env bool_val)
    (cond
      (bool_val (interpret_function_call (lookup fname (unbox box_env)) (eval_params params env) (unbox box_env) bool_val))
      (else (set-box! box_env (interpret_function_call (lookup fname (unbox box_env)) (eval_params params env) (unbox box_env) bool_val)) env))))

; prepares/creates the functions environment before its execution
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
    (cond
      ((list? call_env)
       (assign_vals 
        call_params
        (get_ref_vars closure_params)
        (get_paramvals (remove& closure_params) call_env) 
        (switch_global (get_global call_env) curr_env)))
      (else call_env))))

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

(define interpret_class
 (lambda (name parent params env)
   (bind name (create_class_closure parent params new_environment) (add name env))))
   ;(bind name (create_class_env params (get_penv parent environment)) (add name environment))))

; a class closure is an environment containing 3 vars:
; 1. parent -> name of the parent
; 2. static -> the static pre built environment for static functions and vars
; 3. instance -> list of all the var and method declarations inside the class (non static)
(define create_class_closure
  (lambda (parent params new_env)
   (bind 'static (box 
                  (bind 'parent parent (add 'parent (create_static_env params new_environment))))
    (add 'static 
     (bind 'instance (remove_static params) (add 'instance
       (bind 'parent parent (add 'parent new_env))))))))

; takes the contents of a class and generates a closure
; for that class essentially
; only static vars/methods are placed here
(define create_static_env
  (lambda (params env)
    (cond
      ((null? params) env)
      ((eq? (caar params) 'static-var) 
       (create_static_env (cdr params) (interpret_dec (cdr (car params)) env)))
      ((eq? (caar params) 'static-function)
       (create_static_env (cdr params) (interpret_function_dec (op1 (car params)) (op2 (car params)) (op3 (car params)) env)))
      (else (create_static_env (cdr params) env)))))

(define interpret_new
  (lambda (obj_type env)
    (interpret_new_h (lookup obj_type (lookup 'class env)) env)))

(define interpret_new_h
  (lambda (cenv env)
    (create_instance_env (lookup 'parent cenv) (lookup 'instance cenv) new_environment env)))

; takes the classes paramaters (methods and functions) and uses them to find all the instance variables/methods
(define create_instance_env
  (lambda (parent params instance_env env)
    (cond
      ((and (null? params) (null? parent)) instance_env)
      ((null? params) 
       (append_block instance_env (interpret_new_h (lookup parent (lookup 'class env)) env)))
      ((eq? (caar params) 'var)
       (create_instance_env parent (cdr params) (interpret_dec (cdar params) instance_env) env))
      ((eq? (caar params) 'function)
       (create_instance_env parent (cdr params) (interpret_function_dec (op1 (car params)) (op2 (car params)) (op3 (car params)) instance_env) env))
      (else (create_instance_env parent (cdr params) instance_env env)))))


;takes class paramaters and removes all that are static
(define remove_static
  (lambda (params)
    (cond
      ((null? params) '())
      ((or (eq? (caar params) 'static-var) (eq? (caar params) 'static-function))
       (remove_static (cdr params)))
      (else (cons (car params) (remove_static (cdr params)))))))

(define get_cname
  (lambda (class_def)
    (car (cdr class_def))))

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

; if a class has a parent this will return that parents environment
; otherwise it will simply return an empty environment
(define get_penv
  (lambda (parent env)
    (cond
      ((null? parent) new_environment)
      (else (add_block (lookup parent env))))))

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
      ((and (not (pair? expr)) (not (number? expr)))
       (cond
         ;(find_field (op2 expr) (lookup (op1 expr) (lookup 'class environment))
         ;(lookup 'static (lookup (lookup 'parent env) (lookup 'class env))))))
         ((eq? (lookup_noerr expr environment) 'nothere)
          (find_field expr (find_parent environment) (lookup 'class environment)))
         (else (lookup expr environment))))
      
      ; function call
      
      ;(funcall (dot super fname) p1 p2 .. pn)
      ;(funcall (dot class fname) p1 p2 .. pn)
      ;(funcall (dot object fname) p1 p2 .. pn)
      ((and (eq? (car expr) 'funcall) (list? (cadr expr)))
       (cond
         ((eq? (op1 (cadr expr)) 'super) (static_method_call (op2 (cadr expr)) (cddr expr) (find_parent env) env #t))
         ; private method
         ((eq? (lookup_noerr (op1 (cadr expr)) (lookup 'class environment)) 'nothere)
          (method_call (op2 (cadr expr)) (cddr expr) (lookup (op2 (cadr expr)) (lookup (op1 (cadr expr)) environment)) class_env))
         ;static method
         ;(fname params box_env)
          (else (interpret_dot_call (cadr expr) (cddr expr) environment #t))))
      
      ; (funcall name p1 p2...pn)
      ((eq? (car expr) 'funcall)
       (find_method (cadr expr) (cddr expr) environment #t))
      ;(interpret_function_call (lookup (car (cdr expr)) environment) (cdr (cdr expr)) environment #t))
       
       ; class.field --> (dot class field)
      ; object.field --> (dot object field)
      ; new object field --> (dot (new Class) field)
      ((eq? (car expr) 'dot)
       (cond
         ;super
         ((eq? (op1 expr) 'super) (find_field (op2 expr) (find_parent environment) (lookup 'class environment)))
         ;object 
         ((eq? (lookup_noerr (op1 expr) (lookup 'class environment)) 'nothere)
          (lookup (op2 expr) (lookup (op1 expr) environment)))
         ;class (need to check if parent may have it)
         (else (find_field (op2 expr) (lookup 'static (lookup (op1 expr) (lookup 'class environment))) (lookup 'class environment)))))
         ;(else (find_field (op2 expr) (lookup (op1 expr) (lookup 'class environment)) (lookup 'class environment)))))
      
      ; new object
      ((eq? (car expr) 'new)
       (interpret_new (cadr expr) environment))
      
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
      
      ; function ;paste code from evaluate here
      ((eq? (car expr) 'funcall) environment) ;(interpret_function_call (lookup (car (cdr expr)) environment) (cdr (cdr expr)) environment #f))
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

; goes through the environment until a match for the function has been found
(define find_method
  (lambda (fname params env bool_val)
    (cond
      ((eq? (lookup_noerr fname env) 'nothere) (find_method_h fname params (find_parent env) env bool_val))
      (else (interpret_function_call (lookup fname env) params env bool_val)))))

; penv is the boxed static environment of the parent
; env is the original env (also static but unboxed) where the call originated
(define find_method_h
  (lambda (fname params penv env bool_val)
    (cond
      ((eq? (lookup_noerr fname (unbox penv)) 'nothere) (find_method_h (find_parent env) env bool_val))
      (else (static_method_call fname params penv env bool_val)))))

; potentially a field could be in the parent, so this will
; find it no matter where it is if it exists
; c_env is the class closure, env is the active static environment containing 'class
(define find_field
  (lambda (field c_env env)
    (cond
      ((eq? (lookup_noerr field (unbox c_env)) 'nothere)
       (find_field field (lookup (lookup 'parent c_env) env) env))
      (else (lookup field (unbox c_env))))))
      ;((eq? (lookup_noerr field (unbox (lookup 'static c_env))) 'nothere)
       ;(find_field field (lookup (lookup 'parent c_env) env) env))
      ;(else (lookup field (unbox (lookup 'static c_env)))))))

; given the environment, gets the static environment from the parent class if it exists.
; note this environment is "boxed"
(define find_parent
  (lambda (env)
    (lookup 'static (lookup (lookup 'parent env) (lookup 'class env)))))

; takes a list of expressions and evaluates them returning a list of values.
(define eval_params
  (lambda (params environment)
    (cond
      ((null? params) '())
      (else (cons (evaluate (car params) environment) (eval_params (cdr params) (evaluate-env (car params) environment)))))))

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
                     
                     
                     
                     
