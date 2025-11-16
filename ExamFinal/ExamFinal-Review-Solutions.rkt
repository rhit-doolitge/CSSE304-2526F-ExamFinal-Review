#lang racket

(require "../chez-init.rkt")

(provide compare-truthiness product-all make-checkpointed save-checkpoint restore-checkpoint clear-checkpoints bool-func-type eval-one-exp)

(define stuff #f)


;-----------------------------------------------
; Macros
;-----------------------------------------------

; This takes in two arguments a #t or #f value that is the value you are looking for
; and a list. This list can either be a direct #t or #f value or a pair.
; The pairs come in (value single-argument-boolean-function) it should run the function
; on the value given in the pair and see if the output is the same as the #t or #f value you are looking for
; ex: (compare-truthiness #t ((5 number?)) returns #t
; but (compare-truthiness #f ((5 number?)) returns #f
; and (compare-truthiness #t (#t) returns #t
; but (compare-truthiness #f (#t) returns #f

(define-syntax compare-truthiness
  (syntax-rules ()
    [(_ truth ((val check))) (if (eqv? truth (check val))
                                 #t
                                 #f)]
    [(_ truth (first)) (if (eqv? truth first)
                           #t
                           #f)]
    [(_ truth ((val check) checks ...)) (if (eqv? truth (check val))
                                            (compare-truthiness truth (checks ...))
                                            #f)]
    [(_ truth (first checks ...)) (if (eqv? truth first)
                                      (compare-truthiness truth (checks ...))
                                      #f)]))

; This function takes in infinite amount of arguments.
; The aguments are either numbers or lists of numbers
; it should multiply all the numbers that come in together
; and output the result
; ex: (product-all 3 4 5) return 60
; and (product-all (3 4) (2)) returns 24

(define-syntax product-all
  (syntax-rules ()
    [(_ (vals ...))  (* vals ...)]
    [(_ val) val]
    [(_ (vals ...) others ...) (* (* vals ...) (product-all others ...))]
    [(_ val others ...) (* val (product-all others ...))]))

;-----------------------------------------------
; call/cc - Checkpoint System
;-----------------------------------------------
;; In this problem you will implement a checkpoint/restore system
;; using call/cc. Your code must be written in standard Racket
;; (not in your interpreter).
;;
;; A checkpoint system allows you to save the current execution state
;; and restore it later (potentially multiple times), similar to
;; save states in video games.
;;
;; You will create checkpointed procedures by calling:
;;
;;     (make-checkpointed (lambda () ...))
;;
;; Inside the procedure, you can:
;;   • Save execution state: (save-checkpoint 'name)
;;   • Restore to a saved state: (restore-checkpoint 'name)
;;   • Clear all checkpoints: (clear-checkpoints)
;;
;; When save-checkpoint is first executed, it returns 'saved.
;; When execution is restored to that checkpoint, it returns 'restored.
;; This allows you to write code that behaves differently on restore.
;;
;; Example:
;;
;;   (define proc
;;     (make-checkpointed
;;       (lambda ()
;;         (let ((x 0))
;;           (set! x 5)
;;           (save-checkpoint 'here)
;;           (set! x 10)
;;           x))))
;;
;;   (proc) => 10
;;   (restore-checkpoint 'here) => 10 (execution resumes, x was 5, then set to 10)
;;
;; Another example showing different behavior:
;;
;;   (define proc2
;;     (make-checkpointed
;;       (lambda ()
;;         (let ((x 0))
;;           (set! x 5)
;;           (when (equal? (save-checkpoint 'point) 'saved)
;;             (set! x 10))
;;           x))))
;;
;;   (proc2) => 10 (save-checkpoint returns 'saved, x becomes 10)
;;   (restore-checkpoint 'point) => 5 (returns 'restored, when clause skipped)
;;
;; You must implement the following procedures:
;;
;;   (make-checkpointed proc) - wraps a procedure to support checkpoints
;;   (save-checkpoint name) - saves current state, returns 'saved or 'restored
;;   (restore-checkpoint name) - restores to a saved checkpoint
;;   (clear-checkpoints) - clears all saved checkpoints
;;
;; Requirements:
;;   • Use call/cc to capture and restore continuations.
;;   • save-checkpoint should return 'saved initially, 'restored when restored.
;;   • The same checkpoint can be restored multiple times.
;;   • make-checkpointed should clear old checkpoints at the start.
;;   • restore-checkpoint should error if checkpoint doesn't exist.
;;   • You may use mutation and global variables.
;;   • You may not use threads, async, or coroutine libraries.
;;
;; Implement the checkpoint system below.

(define checkpoints (make-hash))
(define return-stack '())

(define save-checkpoint
  (lambda (name)
    (call/cc (lambda (k)
               (hash-set! checkpoints name k)
               'saved))))

(define restore-checkpoint
  (lambda (name)
    (call/cc (lambda (return-k)
               (set! return-stack (cons return-k return-stack))
               (let ((k (hash-ref checkpoints name #f)))
                 (if k
                     (k 'restored)
                     (error "Checkpoint not found")))))))

(define clear-checkpoints
  (lambda ()
    (set! checkpoints (make-hash))))

(define make-checkpointed
  (lambda (proc)
    (lambda args
      (clear-checkpoints)
      (set! return-stack '())
      (let ((result (apply proc args)))
        (if (not (null? return-stack))
            (let ((return-k (car return-stack)))
              (set! return-stack (cdr return-stack))
              (return-k result))
            result)))))

;-----------------------------------------------
; Typing
;-----------------------------------------------

;; Consider this small variant of scheme that I call bool-func.

;; bool-func-exp ::= <boolean>
;;                 | not
;;                 | and
;;                 | or
;;                 | <identifier>
;;                 | (lambda ({<identifier>}*) <bool-func-exp>)
;;                 | ({bool-func-exp}+)
;;
;; A boolean is either #t or #f.
;; The identifiers 'not', 'and', and 'or' is reserved and cannot be used as a variable.

;; In this language, closures can take only booleans and must return
;; only booleans. There are two possible types:
;;   'bool        -- indicates a boolean value
;;   a number n   -- indicates a closure that takes exactly n boolean
;;                   parameters and returns a boolean.
;;
;; The built-in operator 'not' is a closure of type 1 (i.e. it takes
;; one boolean argument and returns a boolean).
;; The built-in operators 'and' and 'or' are closures of type 2 (i.e. they take
;; two boolean arguments and return a boolean)
;; These are the *only* built-in value that evaluates to a closure type.
;;
;; Examples:
;;   #t has type 'bool
;;   (lambda (x) x) has type 1
;;   not has type 1
;;   
;;   (lambda (x y) (not x)) has type 2
;;   (lambda (x) (lambda (y) y)) should raise 'closure-return
;;   ((lambda (x y) (or x y)) #f #t) has type 'bool
;;   ((lambda (x y z) (and x (not y))) #t #t #f) has type 'bool
;;   (lambda () #t) has type 0
;;
;; Possible type errors in this language:
;;   'bad-proc        -- operator position is not a closure
;;   'bad-param-num   -- wrong number of parameters in an application
;;   'unbound-var     -- variable with no binding in scope
;;   'closure-param   -- a closure is passed as a parameter
;;   'closure-return  -- a closure is returned from any procedure
;;
;; If your typing system detects an error, call:
;;     (raise '<symbol>)
;;
;; Examples of errors:
;;   (#t #f)                   => 'bad-proc
;;   ((lambda (x) x) #t #f)    => 'bad-param-num
;;   (lambda (x) y)            => 'unbound-var
;;   ((lambda (x) x) (lambda (y) y))  => 'closure-param
;;   (lambda (x) (lambda (y) #t))     => 'closure-return
;;
;; Write bool-func-type that given a bool-func expression returns its
;; type or raises the appropriate error. You will likely want a helper
;; and an environment for variable bindings. You do NOT need to write
;; a parser.

(define bool-func-type
  (lambda (exp)
    (bool-func-type-exp '() exp)))

(define bool-func-type-exp
  (lambda (vars exp)
    (cond
      [(null? exp) (raise 'bad-proc)]
      [(boolean? exp) 'bool]
      [(eqv? exp 'not) 1]
      [(eqv? exp 'and) 2]
      [(eqv? exp 'or) 2]
      [(symbol? exp)
       (if (contains? vars exp)
           'bool
           (raise 'unbound-var))]
      [(and (list? exp) (eqv? (car exp) 'lambda))
       (let* ([params (cadr exp)]
              [body-type (bool-func-type-exp (append params vars)
                                             (caddr exp))])
         (if (number? body-type)
             (raise 'closure-return)
             (length params)))]
      [(list? exp)
       (let* ([rator (bool-func-type-exp vars (car exp))]
              [rands (map (curry bool-func-type-exp vars)
                          (cdr exp))])
         (if (number? rator)
             (if (= (length rands) rator)
                 (if (all-bool rands)
                     'bool
                     (raise 'closure-param))
                 (raise 'bad-param-num))
             (raise 'bad-proc)))]
      [else (raise 'bad-proc)])))

(define contains?
  (lambda (lst val)
    (cond [(null? lst) #f]
          [(eqv? (car lst) val) #t]
          [else (contains? (cdr lst) val)])))

(define all-bool
  (lambda (lst)
    (cond [(null? lst) #t]
          [(not (eqv? 'bool (car lst))) #f]
          [else (all-bool (cdr lst))])))


;-----------------------------------------------
; General Interpreter
;-----------------------------------------------

;; Make your interpreter support a new expression, grab-all. It's
;; called with a symbol, like (grab-all x). It returns the list of
;; values that x is bound to, starting at the most recent binding.
;; For example:
; (let ([x 2])
;   (let ([x 3])
;     (grab-all x)))
; => '(3 2)

;; Additionally, you should be able to call set! on (grab-all x) to
;; set! all the bindings of x to the same value across all environments.
;; For example:
; (let ([x 2])
;   (let ([x 3])
;     (set! (grab-all x)) 4)
;   x)
; => 4 (the value of x in the top-most environment was also set to 4, but the interesting part is that the other one changed)

(define eval-one-exp
  (lambda (exp)
    'nyi))

;; here is what I changed
;-------------------+
;                   |
;   sec:DATATYPES   |
;                   |
;-------------------+

;; (define-datatype expression expression?
;;   [grab-all-exp
;;    (id symbol?)]
;;   [grab-all-set!-exp
;;    (grab-all-exp expression?)
;;    (exp expression?)]
;;   )
;; 
;; (define parse-exp         
;;   (lambda (datum)
;;     (cond
;;       [(list? datum)
;;        (cond
;;          [(eqv? (car datum) 'grab-all) (grab-all-exp (second datum))]
;;          [(eqv? (car datum) 'set!)
;;           (cond [(not (= (length datum) 3)) (error 'parse-exp "parse error set should have two arguments: ~s" datum)]
;;                 [(not (symbol? (car datum))) (error 'parse-exp "parse error set first argument should be an identifier: ~s" datum)]
;;                 [else (with-handlers ([exn:parse?
;;                                        (lambda (x) (error 'parse-exp "parse error set! second arg not a valid expression: ~s" datum))])
;;                         (if [symbol? (2nd datum)]
;;                             (set!-exp (2nd datum) (parse-exp (3rd datum)))
;;                             (grab-all-set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))))])]
;;       [else (error 'parse-exp "bad expression: ~s" datum)])))
;; 
;; (define apply-env-all
;;   (lambda (env sym)
;;     (map unbox (apply-env-all-ref env sym))))
;; 
;; (define apply-env-all-ref
;;   (lambda (env sym)
;;     (cases environment env
;;       [empty-env-record ()
;;                         (apply-global-env-all-ref global-env sym)]
;;       [extended-env-record (syms vals env)
;;                            (let ((pos (list-find-position sym syms)))
;;                              (if (number? pos)
;;                                  (cons (list-ref vals pos) (apply-env-all-ref env sym))
;;                                  (apply-env-all-ref env sym)))])))
;; 
;; (define apply-global-env-all-ref
;;   (lambda (env sym)
;;     (cases environment env
;;       [extended-env-record (syms vals env)
;;                            (let ([pos (list-find-position sym syms)])
;;                              (if (number? pos)
;;                                  (list (list-ref vals pos))
;;                                  (list)))]
;;       [empty-env-record ()
;;                         (error 'global-env "This should never happen")])))
;; 						
;; (define eval-exp
;;   (let ([identity-proc (lambda (x) x)])
;;    (lambda (exp env)
;;     (cases expression exp
;;       [grab-all-exp (id) (apply-env-all env id)]
;;       [grab-all-set!-exp (grab-all-e exp)
;;                          (cases expression grab-all-e
;;                            [grab-all-exp (id)
;;                                          (let ([set-value (eval-exp exp env)])
;;                                            (map (lambda (env-box) (set-box! env-box set-value))
;;                                                 (apply-env-all-ref env id)))]
;;                            [else (raise 'bad-set!-exp)])]
;; 
;; (define syntax-exp
;;    (lambda (exp)
;;     (cases expression exp
;;       [grab-all-exp (id) (grab-all-exp id)]
;;       [grab-all-set!-exp (grab-all-exp exp) (grab-all-set!-exp (syntax-exp grab-all-exp) (syntax-exp exp))]