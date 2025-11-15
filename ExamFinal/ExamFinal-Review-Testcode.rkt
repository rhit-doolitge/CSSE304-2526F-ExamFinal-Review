#lang racket


(require "../testcode-base.rkt")
(require "ExamFinal-Review-Problems.rkt")
(provide get-weights get-names individual-test test)

(define expect-raise
  (lambda (exp)
    (with-handlers ([symbol? (lambda (sym) sym)])
      (bool-func-type exp)
      'no-raise)))

; Test 1: Basic save and restore - variables keep their values
(define test1
  (make-checkpointed
   (lambda ()
     (let ((x 0))
       (set! x 10)
       (save-checkpoint 'point1)
       (set! x (+ x 5))
       x))))

; Test 2: Demonstrates mutation persistence across restores
(define test2
  (make-checkpointed
   (lambda ()
     (let ((result '()))
       (set! result (cons 1 result))
       (save-checkpoint 'a)
       (set! result (cons 2 result))
       (save-checkpoint 'b)
       (set! result (cons 3 result))
       result))))

; Test 3: Return value changes behavior
(define test3
  (make-checkpointed
   (lambda ()
     (let ((x 0))
       (set! x 5)
       (when (equal? (save-checkpoint 'start) 'saved)
         (set! x 10))
       x))))

; Test 4: Counter increments on each restore
(define test4
  (make-checkpointed
   (lambda ()
     (let ((count 0))
       (save-checkpoint 'loop)
       (set! count (+ count 1))
       count))))

; Test 5: Using only assignment after checkpoint
(define test5
  (make-checkpointed
   (lambda ()
     (let ((x 0))
       (save-checkpoint 'start)
       (set! x (+ x 5))
       x))))

; Test 6: Multiple checkpoints with overwriting
(define test6
  (make-checkpointed
   (lambda ()
     (let ((val 10))
       (save-checkpoint 'point)
       (set! val (* val 2))
       val))))

(define test
  (make-test
    ; (r)

    (compare-truthiness equal?
                        [(compare-truthiness #t ((5 number?))) #t 1]
                        [(compare-truthiness #t ((5 symbol?))) #f 2]
                        [(compare-truthiness #f (#f)) #t 3]
                        [(compare-truthiness #t ((5 number?) (4 number?) ('works symbol?))) #t 4]
                        [(compare-truthiness #t ((5 number?) #t (4 number?) #t #f)) #f 5]
                        [(compare-truthiness #f ((5 symbol?) (4 symbol?))) #t 6])

    (product-all equal?
                 [(product-all 3 4 5) 60 1]
                 [(product-all 1) 1 2]
                 [(product-all (3 4) (2)) 24 3]
                 [(product-all (5) (2 3)) 30 4]
                 [(product-all 2 2 2 2 (2 3)) 96 4])

    (make-checkpointed equal?
                 ; Basic execution - x=10, then x=15
                 [(test1) 15 1]
                 ; Restore: x is 15 (not restored!), then x=20
                 [(begin (test1) (restore-checkpoint 'point1)) 20 1]
                 
                 ; Normal execution builds list
                 [(test2) '(3 2 1) 1]
                 ; Restore to 'a: result is (3 2 1), cons 2 -> (2 3 2 1), cons 3 -> (3 2 3 2 1)
                 [(begin (test2) (restore-checkpoint 'a)) '(3 2 3 2 1) 1]
                 ; Restore to 'b: result is (3 2 1), cons 3 -> (3 3 2 1)
                 [(begin (test2) (restore-checkpoint 'b)) '(3 3 2 1) 1]
                 
                 ; First call: save-checkpoint returns 'saved, x becomes 10
                 [(test3) 10 1]
                 ; Restore: x is 10, save-checkpoint returns 'restored, when skipped, x stays 10
                 [(begin (test3) (restore-checkpoint 'start)) 10 1]
                 
                 ; Normal: count starts 0, increments to 1
                 [(test4) 1 1]
                 ; Restore: count is 1, increments to 2
                 [(begin (test4) (restore-checkpoint 'loop)) 2 1]
                 ; Another restore: count is 2, increments to 3
                 [(begin (test4) (restore-checkpoint 'loop) (restore-checkpoint 'loop)) 3 1]
                 
                 ; Normal: x=0, add 5, return 5
                 [(test5) 5 1]
                 ; Restore: x=5, add 5, return 10
                 [(begin (test5) (restore-checkpoint 'start)) 10 1]
                 ; Another restore: x=10, add 5, return 15
                 [(begin (test5) (restore-checkpoint 'start) (restore-checkpoint 'start)) 15 1]
                 
                 ; Normal: val=10, double to 20
                 [(test6) 20 1]
                 ; Restore: val=20, double to 40
                 [(begin (test6) (restore-checkpoint 'point)) 40 1]
              )
  

    (bool-func-type equal?
                    ;; ---------- successful typecases ----------
                    [(bool-func-type #t) 'bool 1]
                    [(bool-func-type #f) 'bool 1]
                    [(bool-func-type 'not) 1 1]
                    [(bool-func-type 'and) 2 1]
                    [(bool-func-type 'or)  2 1]
                    [(bool-func-type '(lambda (x) x)) 1 1]
                    [(bool-func-type '(lambda (x y) (not x))) 2 1]
                    [(bool-func-type '(lambda (a b c) (and a (not b)))) 3 1]
                    [(bool-func-type '(lambda () #t)) 0 1]
                    [(bool-func-type '((lambda (x) (not x)) #f)) 'bool 1]
                    [(bool-func-type '((lambda (x y) (or x y)) #f #t)) 'bool 1]
                    [(bool-func-type
                      '((lambda (x y z) (and x (or y z))) #t #f #t))
                     'bool 1]
                    [(bool-func-type
                      '(lambda (y)
                         (and ((lambda (x) x) #t)
                              ((lambda (x) y) #t))))
                     1 1]
                    [(bool-func-type
                      '(lambda (a)
                         ((lambda (x y z) (or x (and a z))) a a a)))
                     1 1]

                    ;; ---------- error cases ----------
                    [(expect-raise '(#t #f)) 'bad-proc 1]
                    [(expect-raise '(((lambda (x) x) #t) #f #t)) 'bad-proc 1]
                    [(expect-raise '((lambda (x) x) #t #f)) 'bad-param-num 1]
                    [(expect-raise '(not #t #f)) 'bad-param-num 1]
                    [(expect-raise '(and #t)) 'bad-param-num 1]
                    [(expect-raise '(or #t #f #t)) 'bad-param-num 1]
                    [(expect-raise '((lambda (x z) y) #t #f)) 'unbound-var 1]
                    [(expect-raise '(lambda (y) (or ((lambda (x) x) #t)
                                                    ((lambda (z) x) #t)))) 'unbound-var 1]
                    [(expect-raise '((lambda (x) x) (lambda (y) y))) 'closure-param 1]
                    [(expect-raise '((lambda (x y) (not x)) (lambda (w) w) #t))
                     'closure-param 1]
                    [(expect-raise '(lambda (x) (lambda (x) #t))) 'closure-return 1]
                    [(expect-raise '(lambda (x) and)) 'closure-return 1]
                    [(expect-raise '(lambda () (lambda (x) x))) 'closure-return 1]
                    )
  
  (grab-all equal?
      [(eval-one-exp '(grab-all x)) '() 1] ; (run-test grab-all 1)
      [(length (eval-one-exp '(grab-all +))) 1 1] ; (run-test grab-all 2)
      [(eval-one-exp '(let ([x 2]) (grab-all x))) '(2) 1] ; (run-test grab-all 3)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (grab-all x)))) '(3 2) 1] ; (run-test grab-all 4)
      [(eval-one-exp '(let ([x 2]) (let ([y 3]) (grab-all x)))) '(2) 1] ; (run-test grab-all 5)
      [(eval-one-exp '(let ([y 2]) (let ([x 3]) (grab-all x)))) '(3) 1] ; (run-test grab-all 6)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) 4)) x)) 4 1] ; (run-test grab-all 7)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) 4) x))) 4 1] ; (run-test grab-all 8)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) 4) (grab-all x)))) '(4 4) 1] ; (run-test grab-all 9)
      [(eval-one-exp '(let ([x 2]) (let ([y 3]) (set! (grab-all x) 4) y))) 3 1] ; (run-test grab-all 10)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) (add1 x)) (grab-all x)))) '(4 4) 1] ; (run-test grab-all 11)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! (grab-all x) (grab-all x)) (grab-all x)))) '((3 2) (3 2)) 1] ; (run-test grab-all 10)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! x 4) x))) 4 1] ; (run-test grab-all 11)
      [(eval-one-exp '(let ([x 2]) (let ([x 3]) (set! x 4)) x)) 2 1] ; (run-test grab-all 12)
      [(length (eval-one-exp '(let ([+ 2]) (set! + 3) (grab-all +)))) 2 1] ; (run-test grab-all 13)
   )
 ))

(implicit-run test) ; Run tests as soon as this file is loaded
