(load "test-driver.scm")
(load "tests/1-1.scm")
(load "tests/1-2.scm")

(define ALLOCA "%~a = alloca ~b, align ~c")
(define STORE "store ~a ~b, ~a* ~c, align ~d")
(define LOAD "%~a = load ~b, ~b* %~c, align ~d")
(define ADD "%~a = add nsw ~b %~c, ~d")
(define RET "ret ~a %~b")
  
(define (get-type x)
  (cond
    [(integer? x) "i32"]
    [(null? x) "i32"]
    [(char? x) "i32"]
    [(boolean? x) "i32"]
    [else "i8"]))
  
(define (type-align x)
  (cond
    [(integer? x) #\4]
    [(null? x) #\4]
    [(char? x) #\4]
    [(boolean? x) #\4]
    [else #\1]))
  
(define (immediate-rep x)
  (cond
    [(integer? x) (number->string x)]
    [(null? x) 47] ;; 00101111
    [(char? x) (char->integer x)]
    [(boolean? x) (if x 1 0)]
    [else x]))

(define (immediate? x)
  (or (integer? x) (null? x) (char? x) (boolean? x)))

(define (primcall? x) (eq? (car x) 'prim-apply))

(define fixnum-shift 2)

(define char-tag 15)
(define char-shift 8)

(define bool-tag 31)
(define bool-shift 8)

(define (boxed-rep x)
  (cond
    [(integer? x) (ash x fixnum-shift)]
    [(null? x) 47] ;; 00101111
    [(boolean? x) (let ([val (if x 1 0)])
                       (logior (ash val bool-shift) bool-tag))]
    [(char? x) (let ([val (char->integer x)])
                    (logior (ash val char-shift) char-tag))]
    [else 0]))

(define (number->binary n)
  (num-bin 0 n))
(define (num-bin acc n)
  (if (= n 0)
    acc
    (num-bin (* 10 (+ acc (mod n 2))) (quotient n 2))))

(define (extend l . xs)
  (if (null? l) 
      xs
      (cons (car l) (apply extend (cdr l) xs))))

(define (hashtable-set hash k v))

(define (primcall-op x) (cadr x))
(define (primcall-args x) (cddr x))

(define (new-register env val)
  `(,(hashtable-size env) ,(hashtable-set! env (hashtable-size env) val)))

(define (emit-apply-args env args)
  (cond
    [(eq? args '()) env]
    [else (let ([alloc (emit-expr env (car args))])
            (emit-apply-args alloc (cdr args)))]))

(define (emit-primcall env expr)
  (case (primcall-op expr)
    [(add1)
     (let ([args-env (emit-apply-args env (reverse (primcall-args expr)))]
           [arg (- (hashtable-size args-env) 1)]
           [alloc
             (new-register args-env (hashtable-ref args-env (- (hashtable-size args-env) 1)))])
       (emit LOAD
             (car alloc)
             (get-type expr)
             arg
             (type-align expr)))
     (let ([result (new-register (cdr alloc) nil)])
       (emit ADD
             (car result)
             (get-type expr)
             (car alloc)
             1)
       (cdr result))]))

(define (emit-expr env expr)
  (cond
    [(immediate? expr)
     (let ([alloc (new-register env expr)])
       (emit ALLOCA (car alloc) (get-type expr) (type-align expr))
       (emit STORE
             (get-type expr)
             (number->string (boxed-rep expr))
             (car alloc)
             (type-align expr))
       (cdr alloc))]
    [(primcall? expr) (emit-primcall env expr)]))

(define (emit-program expr)
  (emit "define dso_local ~a @scheme_entry() #0 {" (get-type expr))
  (let ([env (emit-expr (make-eq-hashtable) expr)])
    (emit LOAD
          (hashtable-size env)
          (- (hashtable-size env) 1))
  (emit RET (get-type expr) (hashtable-size env)))
  (emit "}"))
