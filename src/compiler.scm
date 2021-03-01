(load "test-driver.scm")
(load "tests/1-1.scm")
(load "tests/1-2.scm")

(define (emit-program expr)
  (define expr-type (get-type expr))

  (emit "define dso_local ~a @scheme_entry() #0 {" expr-type)
  (emit "ret ~a ~b" expr-type (number->string (boxed-rep expr)))
  (emit "}"))
  
(define (immediate-rep x)
  (cond
    [(integer? x) (number->string x)]
    [(null? x) 47] ;; 00101111
    [(char? x) (char->integer x)]
    [(boolean? x) (if x 1 0)]
    [else x]))
  
(define (get-type x)
  (cond
    [(integer? x) "i32"]
    [(null? x) "i32"]
    [(char? x) "i32"]
    [(boolean? x) "i32"]
    [else "i8"]))

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
  (if (= n 0)
    0
    (+ (mod n 2)
      (* 10 (number->binary (quotient n 2))))))
