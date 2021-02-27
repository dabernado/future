(load "test-driver.scm")
(load "tests/1-1.scm")

(define (emit-program expr)
  (define (emit-prim x)
    (cond
      (integer? x) (emit "ret i32 ~a" x)))
  
  (emit "define dso_local i32 @scheme_entry() #0 {")
  (emit "ret i32 ~a" expr)
  (emit "}"))
