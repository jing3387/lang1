(declare fac () i64 (i64))
(define fac (n)
  (if (eq n 0)
      1
      (mul (fac (sub n 1)))))

(declare main () i64 ())
(define main ()
  (fac 5))
