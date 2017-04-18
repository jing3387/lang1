;; Compute the nth factorial.
(declare fac i64 (i64))
(define fac (n)
  (if (eq n 0)
      1
      (mul n (fac (sub n 1)))))

;; Return the 5th factorial. Can't go any higher because we're returning a
;; status code.
(declare main i64 ())
(define main ()
  (fac 5))
