;; Booleans
(declare true : a -> b -> a)
(define true
  (lambda (a b)
    a))

(declare false : a -> b -> b)
(define false
  (lambda (a b)
    b))
