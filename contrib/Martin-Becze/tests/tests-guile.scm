(use-modules (srfi srfi-64)
             (srfi srfi-89))

(test-begin "define* test")

(define* simple 1)
(test-equal 1 simple)

(define* (simple2) 2)
(test-equal 2 (simple2))

(define* (named  (#:b a 1)) a)
(test-equal  "a" (named #:b "a"))

(define* (multi-named (#:b a) (#:a z 2)) (list a z))
(test-equal (list 5 2) (multi-named #:b 5))

(define* (multi-named-samename (#:b b 1) (#:a z 2)) (list b z))
(test-equal (list 1 2) (multi-named-samename))

(define* (named-positional (#:b a 1) z) (list a z))
(test-equal (list 1 2) (named-positional 2))

(define* (named-positional-optional (#:b a 1) z (y 3)) (list a z y))
(test-equal (list 1 2 3) (named-positional-optional 2))

(define* (named-positional-optional-rest (#:b a 1) z (y 3) . r) (list a z y r))
(test-equal (list 1 2 'z (list 4)) (named-positional-optional-rest 2 'z 4))

(define* (positional x) x)
(test-equal 1 (positional 1))

(define* (positional-optional a (b #f)) (list a b))
(test-equal '(1 #f) (positional-optional 1))

(define* (optional (b #f)) b)
(test-equal #f (optional))

(define* (positional-named-rest a (#:key k 7) . r)
  (list a k r))

(define* (optional-named-rest (a 8) (#:key k 7) . r)
  (list a k r))

(test-equal '(8 7 ()) (optional-named-rest))

(test-error (test-read-eval-string
	     "(define* (test a b (c 1) (#:v z) f) (list a b c z))"))

(test-error (test-read-eval-string
	     "(define* (test  (#:v z) a b (c 1) (#:f f)) (list a b c z))"))

(test-end "define* test")
