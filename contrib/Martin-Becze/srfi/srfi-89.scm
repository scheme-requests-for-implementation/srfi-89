;;; (srfi srfi-89) --- Guile srfi-89 implementation.

;; Copyright (C) 2020 Martin Becze <mjbecze@riseup.net>
;;
;; This file is part of guile-srfi-89.
;;
;; guile-srfi-89 is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-json is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-json. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; A hygenic srfi-89 for Guile

;;; Code:

(library (srfi srfi-89)
  (export define* lambda*)
  (import (rnrs)
	  (rnrs syntax-case (6))
	  (prefix (guile) guile:)))

(guile:cond-expand-provide (guile:current-module) '(srfi-89))

(define-syntax with-syntax*
  (syntax-rules ()
    ((_ ((i v)) e1 e2 ...)
     (with-syntax ((i v))
       e1 e2 ...))
    ((_ ((i v) i1 i2 ...) e1 e2 ...)
     (with-syntax ((i v))
       (with-syntax* (i1 i2 ...)
		     e1 e2 ...)))))

(define (remove-keywords ls)
  (cond
   ((null? ls) ls)
   ((guile:keyword? (car ls)) (remove-keywords (cddr ls)))
   (else ls)))

(define-syntax lambda*
  (lambda (x)
    (define (generate-temporary)
      (car (generate-temporaries '(_))))

    (define (kword->sym x keyword)
      (datum->syntax x (guile:keyword->symbol (syntax->datum keyword))))

    (define (required-positional? var)
      (identifier? var))

    (define (optional-positional? var)
      (syntax-case var ()
	((var exp)
	 (identifier? #'var) #t)
	(else #f)))

    (define (positional? var)
      (or (required-positional? var)
	  (optional-positional? var)))

    (define (generate-key-error-message x)
      (string-append "key " (symbol->string (syntax->datum x)) " is required"))

    (define (zip a b)
      (syntax-case (list a b) ()
	((() ()) #'())
	(((a) (b)) #'((a b)))
	(((a a1 ...) (b b1 ...))
	 (with-syntax (((r ...) (zip #'(a1 ...) #'(b1 ...))))
	   #'((a b) r ...)))))

    (define (keyword? x)
      (guile:keyword? (syntax->datum x)))

    ;; <named> --> <required named> |  <optional named>
    (define (parse-named ctx x)
      (syntax-case x ()
	;; <required named> --> ( <keyword> <variable> )
	(((key var) n ...)
	 (keyword? #'key)
	 (with-syntax* ((k (kword->sym ctx #'key))
			(((named ...) (vars ...) r) (parse-named ctx #'(n ...)))
			(error-message (generate-key-error-message #'k)))

		       #'(((k (guile:error error-message)) named ...) (var vars ...) r)))
	;; <optional named> --> ( <keyword> <variable> <expression> )
	(((key var exp) n ...)
	 (keyword? #'key)
	 (with-syntax* ((k (kword->sym ctx #'key))
			(((named ...) (vars ...) r) (parse-named ctx #'(n ...))))

		       #'(((k exp) named ...) (var vars ...) r)))
	((rest ...)
	 #'(() () (rest ...)))))

    ;; <positional section> --> <required positional>* <optional positional>*
    (define (parse-pos x)
      (syntax-case x ()
	(() #'(() () ()))
	((var r ...)
	 (if (required-positional? #'var)
	     (with-syntax ((((pos ...) opts rest) (parse-pos #'(r ...))))
	       #'((var pos ...) opts rest))
	     (parse-pos-optional x)))))

    ;; <optional positional> --> ( <variable> <expression> )
    (define (parse-pos-optional x)
      (syntax-case x ()
	(((var exp) r ...)
	 (identifier? #'var)
	 (with-syntax (((pos (opts ...) rest) (parse-pos-optional #'(r ...))))
	   #'(pos ((var exp) opts ...) rest)))
	((rest ...)
	 #'(() () (rest ...)))))

    (syntax-case x ()
      ((_ () body ...) #'(lambda () body ...))
      ((_ (formal formal1 ... . rest) body ...)
       (if (positional? #'formal)
	   ;; <extended def formals> --> <positional section> <named section>? <rest section>
	   (with-syntax* ((((pos ...) (opt ...) r1) (parse-pos #'(formal formal1 ...)))
			  ((((named exp) ...) vars remainder) (parse-named x #'r1))
			  ((wrapper ...) (zip #'vars #'(named ...)))
			  (temp-rest (generate-temporary))
			  ((sanitizeded-rest ...) ((lambda ()
						     (if (identifier? #'rest)
							 #'((rest (remove-keywords temp-rest)))
							 #'())))))

			 (if (null? (syntax->datum #'remainder))
			     #'(guile:lambda* (pos ...
						 #:optional opt ...
						 #:key (named exp) ...
						 #:rest temp-rest)
					    (let (sanitizeded-rest ...
								   wrapper ...)
					      body ...))
			     #'(guile:syntax-error "Invalid parameters" remainder)))
	   ;; <extended def formals> --> <named section>? <positional section> <rest section>
	   (with-syntax* (((((named exp) ...) vars r1) (parse-named x #'(formal formal1 ...)))
			  (((pos ...) (opt ...) remainder) (parse-pos #'r1))
			  ((wrapper ...) (zip #'vars #'(named ...)))
			  (temp-rest (generate-temporary))
			  ((rest ...) ((lambda ()
					 (if (identifier? #'rest)
					     #'(#:rest rest)
					     #'())))))

			 (if (null? (syntax->datum #'remainder))
			     #'(guile:lambda* (#:key (named exp) ... #:rest temp-rest)
					    (let (wrapper ...)
					      (apply
					       (guile:lambda* (pos ...
								   #:optional opt ...
								   rest ...
								   )
							      body ...)
					       (remove-keywords temp-rest))))
			     #'(guile:syntax-error "Invalid parameters" remainder))))))))

(define-syntax define*
  (syntax-rules ()
    ;; define* ( <variable> <extended def formals> ) <body>
    ((_ (var formals ... . rest) body ... )
     (define var
       (lambda* (formals ... . rest) body ...)))
    ;; define* <variable> <expression>
    ((_ var exp)
     (define var exp))))
