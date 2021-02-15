(define-library (srfi 89)
  (export define*
	  lambda*
	  $hash-keyword
	  $perfect-hash-table-lookup
	  $undefined
	  $process-keys
	  $opt-key
	  $req-key)

  (import
   (scheme base)
   (srfi 88))

  (cond-expand
   (gauche
    (import
     (except
      (gauche base)
      keyword? keyword->string string->symbol symbol? symbol->string))))

  (include "89.scm"))
