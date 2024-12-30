(define *failed-assertions* (vector))
(define-syntax assert
  (syntax-rules ()
    [(assert a b)
      (let* (
             [aEvaled a]
             [bEvaled b]
             [aStringified (trim-start-matches (to-string 'a) "'")]
             [bStringified (trim-start-matches (to-string 'b) "'")])
        (when
          (not (equal? aEvaled bEvaled))
          (error-with-span
            (list
              (first (#%syntax-span a))
              (second (#%syntax-span b))
              (third (#%syntax-span a)))
            (string-join
              (list
                (to-string)
                aStringified
                " is not equal to "
                bStringified
                " ("
                (to-string aEvaled)
                " !== "
                (to-string bEvaled)
                ")")))))]))

(define (run-tests)
  (for-each (lambda (x) (displayln x)) (vector->list *failed-assertions*)))

(provide ref)
(define (ref obj . keys)
  (if
    (or (None? obj) (null? keys))
    obj
    (apply ref (cons
                (begin
                  (define key (car keys))
                  (with-handler
                    (lambda (_) (None))
                    (cond
                      [(string? obj) (substring obj key (+ key 1))]
                      [(vector? obj) (vector-ref obj key)]
                      [(list? obj) (list-ref obj key)])))
                (cdr keys)))))
(assert (ref (list 1 2 3) 2) 3)
(assert (ref (list (list (list "hello3"))) 0 0 0 5) "3")

(provide get-char)
(define (get-char str-vec x y)
  (ref str-vec y x))

(define get-char-test (split-many "abc
def
ghi"
                       "\n"))

(assert (get-char get-char-test 0 0) "a")
(assert (get-char get-char-test 2 0) "c")
(assert (get-char get-char-test 3 0) (None))
(assert (get-char get-char-test 0 2) "g")
(assert (get-char get-char-test 0 3) (None))
(assert (get-char get-char-test 2 2) "i")

;; assert(1+2, 3+4)
