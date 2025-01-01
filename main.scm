(require "./test.scm")
(require "steel/sorting/quick-sort.scm")

(provide bencode)
(define (bencode value)
  (cond
    [(string? value)
      (string-join
        (list
          (to-string (bytes-length (string->bytes value)))
          ":"
          value))]
    [(number? value)
      (string-join
        (list "i" (to-string value) "e"))]

    [(list? value)
      (~> (string-join (cons "l" (map bencode value))) (string-append "e"))]
    [(hash? value)
      (~> (string-join (cons "d"
                        (transduce
                          (~> value (hash-keys->list) (quicksort string<?))
                          (flat-mapping (fn (k) (list (bencode k) (bencode (hash-ref value k)))))
                          (into-list))))
        (string-append "e"))]
    [else (error! (string-join (list "cannot bencode value: " (value->string value))))]))

(assert (bencode "bencode") "7:bencode")
(assert (bencode "") "0:")
(assert (bencode 7) "i7e")
(assert (bencode 7.5) "i7.5e")
(assert (bencode -7.5) "i-7.5e")
(assert (bencode -7.5) "i-7.5e")
(assert (bencode '()) "le")
(assert (bencode '(1 2 3)) "li1ei2ei3ee")
(assert (bencode '("bencode" 2 3)) "l7:bencodei2ei3ee")
(assert (bencode (hash "meaning" 42 "wiki" "bencode")) "d4:wiki7:bencode7:meaningi42ee")
(assert (bencode (hash)) "de")
(assert (bencode displayln) "de")
(assert (bencode (hash)) "de")

(define (parse-error . values) (Err (string-join (map to-string values))))
(define (parse-ok value rest) (cons value rest))

(define (parse-const constant)
  (fn (input)
    (if
      (starts-with? input constant)
      (parse-ok constant (substring input (string-length constant)))
      (parse-error "Expected " constant " but found: " input))))

(assert ((parse-const "hello") "hello world") (parse-ok "hello" " world"))
(assert ((parse-const "hellox") "hello world") (parse-error "Expected hellox but found: hello world"))

(define (parse-integer)
  (fn (input)
    (define (iter input acc)
      (if (zero? (string-length input)) (return! acc))
      (define first-chr (substring input 0 1))
      (if (string->number first-chr)
        (iter (substring input 1) (string-append acc first-chr))
        acc))
    (define parsed (iter input ""))

    (if
      (eq? parsed "")
      (parse-error "Expected number but found: " input)
      (parse-ok (string->number parsed) (substring input (string-length parsed))))))
(assert ((parse-integer) "10") (parse-ok 10 ""))
(assert ((parse-integer) "asbc") (parse-error "Expected number but found: asbc"))

(define (parse-number)
  (fn (input)
    (define is-neg ((parse-const "-") input))
    (define neg-rest (if (Err? is-neg) input (cdr is-neg)))
    (define whole-part ((parse-integer) neg-rest))
    (when (Err? whole-part) (return! whole-part))

    (define decimal-point ((parse-const ".") (cdr whole-part)))
    (when (Err? decimal-point) (return!
                                (parse-ok
                                  (if (pair? is-neg) (* -1 (car whole-part)) (car whole-part))
                                  (cdr whole-part))))

    (define decimal-part ((parse-integer) (cdr decimal-point)))

    (parse-ok (string->number (string-join (list
                                            (if (pair? is-neg) "-" "")
                                            (to-string (car whole-part))
                                            "."
                                            (to-string (car decimal-part)))))

      (cdr decimal-part))))
(assert ((parse-number) "-1") (parse-ok -1 ""))
(assert ((parse-number) "1") (parse-ok 1 ""))
(assert ((parse-number) "-0") (parse-ok 0 ""))
(assert ((parse-number) "-1000") (parse-ok -1000 ""))
(assert ((parse-number) "-1.1000") (parse-ok -1.1000 ""))

(assert ((parse-number) "1234567890") (parse-ok 1234567890 ""))
(assert ((parse-number) "1234567890ello") (parse-ok 1234567890 "hello"))
(define (bencode-parse input-string) ())
(assert (bencode-parse "2:ö") "ö")
