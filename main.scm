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

(define (parse-error . values) (Err (string-join
                                     (map (fn (x) (if (string? x) x (to-string x))) values))))
(define (parse-ok value rest) (cons value rest))

(define (parse-const constant)
  (fn (input)
    (if
      (starts-with? input constant)
      (parse-ok constant (substring input (string-length constant)))
      (parse-error "Expected " constant " but found: " input))))

(assert ((parse-const "hello") "hello world") (parse-ok "hello" " world"))
(assert ((parse-const "hellox") "hello world") (parse-error "Expected hellox but found: hello world"))

(define (parse-map parser mapper)
  (fn (input)
    (define result (parser input))
    (if (Err? result) result (cons (mapper (car result)) (cdr result)))))
(assert ((parse-map (parse-const "1") string->int) "1") (parse-ok 1 ""))
(assert ((parse-map (parse-const "1") string->int) "0") (parse-error "Expected 1 but found: 0"))

(define (parse-pred pred)
  (fn (input)
    (when (eq? (string-length input) 0) (return! (parse-error "Applied parse-pred to empty string")))
    (define first-char (substring input 0 1))
    (if
      (pred first-char)
      (parse-ok first-char (substring input 1))
      (parse-error "char did not match predicate: " first-char))))
(assert ((parse-pred string->number) "1") (parse-ok "1" ""))
(assert ((parse-pred string->number) "d") (parse-error "char did not match predicate: d"))

(define (parse-many parser)
  (parse-map
    (fn (input)
      (define (iter input acc)
        (when (empty? input) (return! acc))
        (define result (parser input))

        (if (Err? result)
          (parse-ok acc input)
          (iter (cdr result) (immutable-vector-push acc (car result)))))
      (iter input (immutable-vector)))
    immutable-vector->list))

(assert ((parse-many (parse-const "a")) "aaaaab") (parse-ok '("a" "a" "a" "a" "a") "b"))
(assert ((parse-many (parse-const "a")) "qaaaaab") (parse-ok '() "qaaaaab"))

(define (parse-integer)
  (fn (input)
    (define parsed ((parse-many (parse-pred string->number)) input))

    (if
      (null? (car parsed))
      (parse-error "Expected number but found: " input)
      (parse-ok (string->number (string-join (car parsed))) (cdr parsed)))))

(assert ((parse-integer) "10") (parse-ok 10 ""))
(assert ((parse-integer) "asbc") (parse-error "Expected number but found: asbc"))

(define (parse-maybe parser)
  (fn (input)
    (define result (parser input))
    (if (Err? result)
      (cons '() input)
      result)))

(define (parse-one . xs)
  (fn (input)
    (if (empty? xs) (parse-error "No valid parsers passed to " (function-name parse-one)))
    (define (iter parsers errors-so-far)
      (if (null? parsers) (return! (Err errors-so-far)))
      (define current-result ((car parsers) input))
      (if
        (pair? current-result)
        current-result
        (iter (cdr parsers) (cons current-result errors-so-far))))
    (define result (iter xs '()))
    (if
      (Err? result)
      (parse-error
        "Got the following failures:\n"
        (string-join (map (fn (x) (~> x (Err->value) (string-append "\n"))) (Err->value result))))
      result)))
(assert ((parse-one (parse-const "a") (parse-const "b")) "abb") (parse-ok "a" "bb"))
(assert ((parse-one (parse-const "a") (parse-const "b")) "baa") (parse-ok "b" "aa"))
(assert ((parse-one (parse-const "a") (parse-const "b")) "q")
  (Err
    "Got the following failures:\nExpected b but found: q\nExpected a but found: q\n"))

(define (parse-all . xs) (fn (input)
                          (define (iter parsers input acc)
                            (when (empty? parsers) (return! (cons (reverse acc) input)))

                            (define result ((car parsers) input))
                            (if (Err? result)
                              result
                              (iter (cdr parsers) (cdr result) (cons (car result) acc))))
                          (iter xs input '())))
(assert ((parse-all (parse-const "a") (parse-const "b")) "abb") (parse-ok (list "a" "b") "b"))
(assert ((parse-all (parse-const "a") (parse-const "b")) "aqb") (parse-error "Expected b but found: qb"))

(define (parse-number)
  (parse-map
    (parse-one
      (parse-all
        (parse-maybe (parse-const "-"))
        (parse-integer)
        (parse-const ".")
        (parse-integer))
      (parse-all
        (parse-maybe (parse-const "-"))
        (parse-integer)))
    (fn (values)
      (define multiplier (if (null? (car values)) 1 -1))
      (* multiplier (string->number (string-join (map to-string (cdr values))))))))

(assert ((parse-number) "-1") (parse-ok -1 ""))
(assert ((parse-number) "1") (parse-ok 1 ""))
(assert ((parse-number) "-0") (parse-ok 0 ""))
(assert ((parse-number) "-1000") (parse-ok -1000 ""))

(assert ((parse-number) "-1.1000") (parse-ok -1.1000 ""))

(assert ((parse-number) "1234567890") (parse-ok 1234567890 ""))
(assert ((parse-number) "1234567890hello") (parse-ok 1234567890 "hello"))

(define (parse-byte-string)
  (fn (input)
    (define initial-result
      ((parse-all
          (parse-integer)
          (parse-const ":"))
        input))
    (when (Err? initial-result) (return! initial-result))
    (define bytes-to-parse (caar initial-result))
    (define input (cdr initial-result))

    (define buffer (~>
                    (substring input 0 bytes-to-parse)
                    (string->bytes)
                    (bytes->list)))

    (define bytes (take buffer bytes-to-parse))
    (define rest (drop buffer bytes-to-parse))

    (parse-ok
      (bytes->string/utf8 (list->bytes bytes))
      (string-append
        (bytes->string/utf8 (list->bytes rest))
        (substring input bytes-to-parse)))))

(assert ((parse-byte-string) "3:hello") (parse-ok "hel" "lo"))
