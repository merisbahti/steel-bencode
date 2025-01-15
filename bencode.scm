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
(assert (bencode (hash "meaning" 42 "wiki" "bencode")) "d4:wiki7:bencode7:meaningi42ee")
(assert (bencode (hash)) "de")

(assert (call-with-exception-handler (fn (x) (to-string x)) (fn () (bencode displayln))) "Error: Generic:  cannot bencode value: #<bytecode-closure>")

(assert (bencode (hash)) "de")

(define e-byte (bytes-ref (string->bytes "e") 0))
(define i-byte (bytes-ref (string->bytes "i") 0))
(define l-byte (bytes-ref (string->bytes "l") 0))
(define d-byte (bytes-ref (string->bytes "d") 0))

(define (make-output-string str)
  (define cmd (command "echo" (list str)))
  (set-piped-stdout! cmd)
  (define process (Ok->value (spawn-process cmd)))
  (define process-stdout (child-stdout process))
  process-stdout)

(define (read-string reader)
  (define nrbuff (bytes))

  (while (string->number (bytes->string/utf8 (bytes (peek-byte reader))))
    (bytes-push! nrbuff (read-byte reader)))

  (define parsed-nr (bytes->string/utf8 nrbuff))
  (unless parsed-nr (error! "expected number, but found " nrbuff))

  (let [(nextchar (read-char reader))]
    (unless (equal? nextchar #\:) (error! (string-append "expected `:` but found: " (to-string nextchar)))))

  (bytes->string/utf8 (read-bytes (string->number parsed-nr) reader)))

(assert (read-string (make-output-string "5:hello")) "hello")

(define (read-integer reader)
  (define firstchar (peek-byte reader))
  (when (not (equal? firstchar i-byte))
    (return! (None)))
  (read-char reader) ;; read the i-byte
  (define buff (mutable-vector))
  (while (not (equal? (peek-byte reader) e-byte))
    (vector-push! buff (read-char reader)))
  (read-byte reader)
  (string->number (apply string (vector->list buff))))

(assert (read-integer (make-output-string "i13e")) 13)
(assert (read-integer (make-output-string "ie")) #false)
(assert (read-integer (make-output-string "i1.3e")) 1.3)

(define (read-bencoded-value _) _) ;; define later hehe
(define (read-list reader)
  (let [(first (read-byte reader))]
    (unless (equal? first l-byte)
      (error! (string-append "err, expected l but found: " (to-string first)))))
  (define parsed-list (mutable-vector))

  (while (not (equal? (peek-byte reader) e-byte))
    (vector-push! parsed-list (read-bencoded-value reader)))

  (read-byte reader) ;; read the e

  (vector->list parsed-list))

(define (read-dict reader)
  (define (read-pair reader)
    (cons (read-string reader) (read-bencoded-value reader)))

  (let [(first (read-byte reader))]
    (unless (equal? first d-byte)
      (error! (string-append "err, expected d but found: " (to-string first)))))

  (define parsed-pairs (mutable-vector))

  (while (not (equal? (peek-byte reader) e-byte))
    (let [(parsed-pair (read-pair reader))]

      (vector-push! parsed-pairs (car parsed-pair))
      (vector-push! parsed-pairs (cdr parsed-pair))))

  (read-byte reader) ;; read the e

  (apply hash (vector->list parsed-pairs)))

(set! read-bencoded-value
  (fn (reader)
    (define peeked (peek-byte reader))
    (cond
      [(equal? peeked i-byte) (read-integer reader)]
      [(equal? peeked l-byte) (read-list reader)]
      [(equal? peeked d-byte) (read-dict reader)]
      [(string->number (bytes->string/utf8 (bytes peeked))) (read-string reader)]
      [else (error! "unexpected bencoded value")])))

(assert (read-bencoded-value (make-output-string "i13e")) 13)
(assert (read-bencoded-value (make-output-string "5:hello")) "hello")
(assert (read-bencoded-value (make-output-string "l6:hello\nee")) '("hello\n"))
(assert (read-bencoded-value (make-output-string "d4:hehei1e4:hahai1337ee")) (hash "haha" 1337 "hehe" 1))
(provide read-bencoded-value)

; (assert (read-bencoded-value (make-output-string "l5:helloi13ee")) '("hello" 13))
; (assert (read-bencoded-value (make-output-string "ll5:helloi13eee")) (list (list "hello" 13)))

;
