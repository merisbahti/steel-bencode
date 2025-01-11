(require "./test.scm")

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

(define (read-bencoded-string reader)
  (define nrbuff (bytes))

  (while (string->number (bytes->string/utf8 (bytes (peek-byte reader))))
    (bytes-push! nrbuff (read-byte reader)))

  (define parsed-nr (bytes->string/utf8 nrbuff))
  (unless parsed-nr (error! "expected number, but found " nrbuff))

  (let [(nextchar (read-char reader))]
    (unless (equal? nextchar #\:) (error! (string-append "expected `:` but found: " (to-string nextchar)))))

  (bytes->string/utf8 (read-bytes (string->number parsed-nr) reader)))

(assert (read-bencoded-string (make-output-string "5:hello")) "hello")
(assert (read-bencoded-string (make-output-string "3hello")) "hello")
(assert (read-bencoded-string (make-output-string "3:hello")) "hello")

(define (read-integer reader)
  (define firstchar (peek-byte reader))
  (when (not (equal? firstchar i-byte))
    (return! (None)))
  (read-char reader) ;; read the i-byte
  (define buff (mutable-vector))
  (while (not (equal? (peek-byte reader) e-byte))
    (vector-push! buff (read-char reader)))
  (string->number (apply string (vector->list buff))))

(assert (read-integer (make-output-string "i13e")) 13)
(assert (read-integer (make-output-string "ie")) #false)
(assert (read-integer (make-output-string "i1.3e")) 1.3)

(define (read-bencoded-value reader)
  (define peeked (bytes->string/utf8 (bytes (peek-byte reader))))
  (displayln peeked)
  (cond
    [(string->number peeked) (read-bencoded-string reader)]
    [(equal? peeked "i") (read-integer reader)]
    [else (error! "unexpected bencoded value")]))

(read-bencoded-value (make-output-string "i13e"))
(read-bencoded-value (make-output-string "5:hello"))

;;;
