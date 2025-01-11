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
  (string->number (apply string (vector->list buff))))

(assert (read-integer (make-output-string "i13e")) 13)
(assert (read-integer (make-output-string "ie")) #false)
(assert (read-integer (make-output-string "i1.3e")) 1.3)

(define (read-list reader)
  (let [(first (read-byte reader))]
    (unless (equal? first l-byte)
      (error! (string-append "err, expected l but found: " (to-string first)))))
  (define parsed-list (mutable-vector))

  (while (not (equal? (peek-byte reader) e-byte))
    (vector-push! parsed-list (read-bencoded-value reader)))

  (vector->list parsed-list))

(define (read-dict reader)

  (let [(first (read-byte reader))]
    (unless (equal? first l-byte)
      (error! (string-append "err, expected l but found: " (to-string first) (to-string l-byte) (equal? l-byte first)))))
  (define parsed-list (mutable-vector))

  (while (not (equal? (peek-byte reader) e-byte))
    (vector-push! parsed-list (read-bencoded-value reader)))

  (vector->list parsed-list))

(define (read-bencoded-value reader)
  (define peeked (peek-byte reader))
  (cond
    [(equal? peeked i-byte) (read-integer reader)]
    [(equal? peeked l-byte) (read-list reader)]
    [(equal? peeked d-byte) (read-dict reader)]
    [(string->number (bytes->string/utf8 (bytes peeked))) (read-string reader)]
    [else (error! "unexpected bencoded value")]))

(read-bencoded-value (make-output-string "i13e"))
(read-bencoded-value (make-output-string "5:hello"))

(assert (read-bencoded-value (make-output-string "l5:helloe")) '("hello"))
(assert (read-bencoded-value (make-output-string "l5:helloi13ee")) '("hello" 13))
(assert (read-bencoded-value (make-output-string "ll5:helloi13eee")) (list (list "hello" 13)))

;;;
