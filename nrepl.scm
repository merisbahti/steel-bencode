; starting client
;  clj -Sdeps '{:deps {nrepl/nrepl {:mvn/version "1.3.1"}}}' -M -m nrepl.cmdline
; echo -n 'd2:op4:eval4:code10:(+ 1337 1)e' | netcat 127.0.0.1 64561

(require-builtin steel/tcp)
(require "./test.scm")
(require "./bencode.scm")

(define nrepl-command
  (command "clj"
    (list "-Sdeps" "{:deps {nrepl/nrepl {:mvn/version \"1.3.1\"}}}" "-M" "-m" "nrepl.cmdline")))

(set-piped-stdout! nrepl-command)

(define nrepl-process (Ok->value (spawn-process nrepl-command)))

(define newline (string-ref "\n" 0))
(define (read-until-eof port)
  (define latest-char (None))
  (define buff (mutable-vector))
  (while
    (not (equal? latest-char newline))
    (begin
      (set! latest-char (read-char port))
      (vector-push! buff latest-char)))
  (apply string (vector->list buff)))

(define process-stdout (child-stdout nrepl-process))
(define process-stderr (child-stderr nrepl-process))

(define (get-port-from-start-string str)
  (define starting-str "nREPL server started on port ")
  (define rest-of-str (if (starts-with? str starting-str)
                       (substring str (string-length starting-str))
                       (raise-error "failed getting port from started process")))
  (car (split-once rest-of-str " ")))
(assert (get-port-from-start-string "nREPL server started on port 65433 on host") "65433")
(assert (get-port-from-start-string "nREPL server started on port 337 on host") "337")

(define nrepl-port (get-port-from-start-string (read-until-eof process-stdout)))

(define stream (tcp-connect (string-append "127.0.0.1" ":" nrepl-port)))

(define reader (tcp-stream-reader stream))
(define writer (tcp-stream-writer stream))

(define op (hash "op" "eval" "code" "(map println [1 2 3])"))
(define sent (bencode op))
(displayln "sending " op " as " sent)
(write-string sent writer)

(displayln (read-bencoded-value reader))
(displayln (read-bencoded-value reader))
(displayln (read-bencoded-value reader))

; (displayln (read-until-eof reader))

; stuff?
