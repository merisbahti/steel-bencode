(require "./test.scm")
(require "steel/sorting/quick-sort.scm")

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
