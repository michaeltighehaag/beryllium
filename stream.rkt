#lang racket
#|
Macros and functions derived from srfi/41 Streams library
including some examples taken from the documentation for srfi/41
|#

[require racket/require
  [path-up "common.rkt"]]

[provide [all-defined-out]]

[require srfi/41]

[provide [all-from-out srfi/41]]

;*******************************************************************************************
;******add reversed names*******************************************************************
;*******************************************************************************************

(define (stream<-list objs)
    (define stream<-list
      (stream-lambda (objs)
        (if (null? objs)
            stream-null
            (stream-cons (car objs) (stream<-list  (cdr objs))))))
    (if (not (list? objs))
        (error 'list->stream "non-list argument")
        (stream<-list objs)))

  (define (stream<-port . port)
    (define stream<-port
      (stream-lambda (p)
        (let ((c (read-char p)))
          (if (eof-object? c)
              stream-null
              (stream-cons c (stream<-port p))))))
    (let ((p (if (null? port) (current-input-port) (car port))))
      (if (not (input-port? p))
          (error 'stream<-port "non-input-port argument")
          (stream<-port p))))

  (define (list<-stream . args)
    (let ((n (if (= 1 (length args)) #f (car args)))
          (strm (if (= 1 (length args)) (car args) (cadr args))))
      (cond ((not (stream? strm)) (error 'list<-stream "non-stream argument"))
            ((and n (not (integer? n))) (error 'list<-stream "non-integer count"))
            ((and n (negative? n)) (error 'list<-stream "negative count"))
            (else (let loop ((n (if n n -1)) (strm strm))
                    (if (or (zero? n) (stream-null? strm))
                        '()
                        (cons (stream-car strm) (loop (- n 1) (stream-cdr strm)))))))))

;*******************************************************************************************
;*******************************************************************************************
;*** Streams ***
;***************

[define [disp-stream x [o [current-output-port]]]
  [if [stream? x]
    [begin [displayln "-" o] [stream-for-each [lambda [y] [disp-stream y o]] x]]
    [displayln x o]]]

[define [display-stream x [o [current-output-port]]]
  [if [stream? x]
    [begin [displayln "-" o] [stream-for-each [lambda [y] [display-stream y o]] x]]
    [display x o]]]


[define-stream [stream-chunk n strm]
  [if [stream-null? strm]
    stream-null
    [stream-cons
      [stream-take n strm]
      [stream-chunk n [stream-drop n strm]]]]]

[define-stream [strm-chop pred? strm]
  [if [stream-null? strm]
    stream-null
    [stream-cons
      [stream-cons [stream-car strm] [stream-take-while pred? [stream-cdr strm]]]
      [strm-chop pred? [stream-drop-while pred? [stream-cdr strm]]]]]]

[define-stream [stream-group pred? strm]
  [if [stream-null? strm]
    stream-null
    [let [[cur [stream-car strm]]]
    [stream-cons
      [stream-cons cur [stream-take-while [lambda [x] [pred? x cur]] [stream-cdr strm]]]
      [stream-group pred? [stream-drop-while [lambda [x] [pred? x cur]] [stream-cdr strm]]]]]]]

[define [stream-partition pred? strm]
  [stream-unfolds
    [lambda [seed]
      [if [stream-null? seed]
          [values seed '[] '[]]
          [let [[a [stream-car seed]]
                [d [stream-cdr seed]]]
            [if [pred? a]
                [values d [list a] #f]
                [values d #f [list a]]]]]]
    strm]]

[define-stream [stream-dedup eql? s]
  [if [stream-null? s] stream-null
      [stream-cons [stream-car s]
        [stream-dedup eql?
          [stream-drop-while
            [lambda [x]
              [eql? [stream-car s] x]]
            s]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define-stream [stream-merge lt? . strms]
  [define-stream [merge xx yy]
    [stream-match xx [[] yy] [[x . xs]
      [stream-match yy [[] xx] [[y . ys]
        [if [lt? y x]
            [stream-cons y [merge xx ys]]
            [stream-cons x [merge xs yy]]]]]]]]
  [stream-let loop [[strms strms]]
    [cond [[null? strms] stream-null]
          [[null? [cdr strms]] [car strms]]
          [else [merge [car strms]
                       [apply stream-merge lt?
                         [cdr strms]]]]]]]

[define [rec-stream-sort lt? s]
  [if [stream-null? [stream-cdr s]] s
    [rec-stream-sort lt?
      [stream-map
        [lambda [x] [if [stream-null? [stream-cdr x]]
                      [stream-car x]
                      [stream-merge lt? [stream-car x] [stream-car [stream-cdr x]]]]]
        [stream-chunk 2 s]]]]]

[define [stream-sort lt? s] [stream-car [rec-stream-sort lt? [stream-map [lambda [x] [stream x]] s]]]]

[define-stream [qsort lt? s]
  [if [stream-null? s]
      stream-null
      [let [[x [stream-car s]]
            [xs [stream-cdr s]]]
        [stream-append
          [qsort lt?
            [stream-filter
              [lambda [u] [lt? u x]]
              xs]]
          [stream x]
          [qsort lt?
            [stream-filter
              [lambda [u] [not [lt? u x]]]
              xs]]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [join-base lt sc]
  [let [[ls [car sc]][rs [cdr sc]]]
    [if [stream-null? ls]
      [if [stream-null? rs]
        [cons [stream null][stream null]]   
        [cons [stream-cons null ls] rs]]
      [if [stream-null? rs]
        [cons ls [stream-cons null rs]]
        [let [[l [stream-car ls]][r [stream-car rs]]]
          [if [lt l r]
            [cons ls [stream-cons null rs]]       
            [if [lt r l]
              [cons [stream-cons null ls] rs]
              [cons ls rs]]]]]]]]    
        
[define [join-iter lt sc]
  [let [[nsc [cons [stream-cdr [car sc]] [stream-cdr [cdr sc]]]]]
    [join-base lt nsc]]]

[define [stream-join lt ls rs]
    [let [[sc [cons ls rs]]]
      [stream-unfold
        [lambda [x] [list [stream-car [car x]][stream-car [cdr x]]]]
        [lambda [x] [not [and [null? [stream-car [car x]]][null? [stream-car [cdr x]]]]]]
        [lambda [x] [join-iter lt x]]
        [join-base lt sc]]]]

;filters left left-excl xor and right right-excl
[define [join-filter-left x] [not [null? [car x]]]]
[define [join-filter-left-excl x] [and [not [null? [car x]]] [null? [cdr x]]]]
[define [join-filter-right x] [not [null? [cdr x]]]]
[define [join-filter-right-excl x] [and [not [null? [cdr x]]] [null? [car x]]]]
[define [join-filter-xor x] [xor [not [null? [car x]]] [not [null? [cdr x]]]]]
[define [join-filter-and x] [and [not [null? [car x]]] [not [null? [cdr x]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define-stream [stream<-file filename]
  [let [[p [open-input-file filename]]]
    [stream-let loop [[c [read-line p]]]
      [if [eof-object? c]
          [begin [close-input-port p]
                 stream-null]
          [stream-cons c
            [loop [read-line p]]]]]]]

[define [file<-stream file pf strm]
  [let [[o [open-output-file file #:exists 'replace]]]
    [stream-for-each [lambda [x] [pf x o]] strm]
    [close-output-port o]]]

[define [stream<-cmd cmd]
  [let-values [[[in-p out-p pid err-p stat]
                [apply values [process/ports #f #f [current-output-port] cmd ]]]]
    [stream-let loop [[c [read-line in-p]]]
      [if [eof-object? c]
          [begin [close-input-port in-p]
                 stream-null]
          [stream-cons c
            [loop [read-line in-p]]]]]]]

[define-stream [stream<-rxm-file rx filename]
  [let [[p [open-input-file filename]]]
    [stream-let loop [[c [regexp-match rx p]]]
      [if [equal? #f c]
          [begin [close-input-port p] stream-null]
          [stream-cons c [loop [regexp-match rx p]]]]]]]

;********************************************************************************************

[define [stream<-repl cmd arg-list strm]
  [let-values [[[in-p out-p pid err-p stat]
                [apply values [apply process*/ports #f #f [current-output-port] cmd arg-list]]]]
    [file-stream-buffer-mode out-p 'line]
    [stream-for-each [lambda [x] [displayln x out-p]] strm]
    [stream-let loop [[c [read-line in-p]]]
      [if [eof-object? c]
          [begin [close-input-port in-p]
                 stream-null]
          [stream-cons c
            [loop [read-line in-p]]]]]]]

;********************************************************************************************
;*** Tests ***
;********************************************************************************************

[define [test-strms]
  [let [[ts [stream  2 2 2 2 2 3 4 4 5 6 6 6 7 7 8 9 9 9 10 10]]
        [tsm [stream  2 2 2 2 2 5 6 6 6 7 7 4 4 8 9 9 9 3 10 10]]
        [ts1 [stream 31 0 1 2 2 3 5 7 8 9]]
        [ts2 [stream 1 1 1 2 4 5 5 8 15]]
        [ts3 [stream 31 0 1 2 2 3 05 7 8 9 1 1 0 1 2 4 5 5 8 15 0]]]

    [displayln "chunk 6"]
    [disp-stream [stream-chunk 6 ts]]

    [displayln "group and apply zip"]
    [disp-stream [stream-map [lambda [x] [stream-zip x [stream-from 1]]] [stream-group equal? tsm]]]
    
    [disp-stream [stream-chunk 7 [stream<-cmd "ls -l"]]]
    
    [define jts1 [stream 1 2 4 5 5 5 8 9 11]]
    [define jts2 [stream 2 3 5 6 8 8 9 12]]
    [displayln "full join"]
    [disp-stream [stream-join < jts1 jts2]]


]]
 
[define [test-join x y]
  [let [[a [stream-sort < [stream-map [lambda [z] [random x]] [stream-take y [stream-from 1]]]]]
        [b [stream-sort < [stream-map [lambda [z] [random x]] [stream-take y [stream-from 1]]]]]]
     [let [[r [stream-join < a b]]]
       [time [disp-stream r]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
