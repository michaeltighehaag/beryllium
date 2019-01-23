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

[define-stream [stream-dedup eql? s]
  [if [stream-null? s] stream-null
      [stream-cons [stream-car s]
        [stream-dedup eql?
          [stream-drop-while
            [lambda [x]
              [eql? [stream-car s] x]]
            s]]]]]

[define [stream-number-alike pred? strm]
  [stream-unfold
    [lambda [x] [cons [stream-car [car x]] [cdr x]]]
    [lambda [x] [not [stream-null? [car x]]]]
    [lambda [x] [if [stream-null? [stream-cdr [car x]]]
                  [cons stream-null 0]
                  [if [pred? [stream-car [car x]] [stream-car [stream-cdr [car x]]]]
                    [cons [stream-cdr [car x]] [+ 1 [cdr x]]]
                    [cons [stream-cdr [car x]] 0]]]]
    [cons strm 0]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [jflirc-init lt sc]
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
        
[define [jflirc-iter lt sc]
  [let [[nsc [cons [stream-cdr [car sc]] [stream-cdr [cdr sc]]]]]
    [jflirc-init lt nsc]]]

[define [jf-i-c-init lt sc]
  [let [[ls [car sc]][rs [cdr sc]]]
    [if [or [stream-null? ls] [stream-null? rs]]
      [cons [stream null][stream null]]   
      [let [[l [stream-car ls]][r [stream-car rs]]]
        [if [lt l r]
          [jf-i-c-init lt [cons [stream-drop-while [lambda [x] [lt x r]] ls] rs]]
          [if [lt r l]
            [jf-i-c-init lt [cons ls [stream-drop-while [lambda [x] [lt x l]] rs]]]
            [cons ls rs]]]]]]]    
        
[define [jf-i-c-iter lt sc]
  [let [[nsc [cons [stream-cdr [car sc]] [stream-cdr [cdr sc]]]]]
    [jf-i-c-init lt nsc]]]

[define jmlirc [cons jflirc-init jflirc-iter]]
[define jm-i-c [cons jf-i-c-init jf-i-c-iter]]

[define [strm-join mode lt sc]
  [let [[init [car mode]][iter [cdr mode]]]
    [stream-unfold
      [lambda [x] [list [stream-car [car x]][stream-car [cdr x]]]]
      [lambda [x] [not [and [null? [stream-car [car x]]][null? [stream-car [cdr x]]]]]]
      [lambda [sc] [iter lt sc]]
      [init lt sc]]]]

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

[define [stream-repl-do in-p do-re exit-re]
  [stream-let loop [[r [regexp-match do-re in-p 0]]]
    [if [list? r]          
      [if [regexp-match exit-re [car r]]
        [begin  [stream-cons [cat "" exit-re] stream-null]]
        [begin  [stream-cons [car r]
          [loop [regexp-match do-re in-p 0]]]]]
      stream-null]]]

[define [stream<-repl-cmds cmd arg-list prompt cmd-stream]
  [let-values [[[in-p out-p pid err-p stat]
                [apply values [apply process*/ports #f #f [current-output-port] cmd arg-list]]]]
    [file-stream-buffer-mode out-p 'line]
    [let [[dore [regexp [cat "(" prompt "|[^\n]*\n)"]]]]
      [let [[r [stream-repl-do in-p dore prompt]]] [display-stream r]]
      [stream-for-each
         [lambda [x] [begin
           [displayln x out-p]
           [let [[r [stream-repl-do in-p dore prompt]]]
             [display-stream r]]]]
         cmd-stream]]]]


;********************************************************************************************
;*** Tests ***
;********************************************************************************************

[define [test-strms]
  [let [[ts [stream  2 2 2 2 2 3 4 4 5 6 6 6 7 7 8 9 9 9 10 10]]
        [ts1 [stream 31 0 1 2 2 3 5 7 8 9]]
        [ts2 [stream 1 1 1 2 4 5 5 8 15]]
        [ts3 [stream 31 0 1 2 2 3 05 7 8 9 1 1 0 1 2 4 5 5 8 15 0]]]

    [displayln "num-alike"]
    [define nts [stream-number-alike equal? ts]]
    [stream-for-each displayln nts]
    
    [displayln "chopped"]
    [define unts [strm-chop [lambda [x] [not [equal? 0 [cdr x]]]] nts]]
    [disp-stream unts]

    [displayln "chunk 6"]
    [disp-stream
      [stream-chunk 6 ts]]
 
    [disp-stream [stream-chunk 7 [stream<-cmd "ls -l"]]]
    
    [define jts1 [stream 1 2 4 5 8 9 11]]
    [define jts2 [stream 2 3 5 6 8 9 12]]
    [disp-stream [strm-join jm-i-c < [cons jts1 jts2]]]
    [disp-stream [strm-join jmlirc < [cons jts1 jts2]]]

]]
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;[test-strms]

