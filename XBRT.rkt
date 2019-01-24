#lang racket
#|
A functional eXtensible Binary Radix Tree implementation.
|#

[require racket/require
  [path-up "common.rkt"]]
[require racket/require
  [path-up "stream.rkt"]]
[require bitsyntax]


[provide [all-defined-out]]

[define [inz n [f [lambda [n] 1]]] [if [null? n] 0 [f n]]]


[struct rbk [bits frst last] #:transparent]

[define [gbk-length k] [+ [rbk-last k] [- [rbk-frst k]]]]
[define [gbk-ref k n] [bit-string-ref [rbk-bits k] [+ [rbk-frst k] n]]]
[define [gbk-cat lb rb] [rbk [rbk-bits rb] [rbk-frst lb] [rbk-last rb]]]
[define [bin-char-str<-gbk k]
  [let [[l [gbk-length k]]]
    [if [equal? l 0] ""
      [pad [bit-string->unsigned-integer [sub-bit-string [rbk-bits k] [rbk-frst k][rbk-last k]] #t] l 2]]]]
[define [gbk-sub-key k s e] [rbk [rbk-bits k] [+ s [rbk-frst k]] [+ e [rbk-frst k]]]]
[define [gbk-fd bs1 s1 bs2 s2]
  [if [and [< s1 [gbk-length bs1]] [< s2 [gbk-length bs2]]]
    [if [equal? [gbk-ref bs1 s1] [gbk-ref bs2 s2]]
      [add1 [gbk-fd bs1 [add1 s1] bs2 [add1 s2]]]
      0 ] 0]]

[define [rbk<-bits b][rbk b 0 [bit-string-length b]]]
[define [rbk<-int v s] [rbk [integer->bit-string v s #t] 0 s]]
[define [rbk<-string str]
  [let* [[bs [string->bytes/utf-8 str]]
         [bits [bit-string [bs :: binary]]]]
    [rbk bits 0 [bit-string-length bits]]]]
[define [rbk<-bin-char-str s]
  [let [[sl [string-length s]]]
    [rbk<-int [if [equal? sl 0] 0 [string->number s 2]] sl]]]

[define [xbrt-kfb? n][gbk-ref [xbrt-key n] 0]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[struct xbrt [key ext left right] #:transparent]


[struct extv [val] #:transparent]

[define xf-def [lambda [va l r x]
  [let [[v [if [null? va] [if [null? x] null [extv-val x]] [if [equal? va [void]] null va]]]]
    [extv v]]]]

[define xf-dac [lambda [va l r x]
  [let [[v [if [null? va]
           [if [null? x] null [extv-val x]]
           [if [equal? va [void]] null
             [if [null? x] [list va] [cons va [extv-val x]] ]]]]]
    [extv v]]]]

[define xf-acf [lambda [va l r x]
  [let [[v [if [null? va]
           [if [null? x] null [extv-val x]]
           [if [equal? va [void]] null
             [if [null? x]
                 [let [[o [open-output-file [car va] #:exists 'replace]]]
                   [displayln [cdr va] o] o]
                 [begin [displayln [cdr va] [extv-val x]] [extv-val x]]] ]]]]
    [extv v]]]]



[struct xval extv [count node-height key-height] #:transparent]

[define [xf-statv v l r]
  [if [null? l]
    [if [null? r]
      [values v [inz v] 0 0]
      [values v [+ [inz v] [xval-count [xbrt-ext r]]]
              [+ 1 [xval-node-height [xbrt-ext r]]]
              [+ [gbk-length [xbrt-key r]] [xval-key-height [xbrt-ext r]]]]]
    [if [null? r]
      [values v [+ [inz v] [xval-count [xbrt-ext l]]]
              [+ 1 [xval-node-height [xbrt-ext l]]]
              [+ [gbk-length [xbrt-key l]] [xval-key-height [xbrt-ext l]]]]
      [values v [+ [inz v] [xval-count [xbrt-ext l]][xval-count [xbrt-ext r]]]
              [+ 1 [max [xval-node-height [xbrt-ext l]][xval-node-height [xbrt-ext r]]]]
              [max [+ [gbk-length [xbrt-key l]][xval-key-height [xbrt-ext l]]]
                   [+ [gbk-length [xbrt-key r]][xval-key-height [xbrt-ext r]]]]]]]]

[define xf-stat [lambda [va l r x]
  [let [[v [if [null? va] [if [null? x] null [extv-val x]] [if [equal? va [void]] null va]]]]
    [call-with-values [lambda [] [xf-statv v l r]] [lambda [v c n k] [xval v c n k]]]]]]


[struct xacc xval [tcount] #:transparent]

[define xf-acc [lambda [va l r x]
[let [[v [if [null? va]
           [if [null? x] null [extv-val x]]
           [if [equal? va [void]] null ;[cons va [extv-val x]]
             [if [null? x] va [cons [+ [car va] [car [extv-val x]]] [cdr va]]]]]]]
  [let [[lc [if [null? v] 0 1]][lacc [if [null? v] 0 [car v]]]]
    [let-values [[[v c n k][xf-statv v l r]]]
    [if [null? l]
      [if [null? r]
        [xacc v c n k lacc]
        [xacc v c n k [+ lacc [xacc-tcount [xbrt-ext r]]]]]
      [if [null? r]
        [xacc v c n k [+ lacc [xacc-tcount [xbrt-ext l]]]]
        [xacc v c n k [+ lacc [xacc-tcount [xbrt-ext l]][xacc-tcount [xbrt-ext r]]]]]]]]]]]

;;;*****

[struct xgen extv [stats accum] #:transparent]

[define [val-def va x] va]

[define [val-file va x]
               [if [null? x]
                 [let [[o [open-output-file [car va] #:exists 'replace]]]
                   [displayln [cdr va] o] o]
                   [begin [displayln [cdr va] [extv-val x]] [extv-val x]]]]

[define [val-sum va x] [if [null? x] va [cons [car va] [+ [cdr va] [cdr [extv-val x]]]]]]
                   
              ;     [displayln [cdr va] o] [cons o [cdr va]]]
              ;   [begin [displayln [cdr va] [car [extv-val x]]] [cons [car [extv-val x]][cdr va]]]]]

[struct statrec [ncount vcount node-height key-height] #:transparent]
[define [xgen-def-stat v l r]
  [let [[ls [if [null? l] [statrec 0 0 0 0] [xgen-stats [xbrt-ext l]]]]
        [rs [if [null? r] [statrec 0 0 0 0] [xgen-stats [xbrt-ext r]]]]
        [lkl [if [null? l] 0 [gbk-length [xbrt-key l]]]]
        [rkl [if [null? r] 0 [gbk-length [xbrt-key r]]]]]
      [statrec [+ 1 [statrec-ncount ls][statrec-ncount rs]]
              [+ [inz v] [statrec-vcount ls][statrec-vcount rs]]
              [+ 1 [max [statrec-node-height ls][statrec-node-height rs]]]
              [max [+ lkl [statrec-key-height ls]]
                   [+ rkl [statrec-key-height rs]]]]]]

[define [xgen-def-accum v l r] 0]

[define [mk-xgenf vf sf af]
  [lambda [va l r x]
    [let [[v [if [null? va]
               [if [null? x] null [extv-val x]]
               [if [equal? va [void]] null [vf va x]]]]]
      [let [[s [sf v l r]]
            [a [af v l r]]]
        [xgen v s a]]]]]

[define xgen-def [mk-xgenf val-def xgen-def-stat xgen-def-accum]]
[define xgen-file [mk-xgenf val-file xgen-def-stat xgen-def-accum]]
[define xgen-sum [mk-xgenf val-sum xgen-def-stat xgen-def-accum]]

;;;*****

[define [mk-root f] [xbrt [rbk<-bin-char-str ""] [f null null null null] null null]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [qk t k] ;[values nki kl hmi hbl np]
  [qk-rec k [gbk-length k] 0 [xbrt-key t] [xbrt-left t][xbrt-right t][list t]]]
[define [qk-rec k kl ki hb hl hr np]
  [let* [[hmi [gbk-fd k ki hb 0]]
         [nki [+ ki hmi]]
         [hbl [gbk-length hb]]]
    [if [< hmi hbl]
      [values nki kl hmi hbl np]
      [if [< nki kl]
        [if [equal? [gbk-ref k nki] 0]
          [if [equal? hl null]
            [values nki kl hmi hbl np]
            [qk-rec k kl nki [xbrt-key hl][xbrt-left hl][xbrt-right hl][cons hl np]]]
          [if [equal? hr null]
            [values nki kl hmi hbl np]
            [qk-rec k kl nki [xbrt-key hr][xbrt-left hr][xbrt-right hr][cons hr np]]]]
        [values nki kl hmi hbl np]]]]]

[define [rec-up-bp node nbp xf]
  [if [equal? [gbk-length [xbrt-key node]] 0] node
    [let* [[h [car nbp]]
           [hb [xbrt-key h]][hext [xbrt-ext h]][hl [xbrt-left h]][hr [xbrt-right h]]]
      [rec-up-bp
        [if [equal? [gbk-ref [xbrt-key node] 0] 0]
          [xbrt hb [xf null node hr hext] node hr]
          [xbrt hb [xf null hl node hext] hl node]]
        [cdr nbp] xf]
      ]]]

[define [xset t k v [xf xf-def]]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [let [[h [car np]]]
      [let [[b [xbrt-key h]][x [xbrt-ext h]][l [xbrt-left h]][r [xbrt-right h]]]
          [if [equal? nki kl] 
            [if [equal? hmi hbl]
              [let [[node [xbrt b [xf v l r x] l r]]]
                [rec-up-bp node [cdr np] xf]]
              [let [[nh [xbrt [gbk-sub-key b hmi hbl] [xf null l r x] l r]]]
                [if [equal? [gbk-ref b hmi] 0]
                ;    [let [[node [xbrt [gbk-sub-key b 0 hmi] [xf v nh null null] nh null]]]
                    [let [[node [xbrt [gbk-sub-key k [- nki hmi] nki] [xf v nh null null] nh null]]]
                      [rec-up-bp node [cdr np] xf]]
                    [let [[node [xbrt [gbk-sub-key b 0 hmi] [xf v null nh null] null nh]]]
                      [rec-up-bp node [cdr np] xf]]]]]
            [if [equal? hmi hbl]
              [let [[nn [xbrt [gbk-sub-key k nki kl] [xf v null null null] null null]]]
                [if [equal? [gbk-ref k nki] 0]
                    [let [[node [xbrt b [xf null nn r x] nn r]]]
                      [rec-up-bp node [cdr np] xf]]
                    [let [[node [xbrt b [xf null l nn x] l nn]]]
                      [rec-up-bp node [cdr np] xf]]]]
              [let [[nh [xbrt [gbk-sub-key b hmi hbl] [xf null l r x] l r]]
                    [nn [xbrt [gbk-sub-key k nki kl] [xf v null null null] null null]]]
                [if [equal? [gbk-ref k nki] 0]
                    [let [[node [xbrt [gbk-sub-key b 0 hmi] [xf null nn nh null] nn nh]]]
                      [rec-up-bp node [cdr np] xf]]
                 ;   [let [[node [xbrt [gbk-sub-key b 0 hmi] [xf null nh nn null] nh nn]]]
                    [let [[node [xbrt [gbk-sub-key k [- nki hmi] nki] [xf null nh nn null] nh nn]]]
                      [rec-up-bp node [cdr np] xf]]] ]] ] ]]]]
     
[define [xget t k]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [if [null? np] null
      [let [[n [car np]]]
        [if [equal? hmi [gbk-length [xbrt-key n]]] [xbrt-ext n] null]]]]]

[define [xgetv t k]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [if [null? np] null
      [let [[n [car np]]]
        [if [and [equal? hmi [gbk-length [xbrt-key n]]][equal? nki kl]] [extv-val [xbrt-ext n]] null]]]]]

 
[define [rec-mend-bp b x l r nbp xf]
  [if [equal? [gbk-length b] 0] [let [[node [xbrt b [xf null l r x] l r]]] node]
    [let* [[h [car nbp]]
           [hb [xbrt-key h]][hext [xbrt-ext h]][hl [xbrt-left h]][hr [xbrt-right h]]
           [s [equal? 0 [gbk-ref b 0]]]]
      [if [null? l]
        [if [null? r]
          [if [null? [extv-val x]]
            [if s [rec-mend-bp hb [xf null null hr hext] null hr [cdr nbp] xf]
                  [rec-mend-bp hb [xf null hl null hext] hl null [cdr nbp] xf]]; mend p  000
            [rec-up-bp [xbrt b x l r] nbp xf]];mknode rec-up   010
          [if [null? [extv-val x]]
            [rec-mend-bp [gbk-cat b [xbrt-key r]] [xbrt-ext r] [xbrt-left r] [xbrt-right r] nbp xf];heal ch & mend p 001
            [rec-up-bp [xbrt b x l r] nbp xf]]];mknode rec-up  011
        [if [null? r]
          [if [null? [extv-val x]]
            [rec-mend-bp [gbk-cat b [xbrt-key l]] [xbrt-ext l] [xbrt-left l] [xbrt-right l] nbp xf];heal ch & mend p 100
            [rec-up-bp [xbrt b x l r] nbp xf]];mknode rec-up   110
          [if [null? [extv-val x]]
            [rec-up-bp [xbrt b x l r] nbp xf];mknode rec-up    101
            [rec-up-bp [xbrt b x l r] nbp xf]]]];mknode rec-up 111
      ]]]

[define [xdel t k [xf xf-def]] ;;needs testing
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [let [[h [car np]]]
      [let [[b [xbrt-key h]][x [xbrt-ext h]][l [xbrt-left h]][r [xbrt-right h]]]
        [if [< nki kl] t
          [if [< hmi hbl] t
            [rec-mend-bp b [xf [void] l r x] l r [cdr np] xf]]]]]]]
             

;[define [nset t k n [xf null]] [void]];[values t xv] this really just merge.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [fct-first t cf pxf]
  [if [cf t null]
    [values 0 null]
    [values 1 [list [cons t [pxf t null]]]]]]

[define [fct-next s bp cf pxf]
  [if [equal? 1 s]
    [let [[l [xbrt-left [caar bp]]]]
      [if [or [null? l] [cf l bp]]
        [values 2 bp]
        [values 1 [cons [cons l [pxf l bp]] bp]]]]
    [if [equal? 2 s]
      [let [[r [xbrt-right [caar bp]]]]
        [if [or [null? r] [cf r bp]]
          [values 3 bp]
          [values 1 [cons [cons r [pxf r bp]] bp]]]]
      [if [null? [cdr bp]]
        [values 0 null]
        [if [equal? 0 [xbrt-kfb? [caar bp]]]
          [values 2 [cdr bp]]
          [values 3 [cdr bp]]]]]]]

[define [fct-last t cf pxf]
  [if [cf t null]
    [values 0 null]
    [values 3 [list [cons t [pxf t null]]]]]]

[define [fct-prev s bp cf pxf]
  [if [equal? 3 s]
    [let [[r [xbrt-right [caar bp]]]]
      [if [or [null? r] [cf r bp]]
        [values 2 bp]
        [values 3 [cons [cons r [pxf r bp]] bp]]]]
    [if [equal? 2 s]
      [let [[l [xbrt-left [caar bp]]]]
        [if [or [null? l] [cf l bp]]
          [values 1 bp]
          [values 3 [cons [cons l [pxf l bp]] bp]]]]
      [if [null? [cdr bp]]
        [values 0 null]
        [if [equal? 0 [xbrt-kfb? [caar bp]]]
          [values 1 [cdr bp]]
          [values 2 [cdr bp]]]]]]]


[define [cf-def n bp] #f]
[define [pxf-def n bp] [if [null? bp] [xbrt-key n][gbk-cat [cdar bp][xbrt-key n]]]]
[define [op-def s bp]
 [list s [bin-char-str<-gbk [cdar bp]] [extv-val [xbrt-ext [caar bp]]]]]


[define [cf-tst n bp] [< 5 [+ [gbk-length [xbrt-key n]] [inz bp [lambda [n] [gbk-length [cdar n]]]]]]]
[define [op-tst s bp]
  [if [and [equal? s 2] [equal? 5 [gbk-length [cdar bp]]]]
    [list s [bin-char-str<-gbk [cdar bp]] [extv-val [xbrt-ext [caar bp]]]]
    [void]]]

[define [op-pre s bp] [if [equal? s 1] [extv-val [xbrt-ext [caar bp]]][void]]]

[define [strm-fct t cf pxf op]
  [stream-filter [lambda [x] [not [void? x]]]
    [stream-unfold
      [lambda [x] [op [car x][cdr x]]]
      [lambda [x] [< 0 [car x]]]
      [lambda [x] [call-with-values [lambda [] [fct-next [car x] [cdr x] cf pxf]] cons]]
      [call-with-values [lambda [] [fct-first t cf pxf]] cons]]]]


[define [preorder-init t cf pxf]
  [let-values [[[s bp] [fct-first t cf pxf]]]
    [if [null? [extv-val [xbrt-ext [caar bp]]]]
      [preorder-iter s bp cf pxf]
      [values s bp]]]]
[define [preorder-iter s bp cf pxf]
  [let-values [[[s bp] [fct-next s bp cf pxf]]]
    [if [or [and [equal? s 1] [not [null? [extv-val [xbrt-ext [caar bp]]]]]] [equal? s 0]]
      [values s bp]
      [preorder-iter s bp cf pxf]]]]

[define [strm-preorder t]
  [stream-unfold
    [lambda [x] [list [extv-val [xbrt-ext [caadr x]]]]]
    [lambda [x] [< 0 [car x]]]
    [lambda [x] [call-with-values [lambda [] [preorder-iter [car x] [cdr x] cf-def pxf-def]] cons]]
    [call-with-values [lambda [] [preorder-init t cf-def pxf-def]] cons]]]

[define [fmode-init m t cf pxf]
  [let-values [[[s bp] [fct-first t cf pxf]]]
    [if [null? [extv-val [xbrt-ext [caar bp]]]]
      [fmode-iter m s bp cf pxf]
      [values s bp]]]]
[define [fmode-iter m s bp cf pxf]
  [let-values [[[s bp] [fct-next s bp cf pxf]]]
    [if [or [and [equal? s m] [not [null? [extv-val [xbrt-ext [caar bp]]]]]] [equal? s 0]]
      [values s bp]
      [fmode-iter m s bp cf pxf]]]]

[define [strm-fmode m t]
  [stream-unfold
    [lambda [x] [list [extv-val [xbrt-ext [caadr x]]]]]
    [lambda [x] [< 0 [car x]]]
    [lambda [x] [call-with-values [lambda [] [fmode-iter m [car x] [cdr x] cf-def pxf-def]] cons]]
    [call-with-values [lambda [] [fmode-init m t cf-def pxf-def]] cons]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [xbrt=? x y]
  [let [[nx [if [null? x] [mk-root xf-def] x]]
        [ny [if [null? y] [mk-root xf-def] y]]]
    [and [bit-string-equal? [bit-string-pack [xbrt-key nx]] [bit-string-pack [xbrt-key ny]]]
         [equal? [xbrt-ext nx] [xbrt-ext ny]]
         [xbrt=? [xbrt-left nx] [xbrt-left ny]]
         [xbrt=? [xbrt-right nx] [xbrt-right ny]]]]]

[define [nmk-rbs min max]
  [substring [apply string-append [for/list [[k [in-range 0 [add1 [quotient max 16]]]]]
    [pad [random [expt 2 16]] 16 2]]] min [+ min [random [- [add1 max] min]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [unit-test n r]
  [let* [[kl [for/list [[j [in-range 0 [expt 2 n]]]] [nmk-rbs 0 r]]]
         [ckl [remove-duplicates kl]]
         [rkl [sort ckl [lambda [x y] [< 0.5 [random]]]]]
         [re [car rkl]]
         [drkl [cdr rkl]]
         [skl [sort rkl string<?]]
         [dskl [sort drkl string<?]]
         ]   
    [display "made keylist of length: "] [displayln [length rkl]]
    [display "tree build timing: "]
    [let* [[t [time [rec-set [mk-root xgen-def] rkl xgen-def]]]
          [dt [xdel t  [rbk<-bin-char-str re] xgen-def]]]
      [let [[res [stream->list [stream-map car [strm-preorder t]]]]
            [dres [stream->list [stream-map car [strm-preorder dt]]]]]
      [if [equal? res skl]  [displayln "passed xset test!"]
                            [displayln "FAILED xset test!"]]
      [if [equal? dres dskl]  [displayln "passed xdel test!"]
                              [displayln "FAILED xdel test!"]]
      [list t rkl]]]]]

[define [xbrt-test]
    [let [[mt [unit-test 12 64]]]
      ;[displayln "fct strm def:"]   
      ;[disp-stream [strm-fct [car mt] cf-def pxf-def op-def]]
      ;[disp-stream [strm-preorder [car mt]]]
      [displayln "fct strm tst:"]
      [disp-stream [strm-fct [car mt] cf-tst pxf-def op-tst]]
      [car mt]]]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [rec-set t l xf]
  [if [null? l] t
    [let [[e [car l]]]
      [rec-set [xset t [rbk<-bin-char-str e] e xf ] [cdr l] xf ]]]]

[define [rec-strm-set t s xf]
  [if [stream-null? s] t
    [let [[e [stream-car s]]]
      [rec-strm-set [xset t [rbk<-string e] [cons 1 e] xf] [stream-cdr s] xf]]]]


[define [stream-set kf vf xf s]
  [rec-stream-set [mk-root xf] kf vf xf s]]
[define [rec-stream-set t kf vf xf s]
  [if [stream-null? s] t
    [let [[e [stream-car s]]]
      [rec-stream-set [xset t [kf e] [vf e] xf] kf vf xf [stream-cdr s]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

