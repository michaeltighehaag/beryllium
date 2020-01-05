#lang racket
#|
A functional and eXtensible Binary Radix Tree implementation.
|#

#|
todo
refactor gbk-sub-key use to account for streams
simplify x_gen default
reexamine/refactor traversal with descent predicate to account for other use cases
|#

[require racket/require
  [path-up "common.rkt"]]
[require racket/require
  [path-up "stream.rkt"]]
[require bitsyntax]
[require racket/generic]

[provide [all-defined-out]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define-generics gbk
  [gbk-length gbk]
  [gbk-ref gbk n]
  [gbk-cat gbk rb]
  [bin-char-str<-gbk gbk]
  [gbk-sub-key gbk s e]
  [gbk-fd gbk s1 bs2 s2]
]

;;;;;;;;; RBK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct rbk [bits frst last] #:transparent
  #:methods gen:gbk
  [[define [gbk-length k]
     [+ [rbk-last k] [- [rbk-frst k]]]]
   [define [gbk-ref k n]
     [bit-string-ref [rbk-bits k] [+ [rbk-frst k] n]]]
   [define [gbk-cat lb rb] [rbk [rbk-bits rb] [rbk-frst lb] [rbk-last rb]]]
   [define [bin-char-str<-gbk k]
     [let [[l [gbk-length k]]]
       [if [equal? l 0] ""
         [pad [bit-string->unsigned-integer [sub-bit-string [rbk-bits k] [rbk-frst k][rbk-last k]] #t] l 2]]]]
   [define [gbk-sub-key k s e] [rbk [rbk-bits k] [+ s [rbk-frst k]] [+ e [rbk-frst k]]]]
   [define [gbk-fd bs1 s1 bs2 s2]
     [if [and [< s1 [gbk-length bs1]] [< s2 [gbk-length bs2]]]
       [if [equal? [gbk-ref bs1 s1] [gbk-ref bs2 s2]]
         [add1 [gbk-fd bs1 [add1 s1] bs2 [add1 s2]]] 0 ] 0]]
  ]]
[define [rbk<-int v s] [rbk [integer->bit-string v s #t] 0 s]]
[define [rbk<-bin-char-str s]
  [let [[sl [string-length s]]]
    [rbk<-int [if [equal? sl 0] 0 [string->number s 2]] sl]]]
[define [string<-rbk k]
  [bytes->string/utf-8 [bit-string->bytes [rbk-bits k]]]]
[define [rbk<-string str]
  [let* [[bs [string->bytes/utf-8 str]]
         [bits [bit-string [bs :: binary]]]]
   [rbk bits 0 [bit-string-length bits]]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;; FBK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct fbk [bit-str] #:transparent
  #:methods gen:gbk
  [[define [gbk-length k]
     [bit-string-length k]]
   [define [gbk-ref k n]
     [bit-string-ref k n]]
   [define [gbk-cat lb rb] [bit-string-pack [bit-string-append lb rb]]] 
   [define [bin-char-str<-gbk k]
     [let [[l [gbk-length k]]]
       [if [equal? l 0] ""
         [pad [bit-string->unsigned-integer k #t] l 2]]]]
   [define [gbk-sub-key k s e] [sub-bit-string k s e]] 
   [define [gbk-fd bs1 s1 bs2 s2]
     [if [and [< s1 [gbk-length bs1]] [< s2 [gbk-length bs2]]]
       [if [equal? [gbk-ref bs1 s1] [gbk-ref bs2 s2]]
         [add1 [gbk-fd bs1 [add1 s1] bs2 [add1 s2]]] 0 ] 0]]
  ]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[struct xbrt [key ext left right] #:transparent]

[define-generics x_ext
  [xxf x_ext]]

[struct xvc [val] #:transparent
  #:methods gen:x_ext
  [[define [xxf s] [lambda [p] [xvc p]]]]]

[struct x_key_def  [key_max_height key_min_height] #:transparent]
[define [kx-f p l r]
  [x_key_def [max [if [null? l] 0 [+ [gbk-length [xbrt-key l]]
                                     [x_key_def-key_max_height [x_def-key_x [xbrt-ext l]]]]]
                  [if [null? r] 0 [+ [gbk-length [xbrt-key r]]
                                     [x_key_def-key_max_height [x_def-key_x [xbrt-ext r]]]]]]
             [min [if [null? l] 0 [+ [gbk-length [xbrt-key l]]
                                     [x_key_def-key_min_height [x_def-key_x [xbrt-ext l]]]]]
                  [if [null? r] 0 [+ [gbk-length [xbrt-key r]]
                                     [x_key_def-key_min_height [x_def-key_x [xbrt-ext r]]]]]]]]

[struct x_val_def  [val_count val_cum] #:transparent]
[define [vx-f p l r]
  [x_val_def [+ [if [null? p] 0 1]
              [if [null? l] 0 [x_val_def-val_count [x_def-val_x [xbrt-ext l]]]]
              [if [null? r] 0 [x_val_def-val_count [x_def-val_x [xbrt-ext r]]]]]
             0]]

[struct x_node_def [node_count node_max_height node_min_height] #:transparent]
[define [nx-f p l r]
  [x_node_def [+ 1 [if [null? l] 0 [x_node_def-node_count [x_def-node_x [xbrt-ext l]]]]
                   [if [null? r] 0 [x_node_def-node_count [x_def-node_x [xbrt-ext r]]]]]
              [+ 1 [max [if [null? l] 0 [x_node_def-node_max_height [x_def-node_x [xbrt-ext l]]]]
                        [if [null? r] 0 [x_node_def-node_max_height [x_def-node_x [xbrt-ext r]]]]]]
              [+ 1 [min [if [null? l] 0 [x_node_def-node_min_height [x_def-node_x [xbrt-ext l]]]]
                        [if [null? r] 0 [x_node_def-node_min_height [x_def-node_x [xbrt-ext r]]]]]]
              ]]

[struct x_def xvc [key_x val_x node_x] #:transparent
  #:methods gen:x_ext
  [[define [xxf s]
     [lambda [p l r] 
       [let [[kx [kx-f p l r]]
             [vx [vx-f p l r]]
             [nx [nx-f p l r]]]
         [x_def p kx vx nx]]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;    define generic xbrt functions   ;;;;;;;;;;;;;;

[define-generics gxbrt
  [xbrt-mk  gxbrt]
  [xbrt-set gxbrt k v]
  [xbrt-get-ext gxbrt k]
  [xbrt-get gxbrt k]
  [xbrt-del gxbrt k]
]

[define [val-def v x] v]
;;;;;;;;; default xbrt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbrt_def xbrt [] #:transparent
  #:methods gen:gxbrt
  [[define [xbrt-mk t] [lambda [k x l r] [xbrt_def k x l r]]]
   [define [xbrt-set t k v]
     [x_set t [rbk<-bin-char-str k] [cons k v] [xxf [xbrt-ext t]] val-def [xbrt-mk t]]]
   [define [xbrt-get-ext t k]
     [x_get-ext t [rbk<-bin-char-str k] null]]
   [define [xbrt-get t k]
     [xvc-val [x_get t [rbk<-bin-char-str k] null]]]
   [define [xbrt-del t k]
     [x_del t [rbk<-bin-char-str k] [xxf [xbrt-ext t]] [xbrt-mk t]]]
]]

[define xbrt_def_root
  [xbrt_def [rbk<-bin-char-str ""]
              [x_def null [kx-f null null null] [vx-f null null null] [nx-f null null null]]
              null
              null]]

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

[define [rec_up-bp node nbp xf mkx]
  [if [equal? [gbk-length [xbrt-key node]] 0] node
    [let* [[h [car nbp]]
           [hb [xbrt-key h]][hext [xbrt-ext h]][hl [xbrt-left h]][hr [xbrt-right h]]]
      [rec_up-bp
        [if [equal? [gbk-ref [xbrt-key node] 0] 0]
          [mkx hb [xf [xvc-val hext] node hr] node hr]
          [mkx hb [xf [xvc-val hext] hl node] hl node]]
        [cdr nbp] xf mkx]
      ]]]

[define [x_set t k v xf vf mkx]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [let [[h [car np]]]
      [let [[b [xbrt-key h]][x [xbrt-ext h]][l [xbrt-left h]][r [xbrt-right h]]]
          [if [equal? nki kl] 
            [if [equal? hmi hbl]
              [let [[node [mkx b [xf [vf v x] l r] l r]]]
                [rec_up-bp node [cdr np] xf mkx]]
              [let [[nh [mkx [gbk-sub-key b hmi hbl] [xf [xvc-val x] l r] l r]]]
                [if [equal? [gbk-ref b hmi] 0]
                    [let [[node [mkx [gbk-sub-key k [- nki hmi] nki] [xf v nh null] nh null]]]
                      [rec_up-bp node [cdr np] xf mkx]]
                    [let [[node [mkx [gbk-sub-key b 0 hmi] [xf v null nh] null nh]]]
                      [rec_up-bp node [cdr np] xf mkx]]]]]
            [if [equal? hmi hbl]
              [let [[nn [mkx [gbk-sub-key k nki kl] [xf v null null] null null]]]
                [if [equal? [gbk-ref k nki] 0]
                    [let [[node [mkx b [xf [xvc-val x] nn r] nn r]]]
                      [rec_up-bp node [cdr np] xf mkx]]
                    [let [[node [mkx b [xf [xvc-val x] l nn] l nn]]]
                      [rec_up-bp node [cdr np] xf mkx]]]]
              [let [[nh [mkx [gbk-sub-key b hmi hbl] [xf [xvc-val x] l r] l r]]
                    [nn [mkx [gbk-sub-key k nki kl] [xf v null null] null null]]]
                [if [equal? [gbk-ref k nki] 0]
                    [let [[node [mkx [gbk-sub-key b 0 hmi] [xf null nn nh] nn nh]]]
                      [rec_up-bp node [cdr np] xf mkx]]
                    [let [[node [mkx [gbk-sub-key k [- nki hmi] nki] [xf null nh nn] nh nn]]]
                      [rec_up-bp node [cdr np] xf mkx]]] ]] ] ]]]]

[define [x_get t k d]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [if [null? np] d
      [let [[n [car np]]]
        [if [and [equal? hmi [gbk-length [xbrt-key n]]][equal? nki kl]] [xvc-val [xbrt-ext n]] d]]]]]

[define [x_get-ext t k d]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [if [null? np] d
      [let [[n [car np]]]
        [if [and [equal? hmi [gbk-length [xbrt-key n]]][equal? nki kl]] [xbrt-ext n] d]]]]]
  
  
[define [rec_mend-bp b x l r nbp xf mkx]
  [if [equal? [gbk-length b] 0] [let [[node [mkx b [xf [xvc-val x] l r] l r]]] node]
    [let* [[h [car nbp]]
           [hb [xbrt-key h]][hext [xbrt-ext h]][hl [xbrt-left h]][hr [xbrt-right h]]
           [s [equal? 0 [gbk-ref b 0]]]]
      [if [null? l]
        [if [null? r]
          [if [null? [xvc-val x]]
            [if s [rec_mend-bp hb [xf [xvc-val hext] null hr] null hr [cdr nbp] xf mkx]
                  [rec_mend-bp hb [xf [xvc-val hext] hl null] hl null [cdr nbp] xf mkx]]; mend p 000
            [rec_up-bp [mkx b x l r] nbp xf mkx]];mknode rec-up   010
          [if [null? [xvc-val x]]
            [rec_mend-bp [gbk-cat b [xbrt-key r]] [xbrt-ext r] [xbrt-left r] [xbrt-right r] nbp xf mkx];heal ch & mend p 001
            [rec_up-bp [mkx b x l r] nbp xf mkx]]];mknode rec-up  011
        [if [null? r]
          [if [null? [xvc-val x]]
            [rec_mend-bp [gbk-cat b [xbrt-key l]] [xbrt-ext l] [xbrt-left l] [xbrt-right l] nbp xf mkx];heal ch & mend p 100
            [rec_up-bp [mkx b x l r] nbp xf mkx]];mknode rec-up   110
          [if [null? [xvc-val x]]
            [rec_up-bp [mkx b x l r] nbp xf mkx];mknode rec-up    101
            [rec_up-bp [mkx b x l r] nbp xf mkx]]]];mknode rec-up 111
      ]]]



[define [x_del t k xf mkx] ;;needs testing
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [let [[h [car np]]]
      [let [[b [xbrt-key h]][x [xbrt-ext h]][l [xbrt-left h]][r [xbrt-right h]]]
        [if [< nki kl] t
          [if [< hmi hbl] t
            [rec_mend-bp b [xf null l r] l r [cdr np] xf mkx]]]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [xbrt-kfb? n][gbk-ref [xbrt-key n] 0]]


[define [fct-first t cf pxf]
  [if [cf t null]
    [cons 0 null]
    [cons 1 [list [cons t [pxf t null]]]]]]

[define [fct-next s bp cf pxf]
  [if [equal? 1 s]
    [let [[l [xbrt-left [caar bp]]]]
      [if [or [null? l] [cf l bp]]
        [cons 2 bp]
        [cons 1 [cons [cons l [pxf l bp]] bp]]]]
    [if [equal? 2 s]
      [let [[r [xbrt-right [caar bp]]]]
        [if [or [null? r] [cf r bp]]
          [cons 3 bp]
          [cons 1 [cons [cons r [pxf r bp]] bp]]]]
      [if [null? [cdr bp]]
        [cons 0 null]
        [if [equal? 0 [xbrt-kfb? [caar bp]]]
          [cons 2 [cdr bp]]
          [cons 3 [cdr bp]]]]]]]

[define [fct-last t cf pxf]
  [if [cf t null]
    [cons 0 null]
    [cons 3 [list [cons t [pxf t null]]]]]]

[define [fct-prev s bp cf pxf]
  [if [equal? 3 s]
    [let [[r [xbrt-right [caar bp]]]]
      [if [or [null? r] [cf r bp]]
        [cons 2 bp]
        [cons 3 [cons [cons r [pxf r bp]] bp]]]]
    [if [equal? 2 s]
      [let [[l [xbrt-left [caar bp]]]]
        [if [or [null? l] [cf l bp]]
          [cons 1 bp]
          [cons 3 [cons [cons l [pxf l bp]] bp]]]]
      [if [null? [cdr bp]]
        [cons 0 null]
        [if [equal? 0 [xbrt-kfb? [caar bp]]]
          [cons 1 [cdr bp]]
          [cons 2 [cdr bp]]]]]]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [tdp-def n bp] #f]
[define [tpf-def n bp] [if [null? bp] [xbrt-key n][gbk-cat [cdar bp][xbrt-key n]]]]
[define [vtf-def s bp] [list s [bin-char-str<-gbk [cdar bp]] [xvc-val [xbrt-ext [caar bp]]]]]

[define [tdp-tst n bp] [< 5 [+ [gbk-length [xbrt-key n]] [if [null? bp] 0 [gbk-length [cdar bp]]]]]]

[define [vtf-tst s bp]
  [if [and [equal? s 2] [equal? 5 [gbk-length [cdar bp]]]]
    [list s [bin-char-str<-gbk [cdar bp]] [xbrt-ext [caar bp]]]
    [void]]]
  
[define [pre-val s bp]
  [if [equal? s 1]
    [let [[v [xvc-val [xbrt-ext [caar bp]]]]]
      [if [null? v] [void] [list [cdr v]]]]
    [void]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [stream<-xbrt t #:tdp [tdp tdp-def] #:tpf [tpf tpf-def] #:vtf [vtf vtf-def]]
  [stream-filter [lambda [x] [not [void? x]]]
    [stream-unfold
      [lambda [x] [vtf [car x][cdr x]]]
      [lambda [x] [< 0 [car x]]]
      [lambda [x] [fct-next [car x] [cdr x] tdp tpf]]
      [fct-first t tdp tpf]]]]
[define [stream<-xbrt-rev t #:tdp [tdp tdp-def] #:tpf [tpf tpf-def] #:vtf [vtf vtf-def]]
  [stream-filter [lambda [x] [not [void? x]]]
    [stream-unfold
      [lambda [x] [vtf [car x][cdr x]]]
      [lambda [x] [< 0 [car x]]]
      [lambda [x] [fct-prev [car x] [cdr x] tdp tpf]]
      [fct-last t tdp tpf]]]]

[define [rec-set t l]
  [if [null? l] t
    [let [[e [car l]]]
      [rec-set [xbrt-set t e [cat "" e]] [cdr l]]]]]


[define [xbrt<-stream kf vf xf s]
  [xbrt<-stream-rec xbrt_def_root kf vf xf s]]
[define [xbrt<-stream-rec t kf vf xf s]
  [if [stream-null? s] t
    [let [[e [stream-car s]]]
      [xbrt<-stream-rec [xbrt-set t [kf e] [vf e]] kf vf [stream-cdr s]]]]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





















[struct xbrz [back node zx] #:transparent]

[struct tz [head state tx] #:transparent]


[define [xbrz-first t cf pxf]
  [tz [xbrz null t null] 'pre null]]

[define [xbrz-next z cf pxf]
  [let [[h [tz-head z]][s [tz-state z]]]
    [if [equal? 'pre s]
      [let [[l [xbrt-left [xbrz-node h]]]]
        [if [or [null? l] [cf l z]]
          [tz h 'int null]
          [tz [xbrz h l null] 'pre null]]]
      [if [equal? 'int s]
        [let [[r [xbrt-right [xbrz-node h]]]] 
          [if [or [null? r] [cf r z]]
            [tz h 'pst null]
            [tz [xbrz h r null] 'pre null]]]        
        [if [null? [xbrz-back [tz-head z]]]
          [begin [displayln "reached?"] null]
          [if [equal? 0 [xbrt-kfb? [xbrz-node h]]]
            [tz [xbrz-back [tz-head z]] 'int null] 
            [tz [xbrz-back [tz-head z]] 'pst null]]]]]]] 

[define [xbrz-last t cf pxf]
  [tz [xbrz null t null] 'pst null]]
[define [xbrz-prev z cf pxf]
  [let [[h [tz-head z]][s [tz-state z]]]
    [if [equal? 'pst s]
      [let [[r [xbrt-right [xbrz-node h]]]]
        [if [or [null? r] [cf r z]]
          [tz h 'int null]
          [tz [xbrz h r null] 'pst null]]]
      [if [equal? 'int s]
        [let [[l [xbrt-left [xbrz-node h]]]] 
          [if [or [null? l] [cf l z]]
            [tz h 'pre null]
            [tz [xbrz h l null] 'pst null]]]        
        [if [null? [xbrz-back [tz-head z]]]
          [begin [displayln "reached?"] null]
          [if [equal? 1 [xbrt-kfb? [xbrz-node h]]]
            [tz [xbrz-back [tz-head z]] 'int null] 
            [tz [xbrz-back [tz-head z]] 'pre null]]]]]]] 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [xbrz-tdp-def n z] #f]
[define [xbrz-tpf-def n bp] [if [null? bp] [xbrt-key n][gbk-cat [cdar bp][xbrt-key n]]]]
[define [xbrz-vtf-def z] [list [tz-state z] [xvc-val [xbrt-ext [xbrz-node [tz-head z]]]]]];[list s [bin-char-str<-gbk [cdar bp]] [xvc-val [xbrt-ext [caar bp]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [stream<-xbrtz t #:tdp [tdp xbrz-tdp-def] #:tpf [tpf xbrz-tpf-def] #:vtf [vtf xbrz-vtf-def]]
  [stream-filter [lambda [x] [not [void? x]]]
    [stream-unfold
      [lambda [x] [vtf x]]
      [lambda [x] [not [null? x]]]
      [lambda [x] [xbrz-next x tdp tpf]]
      [xbrz-first t tdp tpf]]]]
[define [stream<-xbrtz-rev t #:tdp [tdp xbrz-tdp-def] #:tpf [tpf xbrz-tpf-def] #:vtf [vtf xbrz-vtf-def]]
  [stream-filter [lambda [x] [not [void? x]]]
    [stream-unfold
      [lambda [x] [vtf x]]
      [lambda [x] [not [null? x]]]
      [lambda [x] [xbrz-prev x tdp tpf]]
      [xbrz-last t tdp tpf]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;























;;;;;;;;;;;;;;;;; deque ;;;;;;;;;;;;;;;;;;;;
[define [mk-deq] void]
[define [deq-head-push d] void]
[define [deq-head-pop  d] void]
[define [deq-tail-push d] void]
[define [deq-tail-pop  d] void]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [nmk-rbs min max]
  [substring [apply string-append [for/list [[k [in-range 0 [add1 [quotient max 16]]]]]
    [pad [random [expt 2 16]] 16 2]]] min [+ min [random [- [add1 max] min]]]]]

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
    [let* [[t [time [rec-set xbrt_def_root rkl]]]
          [dt [xbrt-del t re]]]
      [let [[res [stream->list [stream-map car [stream<-xbrt t #:vtf pre-val]]]]
            [dres [stream->list [stream-map car [stream<-xbrt dt #:vtf pre-val]]]]]
        [displayln [take res 10]]
        [displayln [take skl 10]]
      [if [equal? res skl]  [displayln "passed xset test!"]
                            [displayln "FAILED xset test!"]]
      [if [equal? dres dskl]  [displayln "passed xdel test!"]
                              [displayln "FAILED xdel test!"]]
      [list t dt rkl]]]]]

[define [xbrt-test]
    [let [[mt [unit-test 12 64]]]
      [displayln "fct strm def:"]   
      [disp-stream [stream-take 20 [stream<-xbrt [car mt] ]]]
      [disp-stream [stream-take 20 [stream<-xbrt [car mt] #:vtf pre-val]]]
      [displayln "fct strm tst:"]
      [disp-stream [stream<-xbrt [car mt] #:tdp tdp-tst #:vtf vtf-tst]]
      [displayln [xbrt-get-ext [car mt] ""]]
      [displayln [xbrt-get-ext [cadr mt] ""]]

      [disp-stream [stream-take 200 [stream<-xbrtz [car mt]]]]
      [disp-stream [stream-take 200 [stream<-xbrtz-rev [car mt]]]]
      ]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
