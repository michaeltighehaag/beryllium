#lang racket
#|
A functional and eXtensible Binary Radix Tree implementation.
|#

#|
todo
refactor gbk-sub-key use to account for streams
|#

[require racket/require
  [path-up "common.rkt"]]
[require racket/require
  [path-up "stream.rkt"]]
[require racket/generic]

[provide [all-defined-out]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbrt [key val gnx left right] #:transparent]
[define [xbrt-kfb? n][gbk-ref [xbrt-key n] 0]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define-generics gbk
  [gbk-length gbk]
  [gbk-ref gbk n]
  [gbk-cat gbk rb]
  [bin-char-str<-gbk gbk]
  [gbk-sub-key gbk s e]
  [gbk-fd gbk s1 bs2 s2]
]

[define-generics xbrt_gnx
  [mk_xbrt_xf xbrt_gnx]]

[define-generics gxbrt
  [xbrt-set gxbrt k v]
  [xbrt-nset gxbrt n]
  [xbrt-get gxbrt k]
  [xbrt-xget gxbrt k]
  [xbrt-nget gxbrt k]
  [xbrt-del gxbrt k]
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  [if [equal? [gbk-length [xbrt-key node]] 0];[null? nbp]
      node
    [let* [[h [car nbp]]
           [hb [xbrt-key h]] [hv [xbrt-val h]] 
           [hext [xbrt-gnx h]][hl [xbrt-left h]][hr [xbrt-right h]]]
      [rec_up-bp
        [if [equal? [gbk-ref [xbrt-key node] 0] 0]
          [mkx hb hv [xf hv node hr] node hr]
          [mkx hb hv [xf hv hl node] hl node]]
        [cdr nbp] xf mkx]
      ]]]

[define [x_set t k v xf mkx]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [let [[h [car np]]]
      [let [[b [xbrt-key h]] [hv [xbrt-val h]] [x [xbrt-gnx h]][l [xbrt-left h]][r [xbrt-right h]]]
          [if [equal? nki kl] 
            [if [equal? hmi hbl]
              [let* [[mnvc v][node [mkx b mnvc [xf mnvc l r] l r]]]
                [rec_up-bp node [cdr np] xf mkx]]
              [let [[nh [mkx [gbk-sub-key b hmi hbl] hv [xf hv l r] l r]]]
                [if [equal? [gbk-ref b hmi] 0]
                    [let [[node [mkx [gbk-sub-key k [- nki hmi] nki] v [xf v nh null] nh null]]]
                      [rec_up-bp node [cdr np] xf mkx]]
                    [let [[node [mkx [gbk-sub-key b 0 hmi] v [xf v null nh] null nh]]]
                      [rec_up-bp node [cdr np] xf mkx]]]]]
            [if [equal? hmi hbl]
              [let [[nn [mkx [gbk-sub-key k nki kl] v [xf v null null] null null]]]
                [if [equal? [gbk-ref k nki] 0]
                    [let [[node [mkx b hv [xf hv nn r] nn r]]]
                      [rec_up-bp node [cdr np] xf mkx]]
                    [let [[node [mkx b hv [xf hv l nn] l nn]]]
                      [rec_up-bp node [cdr np] xf mkx]]]]
              [let [[nh [mkx [gbk-sub-key b hmi hbl] hv [xf hv l r] l r]]
                    [nn [mkx [gbk-sub-key k nki kl] v [xf v null null] null null]]]
                [if [equal? [gbk-ref k nki] 0]
                    [let [[node [mkx [gbk-sub-key b 0 hmi] null [xf null nn nh] nn nh]]]
                      [rec_up-bp node [cdr np] xf mkx]]
                    [let [[node [mkx [gbk-sub-key k [- nki hmi] nki] null [xf null nh nn] nh nn]]]
                      [rec_up-bp node [cdr np] xf mkx]]] ]] ] ]]]]

[define [x_nget t k d]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [if [null? np] d
      [let [[n [car np]]]
        [if [and [equal? hmi [gbk-length [xbrt-key n]]][equal? nki kl]] n d]]]]]
    
[define [rec_mend-bp b mnvc x l r nbp xf mkx]
  [if [equal? [gbk-length b] 0] ;[null? nbp]
    [let [[node [mkx b mnvc [xf mnvc l r] l r]]] node]
    [let* [[h [car nbp]]
           [hb [xbrt-key h]][hv [xbrt-val h]][hext [xbrt-gnx h]][hl [xbrt-left h]][hr [xbrt-right h]]
           [s [equal? 0 [gbk-ref b 0]]]]
      [if [null? l]
        [if [null? r]
          [if [null? mnvc ]
            [if s [rec_mend-bp hb hv [xf hv null hr] null hr [cdr nbp] xf mkx]
                  [rec_mend-bp hb hv [xf hv hl null] hl null [cdr nbp] xf mkx]]; mend p 000
            [rec_up-bp [mkx b mnvc x l r] nbp xf mkx]];mknode rec-up   010
          [if [null? mnvc]
            [rec_mend-bp [gbk-cat b [xbrt-key r]]
                         [xbrt-val r] [xbrt-gnx r] [xbrt-left r] [xbrt-right r] nbp xf mkx];heal ch & mend p 001
            [rec_up-bp [mkx b mnvc x l r] nbp xf mkx]]];mknode rec-up  011
        [if [null? r]
          [if [null? mnvc]
            [rec_mend-bp [gbk-cat b [xbrt-key l]]
                         [xbrt-val l] [xbrt-gnx l] [xbrt-left l] [xbrt-right l] nbp xf mkx];heal ch & mend p 100
            [rec_up-bp [mkx b mnvc x l r] nbp xf mkx]];mknode rec-up   110
          [if [null? mnvc]
            [rec_up-bp [mkx b mnvc x l r] nbp xf mkx];mknode rec-up    101
            [rec_up-bp [mkx b mnvc x l r] nbp xf mkx]]]];mknode rec-up 111
      ]]]

[define [x_del t k xf mkx] ;;needs testing
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [let [[h [car np]]]
      [let [[b [xbrt-key h]][x [xbrt-gnx h]][l [xbrt-left h]][r [xbrt-right h]]]
        [if [< nki kl] t
          [if [< hmi hbl] t
            [rec_mend-bp b null [xf null l r] l r [cdr np] xf mkx]]]]]]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbpz [back node bpx] #:transparent]

[struct tz [head state tx] #:transparent]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define-generics gnx
  [nx-set gnx nn]
]

[define-generics gzx
  [tx-set gzx nh nstate]
]

[define-generics gtz
  [gtz-iter gtz]
  [gtz-cdp gtz]
  [gtz-vtf gtz]
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [xbrz-next z cf]
  [let [[h [tz-head z]]
        [s [tz-state z]]
        [x [tz-tx z]]]
    [if [equal? 'pre s]
      [let [[l [xbrt-left [xbpz-node h]]]]
        [if [or [null? l] [cf l z]]
          [tz h 'int [tx-set x h 'int]]
          [let [[nh [xbpz h l [nx-set [xbpz-bpx h] l]]]] [tz nh 'pre [tx-set x nh 'pre]]]]] 
      [if [equal? 'int s]
        [let [[r [xbrt-right [xbpz-node h]]]] 
          [if [or [null? r] [cf r z]]
            [tz h 'pst [tx-set x h 'pst]]
            [let [[nh [xbpz h r [nx-set [xbpz-bpx h] r]]]] [tz nh 'pre [tx-set x nh 'pre]]]]]    
        [let [[pnh [xbpz-back [tz-head z]]]]
          [if [null? pnh]
            null
            [if [equal? 0 [xbrt-kfb? [xbpz-node h]]]
              [tz pnh 'int [tx-set x pnh 'int]] 
              [tz pnh 'pst [tx-set x pnh 'pst]]]]]]]]] 


[define [xbrz-prev z cf]
  [let [[h [tz-head z]]
        [s [tz-state z]]
        [x [tz-tx z]]]
    [if [equal? 'pst s]
      [let [[r [xbrt-right [xbpz-node h]]]]
        [if [or [null? r] [cf r z]]
          [tz h 'int [tx-set x h 'int]]
          [let [[nh [xbpz h r [nx-set [xbpz-bpx h] r]]]] [tz nh 'pst [tx-set x nh 'pst]]]]] 
      [if [equal? 'int s]
        [let [[l [xbrt-left [xbpz-node h]]]] 
          [if [or [null? l] [cf l z]]
            [tz h 'pre [tx-set x h 'pre]]
            [let [[nh [xbpz h l [nx-set [xbpz-bpx h] l]]]] [tz nh 'pst [tx-set x nh 'pst]]]]]    
        [let [[pnh [xbpz-back [tz-head z]]]]
          [if [null? pnh]
            null
            [if [equal? 1 [xbrt-kfb? [xbpz-node h]]]
              [tz pnh 'int [tx-set x pnh 'int]] 
              [tz pnh 'pre [tx-set x pnh 'pre]]]]]]]]] 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


[define [stream<-xbrz tz]
  [let [[xbrz-iter [gtz-iter tz]][tdp [gtz-cdp tz]][vtf [gtz-vtf tz]]]
  [stream-filter [lambda [x] [not [void? x]]]
    [stream-unfold
      [lambda [x] [vtf x]]
      [lambda [x] [not [null? x]]]
      [lambda [x] [xbrz-iter x tdp]]
      tz]]]]

[define [rec-set t l]
  [if [null? l] t
    [let [[e [car l]]]
      [rec-set [xbrt-set t e [cat "" e]] [cdr l]]]]]

[define [xbrt<-stream t s]
  [if [stream-null? s] t
    [let [[e [stream-car s]]]
      [xbrt<-stream [xbrt-set t [car e] [cdr e]] [stream-cdr s]]]]]


