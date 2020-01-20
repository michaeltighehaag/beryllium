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
         [pad [bit-string->unsigned-integer
               [sub-bit-string [rbk-bits k] [rbk-frst k][rbk-last k]] #t] l 2]]]]
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

[struct xbrt [key val gnx left right] #:transparent]
[define [xbrt-kfb? n][gbk-ref [xbrt-key n] 0]]

[define-generics xbrt_gnx
  [xxf xbrt_gnx]]

[struct gnx_null [] #:transparent
  #:methods gen:xbrt_gnx
  [[define [xxf s] [lambda [p l r] [gnx_null]]]]]

[struct x_key_def  [key_max_height key_min_height] #:transparent]
[define [kx-f p l r]
  [x_key_def [max [if [null? l] 0 [+ [gbk-length [xbrt-key l]]
                                     [x_key_def-key_max_height [gnx_def-key_x [xbrt-gnx l]]]]]
                  [if [null? r] 0 [+ [gbk-length [xbrt-key r]]
                                     [x_key_def-key_max_height [gnx_def-key_x [xbrt-gnx r]]]]]]
             [min [if [null? l] 0 [+ [gbk-length [xbrt-key l]]
                                     [x_key_def-key_min_height [gnx_def-key_x [xbrt-gnx l]]]]]
                  [if [null? r] 0 [+ [gbk-length [xbrt-key r]]
                                     [x_key_def-key_min_height [gnx_def-key_x [xbrt-gnx r]]]]]]]]

[struct x_val_def  [val_count val_cum] #:transparent]
[define [vx-f p l r]
  [x_val_def [+ [if [null? p] 0 1]
              [if [null? l] 0 [x_val_def-val_count [gnx_def-val_x [xbrt-gnx l]]]]
              [if [null? r] 0 [x_val_def-val_count [gnx_def-val_x [xbrt-gnx r]]]]]
             0]]

[struct x_node_def [node_count node_max_height node_min_height] #:transparent]
[define [nx-f p l r]
  [x_node_def [+ 1 [if [null? l] 0 [x_node_def-node_count [gnx_def-node_x [xbrt-gnx l]]]]
                   [if [null? r] 0 [x_node_def-node_count [gnx_def-node_x [xbrt-gnx r]]]]]
              [+ 1 [max [if [null? l] 0 [x_node_def-node_max_height [gnx_def-node_x [xbrt-gnx l]]]]
                        [if [null? r] 0 [x_node_def-node_max_height [gnx_def-node_x [xbrt-gnx r]]]]]]
              [+ 1 [min [if [null? l] 0 [x_node_def-node_min_height [gnx_def-node_x [xbrt-gnx l]]]]
                        [if [null? r] 0 [x_node_def-node_min_height [gnx_def-node_x [xbrt-gnx r]]]]]]
              ]]

[struct gnx_def [key_x val_x node_x] #:transparent
  #:methods gen:xbrt_gnx
  [[define [xxf s]
     [lambda [p l r] 
       [let [[kx [kx-f p l r]]
             [vx [vx-f p l r]]
             [nx [nx-f p l r]]]
         [gnx_def  kx vx nx]]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;    define generic xbrt functions   ;;;;;;;;;;;;;;

[define-generics gxbrt
  [xbrt-mk  gxbrt]
  [xbrt-set gxbrt k v]
  [xbrt-get-gnx gxbrt k]
  [xbrt-get gxbrt k]
  [xbrt-del gxbrt k]
]

[define [val-def v x] v]
;;;;;;;;; default xbrt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbrt_def xbrt [] #:transparent
  #:methods gen:gxbrt
  [[define [xbrt-mk t] [lambda [k v x l r] [xbrt_def k v x l r]]]
   [define [xbrt-set t k v]
     [x_set t [rbk<-bin-char-str k] [cons k v] [xxf [xbrt-gnx t]] val-def [xbrt-mk t]]]
   [define [xbrt-get-gnx t k]
     [x_get-gnx t [rbk<-bin-char-str k] null]]
   [define [xbrt-get t k]
      [x_get t [rbk<-bin-char-str k] null]];]
   [define [xbrt-del t k]
     [x_del t [rbk<-bin-char-str k] [xxf [xbrt-gnx t]] [xbrt-mk t]]]
]]

[define xbrt_def_root
  [let* [[b [rbk<-bin-char-str ""]]
         [xk [kx-f null null null]]
         [xv [vx-f null null null]]
         [xn [nx-f null null null]]
         [x [gnx_def xk xv xn]]]
    [xbrt_def b null x null null]]]


;;;;;;;;;;;;;;;;  cons  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbrt_cons xbrt [] #:transparent
  #:methods gen:gxbrt
  [[define [xbrt-mk t] [lambda [k v x l r] [xbrt_cons k v x l r]]]
   [define [xbrt-set t k v]
     [let [[pv [xbrt-get t k]]]
       [if [null? pv]
       [x_set t [rbk<-string k] [list k v] [xxf [xbrt-gnx t]] val-def [xbrt-mk t]]
       [x_set t [rbk<-string k] [cons k [cons v [cdr pv]]] [xxf [xbrt-gnx t]] val-def [xbrt-mk t]]]]]
   [define [xbrt-get-gnx t k]
     [x_get-gnx t [rbk<-string k] null]]
   [define [xbrt-get t k]
      [x_get t [rbk<-string k] null]];]
   [define [xbrt-del t k]
     [x_del t [rbk<-string k] [xxf [xbrt-gnx t]] [xbrt-mk t]]]
]]

[define xbrt_cons_root
  [let* [[b [rbk<-bin-char-str ""]]
         [x [gnx_null]]]
    [xbrt_cons b null x null null]]]


;;;;;;;;;;;;;;;;  file append  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbrt_file_app xbrt [] #:transparent
  #:methods gen:gxbrt
  [[define [xbrt-mk t] [lambda [k v x l r] [xbrt_file_app k v x l r]]]
   [define [xbrt-set t k v]
     [x_set t [rbk<-bin-char-str k] [cons k v] [xxf [xbrt-gnx t]] val-def [xbrt-mk t]]]
   [define [xbrt-get-gnx t k]
     [x_get-gnx t [rbk<-bin-char-str k] null]]
   [define [xbrt-get t k]
      [x_get t [rbk<-bin-char-str k] null]];]
   [define [xbrt-del t k]
     [x_del t [rbk<-bin-char-str k] [xxf [xbrt-gnx t]] [xbrt-mk t]]]
]]

[define xbrt_file_app_root
  [let* [[b [rbk<-bin-char-str ""]]
         [x [gnx_null]]]
    [xbrt_file_app b null x null null]]]

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

[define [x_set t k v xf vf mkx]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [let [[h [car np]]]
      [let [[b [xbrt-key h]] [hv [xbrt-val h]] [x [xbrt-gnx h]][l [xbrt-left h]][r [xbrt-right h]]]
          [if [equal? nki kl] 
            [if [equal? hmi hbl]
              [let* [[mnvc [vf v x]][node [mkx b mnvc [xf mnvc l r] l r]]]
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

[define [x_get t k d]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [if [null? np] d
      [let [[n [car np]]]
        [if [and [equal? hmi [gbk-length [xbrt-key n]]][equal? nki kl]]
          [xbrt-val n]
          d]]]]]

[define [x_get-gnx t k d]
  [let-values [[[nki kl hmi hbl np] [qk t k]]]
    [if [null? np] d
      [let [[n [car np]]]
        [if [and [equal? hmi [gbk-length [xbrt-key n]]][equal? nki kl]] [xbrt-gnx n] d]]]]]
  
  
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[struct xbrz [back node nx] #:transparent]

[struct tz [head state zx] #:transparent]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define-generics gnx
  [nx-set gnx nn]
]

[struct nx_def [key_depth] #:transparent
  #:methods gen:gnx
  [[define [nx-set gnx node]
     [nx_def [+ [gbk-length [xbrt-key node]] [nx_def-key_depth gnx]]]]
  ]]
 
[define-generics gzx
  [tx-set gzx z s]
]

[struct zx_def [count_val] #:transparent
  #:methods gen:gzx
  [[define [tx-set gzx z s] [zx_def [add1 [zx_def-count_val gzx]]]]
  ]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [xbrz-first t cf]
  [tz [xbrz null t [nx_def 0]] 'pre [zx_def 1]]]

[define [xbrz-next z cf]
  [let [[h [tz-head z]]
        [s [tz-state z]]
        [x [tz-zx z]]]
    [if [equal? 'pre s]
      [let [[l [xbrt-left [xbrz-node h]]]]
        [if [or [null? l] [cf l z]]
          [tz h 'int [tx-set x z 'int]]
          [tz [xbrz h l [nx-set [xbrz-nx h] l]] 'pre [tx-set x z 'pre]]]] 
      [if [equal? 'int s]
        [let [[r [xbrt-right [xbrz-node h]]]] 
          [if [or [null? r] [cf r z]]
            [tz h 'pst [tx-set x z 'pst]]
            [tz [xbrz h r [nx-set [xbrz-nx h] r]] 'pre [tx-set x z 'pre]]]]    
        [if [null? [xbrz-back [tz-head z]]]
          null
          [if [equal? 0 [xbrt-kfb? [xbrz-node h]]]
            [tz [xbrz-back [tz-head z]] 'int [tx-set x z 'int]] 
            [tz [xbrz-back [tz-head z]] 'pst [tx-set x z 'pst]]]]]]]] 

[define [xbrz-last t cf]
  [tz [xbrz null t [nx_def 0]] 'pst [zx_def 1]]]

[define [xbrz-prev z cf]
  [let [[h [tz-head z]]
        [s [tz-state z]]
        [x [tz-zx z]]]
    [if [equal? 'pst s]
      [let [[r [xbrt-right [xbrz-node h]]]]
        [if [or [null? r] [cf r z]]
          [tz h 'int [tx-set x z 'int]]
          [tz [xbrz h r [nx-set [xbrz-nx h] r]] 'pst [tx-set x z 'pst]]]] 
      [if [equal? 'int s]
        [let [[l [xbrt-left [xbrz-node h]]]] 
          [if [or [null? l] [cf l z]]
            [tz h 'pre [tx-set x z 'pre]]
            [tz [xbrz h l [nx-set [xbrz-nx h] l]] 'pst [tx-set x z 'pst]]]]    
        [if [null? [xbrz-back [tz-head z]]]
          null
          [if [equal? 1 [xbrt-kfb? [xbrz-node h]]]
            [tz [xbrz-back [tz-head z]] 'int [tx-set x z 'int]] 
            [tz [xbrz-back [tz-head z]] 'pre [tx-set x z 'pre]]]]]]]] 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [xbrz_tdp_def n z] #f]
[define [xbrz_tdp_tst n z] [< 5 [nx_def-key_depth [xbrz-nx [tz-head z]]]]]

[define [mhcctf x y] [if [equal? x y] #t #f]]

[define [xbrz_vtf_def z]
  [list [tz-state z]
        [nx_def-key_depth [xbrz-nx [tz-head z]]]
        [zx_def-count_val [tz-zx z]]
        [xbrt-val [xbrz-node [tz-head z]]]
        ]]
[define [xbrz_vtf_tst z]
  [if [and [equal? [tz-state z] 'int] [equal? 5 [nx_def-key_depth [xbrz-nx [tz-head z]]]]] 
    [list [tz-state z] [xbrt-gnx [xbrz-node [tz-head z]]]] 
    [void]]]
[define [xbrz_vtf_val z]
  [if [equal? [tz-state z] 'pre]
    [let [[v[xbrt-val [xbrz-node [tz-head z]]]]]
      [if [null? v] [void] [list [cdr v]]]]
    [void]]]
[define [xbrz_vtf_main z]
  [if [equal? [tz-state z] 'pre]
    [let [[v[xbrt-val [xbrz-node [tz-head z]]]]]
      [if [null? v] [void] v]]
    [void]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [stream<-xbrt t #:tdp [tdp xbrz_tdp_def] #:vtf [vtf xbrz_vtf_def]]
  [stream-filter [lambda [x] [not [void? x]]]
    [stream-unfold
      [lambda [x] [vtf x]]
      [lambda [x] [not [null? x]]]
      [lambda [x] [xbrz-next x tdp]]
      [xbrz-first t tdp]]]]

[define [stream<-xbrt-rev t #:tdp [tdp xbrz_tdp_def] #:vtf [vtf xbrz_vtf_def]]
  [stream-filter [lambda [x] [not [void? x]]]
    [stream-unfold
      [lambda [x] [vtf x]]
      [lambda [x] [not [null? x]]]
      [lambda [x] [xbrz-prev x tdp]]
      [xbrz-last t tdp]]]]

[define [gxbrt<-stream t s]
  [if [stream-null? s] t
    [let [[e [stream-car s]]]
      [gxbrt<-stream [xbrt-set t [car e] [cdr e]] [stream-cdr s]]]]]


[define [rec-set t l]
  [if [null? l] t
    [let [[e [car l]]]
      [rec-set [xbrt-set t e [cat "" e]] [cdr l]]]]]


[define [xbrt<-stream t s]
  [if [stream-null? s] t
    [let [[e [stream-car s]]]
      [xbrt<-stream [xbrt-set t [car e] [cdr e]] [stream-cdr s]]]]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      [let [[res [stream->list [stream-map car [stream<-xbrt t #:vtf xbrz_vtf_val]]]]
            [dres [stream->list [stream-map car [stream<-xbrt dt #:vtf xbrz_vtf_val]]]]]
        [displayln [take res 10]]
        [displayln [take skl 10]]
      [if [equal? res skl]  [displayln "passed xset test!"]
                            [displayln "FAILED xset test!"]]
      [if [equal? dres dskl]  [displayln "passed xdel test!"]
                              [displayln "FAILED xdel test!"]]
      [list t dt rkl]]]]]

[define [xbrt-test]
    [let [[mt [time [unit-test 12 64]]]]
      [displayln "fct strm def:"]   
      [disp-stream [stream-take 20 [stream<-xbrt [car mt] ]]]
      [disp-stream [stream-take 20 [stream<-xbrt [car mt] #:vtf xbrz_vtf_val]]]
      [displayln "fct strm tst:"]
      [disp-stream [stream<-xbrt [car mt] #:tdp xbrz_tdp_tst #:vtf xbrz_vtf_tst]]
      [displayln [xbrt-get-gnx [car mt] ""]]
      [displayln [xbrt-get-gnx [cadr mt] ""]]

      [disp-stream [stream-take 200 [stream<-xbrt [car mt]]]]
      [disp-stream [stream-take 200 [stream<-xbrt-rev [car mt]]]]
      ]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
