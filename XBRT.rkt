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
[require racket/require
  [path-up "XBRT-core.rkt"]]

[require bitsyntax]

[provide [all-from-out "XBRT-core.rkt"]]
[provide [all-defined-out]]

[define [inn g r] [if [null? r] r [g r]]]
[define [inn01 r] [if [null? r] 0 1]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; RBK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct gnx_null [] #:transparent
  #:methods gen:xbrt_gnx
  [[define [mk_xbrt_xf s] [lambda [p l r] [gnx_null]]]]]

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

[struct x_val_def  [val_count val_max_height val_min_height] #:transparent]
[define [vx-f p l r]
  [x_val_def [+ [if [null? p] 0 1]
              [if [null? l] 0 [x_val_def-val_count [gnx_def-val_x [xbrt-gnx l]]]]
              [if [null? r] 0 [x_val_def-val_count [gnx_def-val_x [xbrt-gnx r]]]]]
              [+ [if [null? p] 0 1]
                 [max [if [null? l] 0 [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx l]]]]
                      [if [null? r] 0 [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx r]]]]]]
              [+ [if [null? p] 0 1]
                 [min [if [null? l] 0 [x_val_def-val_min_height [gnx_def-val_x [xbrt-gnx l]]]]
                      [if [null? r] 0 [x_val_def-val_min_height [gnx_def-val_x [xbrt-gnx r]]]]]]
             ]]

[struct x_node_def [node_count node_leaf_count node_max_height node_min_height] #:transparent]
[define [nx-f p l r]
  [let [[ln [if [null? l] null [gnx_def-node_x [xbrt-gnx l]]]]
        [rn [if [null? r] null [gnx_def-node_x [xbrt-gnx r]]]]]        
    [x_node_def [+ 1 [if [null? l] 0 [x_node_def-node_count ln]]
                     [if [null? r] 0 [x_node_def-node_count rn]]]
                [if [and [null? l] [null? r]] 1 
                  [+ [if [null? l] 0 [x_node_def-node_leaf_count ln]]
                     [if [null? r] 0 [x_node_def-node_leaf_count rn]]]]
                [+ 1 [max [if [null? l] 0 [x_node_def-node_max_height ln]]
                          [if [null? r] 0 [x_node_def-node_max_height rn]]]]
                [+ 1 [min [if [null? l] 0 [x_node_def-node_min_height ln]]
                          [if [null? r] 0 [x_node_def-node_min_height rn]]]]
                ]]]

[struct gnx_def [key_x val_x node_x] #:transparent
  #:methods gen:xbrt_gnx
  [[define [mk_xbrt_xf s]
     [lambda [p l r] 
       [let [[kx [kx-f p l r]]
             [vx [vx-f p l r]]
             [nx [nx-f p l r]]]
         [gnx_def  kx vx nx]]]]]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;  simple  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbrt_def xbrt [] #:transparent
  #:methods gen:gxbrt
  [[define [xbrt-set t k v]
       [x_set t [rbk<-string k] v [mk_xbrt_xf [xbrt-gnx t]] xbrt_def]]
   [define [xbrt-get t k]
      [inn xbrt-val [x_nget t [rbk<-string k] null]]]
   [define [xbrt-xget t k]
      [inn xbrt-gnx [x_nget t [rbk<-string k] null]]]
   [define [xbrt-nget t k]
      [x_nget t [rbk<-string k] null]]
   [define [xbrt-del t k]
     [x_del t [rbk<-string k] [mk_xbrt_xf [xbrt-gnx t]] xbrt_def]]
]]

[define xbrt_def_root
  [let* [[b [rbk<-string ""]]
         [x [gnx_null]]]
    [xbrt_def b null x null null]]]

;;;;;;;;;;;;;;;;  cons  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbrt_cons xbrt [] #:transparent
  #:methods gen:gxbrt
  [[define [xbrt-set t k v]
     [let [[pv [xbrt-get t k]]]
       [x_set t [rbk<-string k] [cons v pv] [mk_xbrt_xf [xbrt-gnx t]] xbrt_cons]]]
   [define [xbrt-get t k]
      [inn xbrt-val [x_nget t [rbk<-string k] null]]]
   [define [xbrt-xget t k]
      [inn xbrt-gnx [x_nget t [rbk<-string k] null]]]
   [define [xbrt-nget t k]
      [x_nget t [rbk<-string k] null]]
   [define [xbrt-del t k]
     [x_del t [rbk<-string k] [mk_xbrt_xf [xbrt-gnx t]] xbrt_cons]]
]]

[define xbrt_cons_root
  [let* [[b [rbk<-string ""]]
         [x [gnx_null]]]
    [xbrt_cons b null x null null]]]

;;;;;;;;;;;;;;;;  file append  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbrt_file_rep xbrt [] #:transparent
  #:methods gen:gxbrt
  [[define [xbrt-set t k v]
     [let [[pv [xbrt-get t k]]]
       [if [null? pv]
         [let [[op [open-output-file k #:exists 'replace]]]
           [displayln v op]
           [x_set t [rbk<-string k] op [mk_xbrt_xf [xbrt-gnx t]] xbrt_file_rep]]
         [begin [displayln v pv] t]]]]
   [define [xbrt-get t k]
      [inn xbrt-val [x_nget t [rbk<-string k] null]]]
   [define [xbrt-xget t k]
      [inn xbrt-gnx [x_nget t [rbk<-string k] null]]]
   [define [xbrt-nget t k]
      [x_nget t [rbk<-string k] null]]
   [define [xbrt-del t k]
     [x_del t [rbk<-string k] [mk_xbrt_xf [xbrt-gnx t]] xbrt_file_rep]]
]]

[define xbrt_file_rep_root
  [let* [[b [rbk<-string ""]]
         [x [gnx_null]]]
    [xbrt_file_rep b null x null null]]]

;;;;;;;;;;;;;;;;; for testing;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct xbrt_test xbrt [] #:transparent
  #:methods gen:gxbrt
  [[define [xbrt-set t k v]
     [x_set t [rbk<-bin-char-str k] [cons k v] [mk_xbrt_xf [xbrt-gnx t]] xbrt_test]]
   [define [xbrt-get t k]
      [inn xbrt-val [x_nget t [rbk<-bin-char-str k] null]]]
   [define [xbrt-xget t k]
      [inn xbrt-gnx [x_nget t [rbk<-bin-char-str k] null]]]
   [define [xbrt-nget t k]
      [x_nget t [rbk<-bin-char-str k] null]]
   [define [xbrt-del t k]
     [x_del t [rbk<-bin-char-str k] [mk_xbrt_xf [xbrt-gnx t]] xbrt_test]]
]]

[define xbrt_test_root
  [let* [[b [rbk<-bin-char-str ""]]
         [xk [kx-f null null null]]
         [xv [vx-f null null null]]
         [xn [nx-f null null null]]
         [x [gnx_def xk xv xn]]]
    [xbrt_test b null x null null]]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct npx_def [key_depth val_depth] #:transparent
  #:methods gen:gnx
  [[define [nx-set gnx node]
     [npx_def [+ [gbk-length [xbrt-key node]] [npx_def-key_depth gnx]]
             [+ [if [null? [xbrt-val node]] 0 1] [npx_def-val_depth gnx]]]]
  ]]

[define [mk_npx_def t]
  [npx_def [gbk-length [xbrt-key t]] 0]]


[struct zx_f_def [val_count leaf_count] #:transparent
  #:methods gen:gzx
  [[define [tx-set gzx nh nstate]
     [let [[cn [xbpz-node nh]]]
       [if [equal? nstate 'int] 
         [zx_f_def [+ [inn01 [xbrt-val cn]] [zx_f_def-val_count gzx]]
                   [+ [if [and [null? [xbrt-left cn]][null? [xbrt-right cn]]] 1 0]
                      [zx_f_def-leaf_count gzx]]]
         [zx_f_def [zx_f_def-val_count gzx] [zx_f_def-leaf_count gzx]]]]]
  ]]

[define [mk_zx_f_def t]
  [zx_f_def 
    [inn01 [xbrt-val t]]
    [if [and [null? [xbrt-left t]]
             [null? [xbrt-right t]]
             [not [null? [xbrt-val t]]]]
      1 0]]]

[struct zx_r_def [val_count leaf_count] #:transparent
  #:methods gen:gzx
  [[define [tx-set gzx nh nstate]
     [let [[cn [xbpz-node nh]]]
       [if [equal? nstate 'int] 
         [zx_r_def [+ [inn01 [xbrt-val cn]] [zx_r_def-val_count gzx]]
                   [+ [if [and [null? [xbrt-left cn]][null? [xbrt-right cn]]] 1 0]
                      [zx_r_def-leaf_count gzx]]]
         [zx_r_def [zx_r_def-val_count gzx] [zx_r_def-leaf_count gzx]]]]]
  ]]

[define [mk_zx_r_def t]
  [zx_r_def [inn01 [xbrt-val t]]
          [if [and [null? [xbrt-left t]]
                   [null? [xbrt-right t]]
                   [not [null? [xbrt-val t]]]]
              1 0]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define [xbrz_tdp_def n z] #f]

[define [xbrz_tdp_tst n z] [< 3 [npx_def-key_depth [xbpz-bpx [tz-head z]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define [xbrz_vtf_def z]
  [list [tz-state z]
        [npx_def-key_depth [xbpz-bpx [tz-head z]]]
        [zx_f_def-val_count [tz-tx z]]
        [xbrt-val [xbpz-node [tz-head z]]]
        ]]
[define [xbrz_vtr_def z]
  [list [tz-state z]
        [npx_def-key_depth [xbpz-bpx [tz-head z]]]
        [zx_r_def-val_count [tz-tx z]]
        [xbrt-val [xbpz-node [tz-head z]]]
        ]]
[define [xbrz_vtf_tst z]
  [let [[cn [xbpz-node [tz-head z]]]]
  [if [and [equal? [tz-state z] 'int]] 
    [list [tz-state z]
          [npx_def-key_depth [xbpz-bpx [tz-head z]]]
          [xbrt-val cn]
          [x_val_def-val_count [gnx_def-val_x [xbrt-gnx cn]]]
          ] 
    [void]]]]

[define [xbrz_vtf_val z]
  [if [equal? [tz-state z] 'pre]
    [let [[v[xbrt-val [xbpz-node [tz-head z]]]]]
      [if [null? v] [void] [list [cdr v]]]]
    [void]]]

[define [xbrz_vtf_main z]
  [if [equal? [tz-state z] 'pre]
    [let [[v[xbrt-val [xbpz-node [tz-head z]]]]]
      [if [null? v] [void] [cons [string<-rbk [xbrt-key [xbpz-node [tz-head z]]]] v]]]
    [void]]]

[define [xbrz_vtf_pre_val z]
  [if [equal? [tz-state z] 'pre]
    [let [[v [xbrt-val [xbpz-node [tz-head z]]]]]
      [if [null? v] [void] v]]
    [void]]]





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct tz_fdv tz [] #:transparent
  #:methods gen:gtz
  [[define [gtz-iter gtz] xbrz-next]
   [define [gtz-cdp gtz] xbrz_tdp_def]
   [define [gtz-vtf gtz] xbrz_vtf_val]
  ]]

[define [mktz_fdv t]
  [tz_fdv [xbpz null t [mk_npx_def t]] 'pre [mk_zx_f_def t]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct tz_fdd tz [] #:transparent
  #:methods gen:gtz
  [[define [gtz-iter gtz] xbrz-next]
   [define [gtz-cdp gtz] xbrz_tdp_def]
   [define [gtz-vtf gtz] xbrz_vtf_def]
  ]]

[define [mktz_fdd t]
  [tz_fdd [xbpz null t [mk_npx_def t]] 'pre [mk_zx_f_def t]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct tz_ftt tz [] #:transparent
  #:methods gen:gtz
  [[define [gtz-iter gtz] xbrz-next]
   [define [gtz-cdp gtz] xbrz_tdp_tst]
   [define [gtz-vtf gtz] xbrz_vtf_tst]
  ]]

[define [mktz_ftt t]
  [tz_ftt [xbpz null t [mk_npx_def t]] 'pre [mk_zx_f_def t]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct tz_rdd tz [] #:transparent
  #:methods gen:gtz
  [[define [gtz-iter gtz] xbrz-prev]
   [define [gtz-cdp gtz] xbrz_tdp_def]
   [define [gtz-vtf gtz] xbrz_vtr_def]
  ]]

[define [mktz_rdd t]
  [tz_rdd [xbpz null t [mk_npx_def t]] 'pst [mk_zx_r_def t]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct tz_fdm tz [] #:transparent
  #:methods gen:gtz
  [[define [gtz-iter gtz] xbrz-next]
   [define [gtz-cdp gtz] xbrz_tdp_def]
   [define [gtz-vtf gtz] xbrz_vtf_main]
  ]]

[define [mktz_fdm t]
  [tz_fdm [xbpz null t [mk_npx_def t]] 'pre [mk_zx_f_def t]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[struct tz_fdpv tz [] #:transparent
  #:methods gen:gtz
  [[define [gtz-iter gtz] xbrz-next]
   [define [gtz-cdp gtz] xbrz_tdp_def]
   [define [gtz-vtf gtz] xbrz_vtf_pre_val]
  ]]

[define [mktz_fdpv t]
  [tz_fdpv [xbpz null t [mk_npx_def t]] 'pre [mk_zx_f_def t]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;[define [xbrz-first t cf]
;  [tz [xbpz null t [npx_def 0]] 'pre [zx_def 1]]]
;[define [xbrz-last t cf]
;  [tz [xbpz null t [npx_def 0]] 'pst [zx_def 1]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    [let* [[t [time [rec-set xbrt_test_root rkl]]]
          [dt [xbrt-del t re]]]
      [let [[res [stream->list [stream-map car [stream<-xbrz [mktz_fdv t]] ]]]
            [dres [stream->list [stream-map car [stream<-xbrz [mktz_fdv dt]] ]]]]
        [displayln [take res 10]]
        [displayln [take skl 10]]
      [if [equal? res skl]  [displayln "passed xset test!"]
                            [displayln "FAILED xset test!"]]
      [if [equal? dres dskl]  [displayln "passed xdel test!"]
                              [displayln "FAILED xdel test!"]]
      [list t dt rkl]]]]]

[define [xbrt-test]
    [let [[mt [time [unit-test 12 64]]]]
      [displayln "fct strm def def:"]   
      [disp-stream [stream-take 20 [stream<-xbrz [mktz_fdd [car mt]]]]]
      [displayln "fct strm def val:"]
      [disp-stream [stream-take 20 [stream<-xbrz [mktz_fdv [car mt]]]]]
      [displayln "fct strm tst tst:"]
      [disp-stream [stream<-xbrz [mktz_ftt [car mt]]]]
      [displayln "fct strm rev dd:"]
      [disp-stream [stream-take 20 [stream<-xbrz [mktz_rdd [car mt]]]]]
      [newline]
      [displayln [xbrt-xget [car mt] ""]]
      [displayln [xbrt-xget [cadr mt] ""]]
      [newline]
      ]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [xbrt-enc-xml x]
  [xbrt-enc-xml-help-node xbrt_test_root "" x]]

[define [xbrt-enc-xml-help-node t kp n]
  [if [string? n]
    [xbrt-set t kp [list n]]
    [let [[nl [[sxpath "/child::node()" [list]] n]]]
      [let [[kpl [if [equal? 1 [length nl]] [list "0"] [tns [length nl]]]]
            [nt [xbrt-set t kp [list [car n] [[sxpath "/attribute::*" [list]] n]]]]]
        [xbrt-enc-xml-help-list nt kp nl kpl]]]
  ]]
[define [xbrt-enc-xml-help-list t kp nl kl]
  [if [null? nl] t
    [let [[nt [xbrt-enc-xml-help-node t [cat kp [car kl]] [car nl]]]]
      [xbrt-enc-xml-help-list nt kp [cdr nl] [cdr kl]]]]]
      

[define [tns l] [for/list [[i [in-range 0 l]]] [pad i [integer-length [sub1 l]] 2]]]

[define xt [cadr [ssax:xml->sxml [open-input-string
[cat "<cell tag=\"a0\">"
       "<cell tag=\"s0\">"
         "<cell tag=\"x1\"></cell>"
         "<cell tag=\"x2\"></cell>"
         "<cell tag=\"x3\"></cell>"
       "</cell>"
       "<cell tag=\"s1\">"
         "<cell tag=\"t1\"></cell>"
       "</cell>"
       "<cell tag=\"s2\">"
         "<cell tag=\"y1\"></cell>"
         "<cell tag=\"y2\">"
           "<cell tag=\"z1\"></cell>"
           "<cell tag=\"z2\"></cell>"
         "</cell>"
         "<cell tag=\"y3\"></cell>"
       "</cell>"
     "</cell>"]] [list]]]]

;[displayln [eval [read [open-input-string "[list 1 2 3]"]]]]

[define [xbrz_vtf_enc-xml z]
  [if [and [equal? [tz-state z] 'pre] [not [null? [xbrt-val [xbpz-node [tz-head z]]]]]]
    z
    [void]]]

[struct tz_enc-xml tz [] #:transparent
  #:methods gen:gtz
  [[define [gtz-iter gtz] xbrz-next]
   [define [gtz-cdp gtz] xbrz_tdp_def]
   [define [gtz-vtf gtz] xbrz_vtf_enc-xml]
  ]]

[define [mktz_enc-xml t]
  [tz_enc-xml [xbpz null t [mk_npx_def t]] 'pre [mk_zx_f_def t]]]

[define [bmod h tn]
  [lambda [z]
    [let* [[cn [xbpz-node [tz-head z]]]
           [mh [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx cn]]]]]
      [list
        z
        [cadar [caddr [xbrt-val cn]]]
        tn
        [+ 1 [npx_def-val_depth [xbpz-bpx [tz-head z]]]]
        [if [equal? 1 mh] [+ mh [- h [+ 1 [npx_def-val_depth [xbpz-bpx [tz-head z]]]] ]] 1]
        [+ 1 [zx_f_def-leaf_count [tz-tx z]]]
        [x_node_def-node_leaf_count [gnx_def-node_x [xbrt-gnx cn]]] mh]
      ]]]

[define [bmod2  tn]
  [lambda [z]
    [let* [[cn [xbpz-node [tz-head z]]]
           [mh [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx cn]]]]]
      [list
        [cadar [caddr [xbrt-val cn]]]
        tn
        [tz-tx z]
        [xbrt-gnx cn]
      ]]]]


[define [mk-cell-block t tn]
  [let [[h [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx [xbrt-enc-xml xt]]]]]]
    [stream-map [bmod h tn]
    [stream<-xbrz [mktz_enc-xml [xbrt-enc-xml xt]]]]]]

[disp-stream [stream-map cdr [mk-cell-block xt "tf"]]]
