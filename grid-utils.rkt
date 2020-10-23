#lang racket
[require "common.rkt"]
[require "stream.rkt"]
[require "XBRT.rkt"]
;[require "polydex.rkt"]
[require "time.rkt"]
[require "XML.rkt"]
[require "X3D.rkt"]
[require "HTML.rkt"]
[require "align.rkt"]
[require "odf.rkt"]

[provide [all-defined-out]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define nlist-testlist [list "a" [list "b" "c1" "c2"] "d"]]
[define xt-nlist [list "ah0" [list "s0" [list "x1"][list "x2"][list "x3"]]
                       [list "s1" [list "t1"]]
                       [list "s2" [list "y1"][list "y2" [list "z1"][list "z2"]][list "y3"]]]]
[define yt-nlist [list "a0" [list "s0"]
                       [list "s1" [list "t1"]]
                       [list "s2" [list "y1"][list "y2"][list "y3"]]]]
                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [mk-covered e v]
  [let [[h [car e]][x [cadr e]][y [caddr e]]]
    [let [[xp [car x]]
          [yp [car y]]
          [xs [cadr x]]
          [ys [cadr y]]]
      [stream-cdr [stream<-list
      [for*/list [[i [in-range 0 xs]][j [in-range 0 ys]]]
        [list [cons v [cdr h]] [list [+ xp i] 1] [list [+ yp j] 1]]]]]]]]
      
[define [add-covered v s]
  [stream-append s
  [stream-concat
  [stream-map [lambda [x] [mk-covered x v]]
  s]]]]

[define [mk-empty x y tid]
  [stream<-list
  [for*/list [[i [in-range 0 x]][j [in-range 0 y]]]
    [list [list [list [list] "empty"] tid] [list [+ 1 i] 1] [list [+ 1 j] 1]]]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [nstream<-nlist l]
  [stream<-list
  [map [lambda [x]
         [if [list? x]
           [stream<-list
             [map [lambda [e][if [list? e][stream<-list e] e]] x]]
           x]]
  l]]]

[define [nlist<-nstream l]
  [list<-stream
  [stream-map
    [lambda [x]
      [if [stream? x]
        [list<-stream
          [stream-map [lambda [e][if [stream? e] [list<-stream e] e]] x]]
        x]]
  l]]]

[define [mk-cell t b]
  [cons 'cell [cons [list '@ [list 'tag t]] b]]]

[define [xml<-nlist l]
  [if [null? l] [list]
    [mk-cell
      [car l]
      [map [lambda [e]
             [if [list? e]
               [xml<-nlist e]
               [mk-cell e [list]]]] 
      [cdr l]]]]]
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [xbrt-enc-xml x]
  [xbrt-enc-xml-help-node xbrt_test_root "" x]]

[define [xbrt-enc-xml-help-node t kp n]
  [if [string? n]
    [xbrt-set t kp [list n]]
    [let [[nl [[sxpath "/child::node()" [list]] n]]]
      [let [[kpl [bit-str-list [length nl]]]
            [nt [xbrt-set t kp [list [car n] [[sxpath "/attribute::*" [list]] n]]]]]
        [xbrt-enc-xml-help-list nt kp nl kpl]]]
  ]]
[define [xbrt-enc-xml-help-list t kp nl kl]
  [if [null? nl] t
    [let [[nt [xbrt-enc-xml-help-node t [cat kp [car kl]] [car nl]]]]
      [xbrt-enc-xml-help-list nt kp [cdr nl] [cdr kl]]]]]     
[define [bit-str-list l]
  [if [equal? 1 l]
    [list "0"] 
    [for/list [[i [in-range 0 l]]]
      [pad i [integer-length [sub1 l]] 2]]]]

[define [cell_mod h tid]
  [lambda [z]
    [let* [[cn [xbpz-node [tz-head z]]]
           [mh [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx cn]]]]]
      [list
        [list [reverse [taglist [tz-head z]]] tid ]
        ;tn
        [list [+ 1 [zx_def-leaf_count [tz-tx z]]]
              [x_node_def-node_leaf_count [gnx_def-node_x [xbrt-gnx cn]]]]
        [list [+ 1 [npx_def-val_depth [xbpz-bpx [tz-head z]]]]
              [if [equal? 1 mh] [+ mh [- h [+ 1 [npx_def-val_depth [xbpz-bpx [tz-head z]]]] ]] 1]]
        ]
      ]]]

[define [mk-header_enc-xml t tid]
  [let* [[h [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx t]]]]]
    [stream-map [cell_mod h tid]
    [stream<-xbrz [mktz_enc-xml t]]]]]

[define [taglist w]
  [if [null? [xbpz-back w]] [list [cadar [caddr [xbrt-val [xbpz-node w]]]]]
    [if [null? [xbrt-val [xbpz-node w]]]
      [taglist [xbpz-back w]]
      [cons [cadar [caddr [xbrt-val [xbpz-node w]]]] [taglist [xbpz-back w]]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [test-cf tid style]
  [lambda [x y]
  [list [list [list [list [cat "h" [car [reverse [caar x]]] "v" [car [reverse [caar y]]] "?"] style]] tid]
        [list [caadr x] 1]
        [list [caaddr y] 1]]]]

[define [mk-table-block cellf row-strm col-strm]
  [stream-concat
  [stream-map
    [lambda [y]
      [stream-map
        [lambda [x]  [cellf y x]]
        col-strm]]
    row-strm]]]

;[define xcminf [let [[f [lambda [z] [list-ref z 1]]]] [lambda [s] [f [[stream-argext < f] s]]]]]
;[define xcmaxf [let [[f [lambda [z] [+ [list-ref z 1] [sub1 [list-ref z 2]]]]]] [lambda [s] [f [[stream-argext > f] s]]]]]

[define [shift-2d-block xs ys s]
  [stream-map
    [lambda [z]
      [let [[x [list-ref z 1]][y [list-ref z 2]]]
        [list [car z] ;[cadr z][let [[x
              [list [+ xs [car x]] [cadr x]]
              [list [+ ys [car y]] [cadr y]]]]] s]]


;[define [flip-block-x s]
;  [let [[xmin [xcminf s]][xmax [xcmaxf s]]]
;    [stream-map [lambda [z]
;    [append [take z 1] [list [+ 1 xmin xmax [- [+ [list-ref z 1][list-ref z 2]]]] [list-ref z 2][list-ref z 3][list-ref z 4] ]]] s]]]

[define [transpose-2d-cells l]
  [list [car l] [list-ref l 2] [list-ref l 1]]]

[define [sort-cells d s]
  [stream-sort [lambda [x y] [< [car [list-ref x d]] [car [list-ref y d]]]] s]]

;[define [cell-filter-leaves s] 
;  [stream-filter [lambda [x] [equal? 1 [cadar x]]] s]]

[define [cell-filter-x v] 
  [lambda [x] [equal? [add1 v] [apply + [cadr x]]]]]
[define [cell-filter-y v]
  [lambda [x] [equal? [add1 v] [apply + [caddr x]]]]]

[define [mk-gen-table-stream xht yht tid dbf xf yf]
  [let [[xpht [xbrt-enc-xml xht]]
        [ypht [xbrt-enc-xml yht]]]
  [let [[xhth [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx xpht]]]]
        [yhth [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx ypht]]]]]
  [let [[ixhb [mk-header_enc-xml xpht tid]]
        [iyhb [mk-header_enc-xml ypht tid]]]
  [let [[xhb  [shift-2d-block yhth 0 ixhb]]
        [yhb  [stream-map transpose-2d-cells [shift-2d-block xhth 0 iyhb]]]]
  [let [[db   [mk-table-block dbf 
                [sort-cells 1 [stream-filter [cell-filter-y xhth] xhb]]
                [sort-cells 2 [stream-filter [cell-filter-x yhth] yhb]] ]]]
    [add-covered [list [list] "covered"]
    [stream-append [stream-map xf xhb] [stream-map yf yhb] db [mk-empty yhth xhth tid]]]]]]]]]

[define [table-filter-null s] [stream-filter [lambda [x] [not [null? [caaar x]]]] s]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;[html-table-test]
[define [file<-html_table fname html_table_stream]                                            
  [let* [[out-file-html [open-output-file [cat "../" fname ".html"] #:exists 'replace]]]
    [write-html
      [mk-html [list] [list]
        [list [mk-css-link [cat "./" fname ".css"]]]
        [list]
        [list [mk-table-divs html_table_stream]]]
      out-file-html]
    [close-output-port out-file-html]
    [file<-stream [cat "../" fname ".css"] displayln
      [stream-append diff-css
                     table-div-css
                     [mk-table-css [stream html_table_stream]]]]
    ]]

[define [test-hf x]
  [cons [list [cons [list [car [reverse [caar x]]] "ce1"] [caar x]] [cadar x] ] [drop x 1]]]

[define [html-table-test]
  [let [[tt10 [mk-gen-table-stream
                [xml<-nlist nlist-testlist]
                [xml<-nlist nlist-testlist] "tf" [test-cf "tf" "ce1"] test-hf test-hf]]
        [tt20 [mk-gen-table-stream
                [xml<-nlist xt-nlist]
                [xml<-nlist yt-nlist] "tf" [test-cf "tf" "ce1"] test-hf test-hf]]]
  [disp-stream tt10] 
  [file<-html_table "html-table-test11" [table-filter-null tt10]]
  [disp-stream tt20] 
  [file<-html_table "html-table-test21" [table-filter-null tt20]]
;  [save-as-ods "mfo40.ods" [ods-tab-conv tt20] [ods-styles]]
;  [writeln [ods-tab-conv tt20]]
  ]]

[define [col-max x] [+ -1 [caadr x] [cadadr x]]]
[define [ods-tab-conv st]
  [let [[col-count [argmax [lambda [x] x] [list<-stream [stream-map col-max st]]]]]  
    [mk-ods-table "ta1" [cons
    [mk-ods-col "co1" col-count]
    [list<-stream
    [stream-map ods-row-conv
    [stream-map [lambda [x] [sort-cells 1 x]]
    [stream-group [lambda [a b] [equal? [caaddr a] [caaddr b]]]
    [sort-cells 2 st]]]]]]]]]

[define [ods-row-conv s]
  [mk-ods-row "ro1" [list<-stream 
    [stream-map ods-conv s]]]]

[define [ods-conv e]
  [cond [[equal? [list] [caaar e]]
           [cond [[equal? "covered" [cadr [caar e]]]
                     ods-covered-cell]
                 [[equal? "empty" [cadr [caar e]]]
                     [ods-empty-cell "ce1"]]]]
;        [[regexp-match #rx"h.*v.*." [caaar e]]
;           [mk-ods-cell "ce1" "string" [list] 1 1
;             [mk-ods-text [list [caaar e]]]]]
        [else
           [mk-ods-cell [cadr [caaar e]] "string" [list] [cadr [cadr e]] [cadr [caddr e]] 
             [mk-ods-text [list [car [caaar e]]]]]]]]


  