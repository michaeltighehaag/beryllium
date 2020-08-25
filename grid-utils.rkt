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

[define [mk-empty x y]
  [stream<-list
  [for*/list [[i [in-range 0 x]][j [in-range 0 y]]]
    [list [list [list "empty"]] [list [+ 1 i] 1] [list [+ 1 j] 1]]]]]
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

[define [cell_mod h]
  [lambda [z]
    [let* [[cn [xbpz-node [tz-head z]]]
           [mh [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx cn]]]]]
      [list
        [list [taglist [tz-head z]] mh ]
        ;tn
        [list [+ 1 [zx_def-leaf_count [tz-tx z]]]
              [x_node_def-node_leaf_count [gnx_def-node_x [xbrt-gnx cn]]]]
        [list [+ 1 [npx_def-val_depth [xbpz-bpx [tz-head z]]]]
              [if [equal? 1 mh] [+ mh [- h [+ 1 [npx_def-val_depth [xbpz-bpx [tz-head z]]]] ]] 1]]
        ]
      ]]]

[define [mk-header_enc-xml t]
  [let* [[h [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx t]]]]]
    [stream-map [cell_mod h]
    [stream<-xbrz [mktz_enc-xml t]]]]]

[define [taglist w]
  [if [null? [xbpz-back w]] [list [cadar [caddr [xbrt-val [xbpz-node w]]]]]
    [if [null? [xbrt-val [xbpz-node w]]]
      [taglist [xbpz-back w]]
      [cons [cadar [caddr [xbrt-val [xbpz-node w]]]] [taglist [xbpz-back w]]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [test-cf x y]
  [list [list [list [cat "h" [caaar x] "v" [caaar y] "?"]]]
        [list [caadr x] 1]
        [list [caaddr y] 1]]]

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

[define [cell-filter-leaves s] 
  [stream-filter [lambda [x] [equal? 1 [cadar x]]] s]]

[define [mk-table-stream xht yht table_id dbf]
  [let [[xpht [xbrt-enc-xml xht]]
        [ypht [xbrt-enc-xml yht]]]
  [let [[xhth [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx xpht]]]]
        [yhth [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx ypht]]]]]
  [let [[ixhb [mk-header_enc-xml xpht]]
        [iyhb [mk-header_enc-xml ypht]]]
  [let [[xhb  [shift-2d-block yhth 0 ixhb]]
        [yhb  [stream-map transpose-2d-cells [shift-2d-block xhth 0 iyhb]]]]
  [let [[db   [mk-table-block dbf 
                [sort-cells 1 [cell-filter-leaves xhb]]
                [sort-cells 2 [cell-filter-leaves yhb]]]]]
    ;[disp-stream [mk-empty yhth xhth]]  
    [stream-map [lambda [x] [cons [list [caar x] table_id] [drop x 1]]]
    [stream-append xhb yhb db [mk-empty yhth xhth]]]]]]]]]

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

[define [html-table-test]
  [let [[tt10 [mk-table-stream
                [xml<-nlist nlist-testlist]
                [xml<-nlist nlist-testlist] "tf" test-cf]]
        [tt20 [mk-table-stream
                [xml<-nlist xt-nlist]
                [xml<-nlist yt-nlist] "tf" test-cf]]]
  [disp-stream tt10] 
  [file<-html_table "html-table-test10" tt10]
  [disp-stream tt20] 
  [file<-html_table "html-table-test20" tt20]
  [disp-stream
    [stream-map [lambda [x] [sort-cells 2 x]]
    [stream-group [lambda [a b] [equal? [caadr a] [caadr b]]]
    [sort-cells 1
    [add-covered "covered"
    tt10]]]]] 

  ]]