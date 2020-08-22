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




[define [cell_mod h tn]
  [lambda [z]
    [let* [[cn [xbpz-node [tz-head z]]]
           [mh [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx cn]]]]]
      [list
        [list [cadar [caddr [xbrt-val cn]]] mh z [taglist [tz-head z]] [xbpz-back [tz-head z]]]
        ;tn
        [+ 1 [zx_def-leaf_count [tz-tx z]]]
        [x_node_def-node_leaf_count [gnx_def-node_x [xbrt-gnx cn]]]
        [+ 1 [npx_def-val_depth [xbpz-bpx [tz-head z]]]]
        [if [equal? 1 mh] [+ mh [- h [+ 1 [npx_def-val_depth [xbpz-bpx [tz-head z]]]] ]] 1]
        ]
      ]]]

[define [mk-header_enc-xml t table_id]
  [let* [;[t [xbrt-enc-xml x]]
         [h [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx t]]]]]
    [stream-map [cell_mod h table_id]
    [stream<-xbrz [mktz_enc-xml t]]]]]

[define [disp-cell x][cons [caar x][cdr x]]]
[define [enc-xml_get-tag x]  [xbrt-val x]]
[define [taglist w]
  [if [null? [xbpz-back w]] [list [enc-xml_get-tag [xbpz-node w]]]
    [cons [enc-xml_get-tag [xbpz-node w]] [taglist [xbpz-back w]]]]]
;[disp-stream [stream-map disp-cell [mk-header_enc-xml xt "tf"]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [test-df p x y xh yh] [list [list [cat "h" xh "v" yh "?"]] x 1 y 1 1]]

[define [mk-table-stream-block df p row-strm col-strm]
  [let [[nrs [stream-zip [stream-map car row-strm] [stream-from 1]]]
        [ncs [stream-zip [stream-map car col-strm] [stream-from 1]]]]
  [stream-concat
  [stream-map
    [lambda [x]
      [stream-map
        [lambda [y]  [df p [cadr x] [cadr y] [caar x] [caar y]] ]
        ncs]]
    nrs]]]]

[define [set-block s]
  [stream-map [lambda [z] [cons [car z]  [map number->string [cdr z]]]]
  [stream-map disp-cell s]]]




[define xcminf [let [[f [lambda [z] [list-ref z 1]]]] [lambda [s] [f [[stream-argext < f] s]]]]]
[define xcmaxf [let [[f [lambda [z] [+ [list-ref z 1] [sub1 [list-ref z 2]]]]]] [lambda [s] [f [[stream-argext > f] s]]]]]

[define [shift-block xs ys s]
  [stream-map
    [lambda [z] [list [car z] ;[cadr z]
                  [+ xs [list-ref z 1]] [list-ref z 2]
                  [+ ys [list-ref z 3]] [list-ref z 4] ]]  s]]

[define [flip-block-x s]
  [let [[xmin [xcminf s]][xmax [xcmaxf s]]]
    [stream-map [lambda [z]
    [append [take z 1] [list [+ 1 xmin xmax [- [+ [list-ref z 1][list-ref z 2]]]] [list-ref z 2][list-ref z 3][list-ref z 4] ]]] s]]]

[define [transpose-cells l]
  [list [car l] [list-ref l 3] [list-ref l 4] [list-ref l 1][list-ref l 2] ]]

[define [cell-sort d s]
  [let [[p [sub1 [* 2 d]]]] [stream-sort [lambda [x y] [< [list-ref x p] [list-ref y p]]] s]]]

[define [cell-filter-leaves s] 
  [stream-filter [lambda [x] [equal? 1 [cadar x]]] s]]



[define [mk-table-stream xht yht table_id dbf]
  [let* [[xpht [xbrt-enc-xml xht]]
         [ypht [xbrt-enc-xml yht]]
         [xhth [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx xpht]]]]
         [yhth [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx ypht]]]]
         [ixhb [mk-header_enc-xml xpht table_id]]
         [xhb  [shift-block yhth 0 ixhb]]
         [iyhb [mk-header_enc-xml ypht table_id]]
         [yhb  [stream-map transpose-cells [shift-block xhth 0 iyhb]]]
         [db   [shift-block yhth xhth
               [mk-table-stream-block dbf table_id
                 [cell-sort 1 [cell-filter-leaves xhb]]
                 [cell-sort 2 [cell-filter-leaves yhb]]]]]
         ]  
    [stream-map [lambda [x] [cons [list [car x] table_id] [drop x 1]]]
    [set-block  
    [stream-append xhb yhb db]]]]]


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
      [stream-append diff-css table-div-css [mk-table-css [stream html_table_stream]]]]
    ]]

[define [html-table-test]
  [file<-html_table "html-table-test2" [mk-table-stream [xml<-nlist xt-nlist][xml<-nlist yt-nlist] "tf" test-df]]]