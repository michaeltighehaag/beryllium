#lang racket
[require html-writing]
[require "common.rkt"]
[require "stream.rkt"]
[require racket/require
  [path-up "XBRT.rkt"]]
[provide [all-from-out html-writing]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Provided functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[provide [all-defined-out]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


[define [html:html al el] [cons 'html [cons [cons '@ al] el]]]
[define [html:head al el] [cons 'head [cons [cons '@ al] el]]]
[define [html:meta al el] [cons 'meta [cons [cons '@ al] el]]]
[define [html:title al el] [cons 'title [cons [cons '@ al] el]]]
[define [html:link al el] [cons 'link [cons [cons '@ al] el]]]
[define [html:script al el] [cons 'script [cons [cons '@ al] el]]]
[define [html:style al el] [cons 'style [cons [cons '@ al] el]]]
[define [html:body al el] [cons 'body [cons [cons '@ al] el]]]
[define [html:p al el] [cons 'p [cons [cons '@ al] el]]]
[define [html:div al el] [cons 'div [cons [cons '@ al] el]]]
[define [html:span al el] [cons 'span [cons [cons '@ al] el]]]
[define [html:h3 al el] [cons 'h3 [cons [cons '@ al] el]]]
[define [html:table al el] [cons 'table [cons [cons '@ al] el]]]
[define [html:tr al el] [cons 'tr [cons [cons '@ al] el]]]
[define [html:td al el] [cons 'td [cons [cons '@ al] el]]]
[define [html:a al el] [cons 'a [cons [cons '@ al] el]]]
[define [html:br al el] [cons 'br [cons [cons '@ al] el]]]


[define [mk-html html-attr-list head-attr-list head-list body-attr-list body-list]
  [html:html html-attr-list [list
      [html:head head-attr-list head-list]
      [html:body body-attr-list body-list]
  ]]]

[define [mk-css-link loc]
  [html:link
    [list [cons 'rel "stylesheet"][cons 'type "text/css"][cons 'href loc]]
    [list]]]


[define diff-css [stream
".diff-mod {"
"  background-color:rgb(255,191,191);"
"}"
".diff-gap {"
"  background-color:rgb(255,127,127);"
"}"
]]    


[define table-div-css  
  [stream
".grid-container {"
"  display: inline-grid;"
"  grid-gap: 10px;"
"  background-color: rgb(159, 159, 255);"
"  padding: 10px 10px 10px 10px; "
"  border: 0px solid rgba(0, 0, 0, 0.8);"
"  text-align: center;"
"}"
".grid-container > div {"
"  background-color: rgb(223, 223, 255);"
"  padding: 10px 10px 10px 10px;"
"  font-size: 20px;"
"}"
".colhead { background-color: rgb(219, 219, 255); "
"           writing-mode: vertical-rl; "
"           transform: rotate(180deg); "
"           text-align:left; "
"}"
".rowhead { "
"  background-color: rgb(159, 159, 255);"
"}"
]]
     
[define [mk-table-div-id-css l] [stream
  [cat "#cell-" [list-ref l 1]  "-" [list-ref l 4] "-" [list-ref l 2] " {"]
  [cat "  grid-area: " [list-ref l 4] " / " [list-ref l 2] " / span " [list-ref l 5] " / span " [list-ref l 3] ";"]
  [cat "}"]]]

[define [mk-table-div-element l]
  [html:div [list [cons 'id [cat "cell-" [list-ref l 1]  "-" [list-ref l 4] "-" [list-ref l 2]]]]
            [list [list-ref l 0]]]]

[define [mk-table-css dss]
  [stream-concat
  [stream-map [lambda [x] [stream-concat [stream-map mk-table-div-id-css x] ]]
  dss]]]            

[define [mk-table-divs ds]
  [html:div
    [list [cons 'class "grid-container"]]
    [list<-stream [stream-map mk-table-div-element ds]]]]


[define [test-df p x y xh yh] [list [list [cat "h" xh "v" yh "?"]] p x 1 y 1 1]]

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
  [stream-map [lambda [z] [cons [car z] [cons [cadr z] [map number->string [cddr z]]]]]
  [stream-map disp-cell s]]]




[define xcminf [let [[f [lambda [z] [list-ref z 2]]]] [lambda [s] [f [[stream-argext < f] s]]]]]
[define xcmaxf [let [[f [lambda [z] [+ [list-ref z 2] [sub1 [list-ref z 3]]]]]] [lambda [s] [f [[stream-argext > f] s]]]]]

[define [shift-block xs ys s]
  [stream-map
    [lambda [z] [list [car z] [cadr z]
                  [+ xs [list-ref z 2]] [list-ref z 3]
                  [+ ys [list-ref z 4]] [list-ref z 5] ]]  s]]

[define [flip-block-x s]
  [let [[xmin [xcminf s]][xmax [xcmaxf s]]]
    [stream-map [lambda [z]
    [append [take z 2] [list [+ 1 xmin xmax [- [+ [list-ref z 2][list-ref z 3]]]] [list-ref z 3][list-ref z 4][list-ref z 5] ]]] s]]]

[define [transpose-cells l]
  [list [car l] [cadr l] [list-ref l 4] [list-ref l 5] [list-ref l 2][list-ref l 3] ]]

[define [cell-sort d s]
  [let [[p [* 2 d]]] [stream-sort [lambda [x y] [< [list-ref x p] [list-ref y p]]] s]]]

[define [cell-filter-leaves s] 
  [stream-filter [lambda [x] [equal? 1 [cadar x]]] s]]



[define [mk-html-table xht yht table_id dbf]
  [let* [[xpht [xbrt-enc-xml xht]]
         [ypht [xbrt-enc-xml yht]]
         [xhth [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx xpht]]]]
         [yhth [x_val_def-val_max_height [gnx_def-val_x [xbrt-gnx ypht]]]]
         [ixhb [mk-header_enc-xml xpht table_id]]
         [xhb  [shift-block yhth 0 ixhb]]
         [iyhb [mk-header_enc-xml ypht table_id]]
         [yhb  [stream-map transpose-cells [flip-block-x [shift-block xhth 0 iyhb]]]]
         [db   [shift-block yhth xhth
               [mk-table-stream-block dbf table_id
                 [cell-sort 1 [cell-filter-leaves xhb]]
                 [cell-sort 2 [cell-filter-leaves yhb]]]]]]
    [set-block  [stream-append xhb yhb db]]]]


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
  [file<-html_table "html-table-test2" [mk-html-table xt yt "tf" test-df]]]