#lang racket
[require html-writing]
[require "common.rkt"]
[require "stream.rkt"]

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
".diff-ins {"
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
  [cat "#cell-" [list-ref l 1]  "-" [list-ref l 2] "-" [list-ref l 4] " {"]
  [cat "  grid-area: " [list-ref l 2] " / " [list-ref l 4] " / span " [list-ref l 3] " / span " [list-ref l 5] ";"] 
  [cat "}"]]]

[define [mk-table-div-element l]
  [html:div [list [cons 'id [cat "cell-" [list-ref l 1]  "-" [list-ref l 2] "-" [list-ref l 4]]]]
            [list [list-ref l 0]]]]

[define [mk-table-css dss] 
  [stream-concat
  [stream-map [lambda [x] [stream-concat [stream-map mk-table-div-id-css x] ]]
  dss]]]            

[define [mk-table-divs ds]
  [html:div
    [list [cons 'class "grid-container"]]
    [list<-stream [stream-map mk-table-div-element ds]]]]


[define [mk-table-stream-block df p row-start row-strm col-start col-strm]
  [let [[nrs [stream-zip row-strm [stream-map [lambda [x] [number->string x]] [stream-from row-start]]]]
        [ncs [stream-zip col-strm [stream-map [lambda [x] [number->string x]] [stream-from col-start]]]]]
  [stream-concat
  [stream-map
    [lambda [x]
      [stream-map
        [lambda [y]  [df p [cadr x] [cadr y] [car x] [car y]] ]
        nrs]]
    ncs]]]]


[define [walk-header-tree t [p 0]]
  [wht-h t [list] 1 1 [add1  [tree-depth t]] p]]

[define [leaf-count t]
  [if [null? [cdr t]] 1
    [apply + [map leaf-count [cdr t]]]]]
    
[define [tree-depth t]
  [if [null? [cdr t]] 1
    [+ 1 [apply max [map tree-depth [cdr t]]]]]]

[define [wht-h t r lev i d p]
  [let* [[slev [if [equal? p 1] 1 [- d [+ [sub1 lev] [tree-depth t]]]]]
         [n [cons [list [car t] lev slev i [leaf-count t]] r]]]
    [if [null? [cdr t]] n [wht-l [cdr t] n [+ slev lev] i d p]]]]

[define [wht-l l r lev i d p]
  [let [[n [wht-h [car l] r lev i d p]]]
    [if [null? [cdr l]] n [wht-l [cdr l] n lev [+ i [leaf-count [car l]]] d p]]]]

[define [mk-table-header-block t]
  [stream<-list
    [map [lambda [x] [cons [car x] [cons "t2" [map number->string [cdr x]]]]]
         [walk-header-tree t 0]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define test-rs [stream-map [lambda [x] [pad x 2]] [stream-range 21 30]]]
[define test-cs [stream-map [lambda [x] [pad x 2]] [stream-range 70 81]]]
[define test-header
  [list "a"
    [list [list "s1" [html:br [list] [list]] "new line"] [list "x1"] [list "x2"] [list "x3"]]
    [list "s2" [list "y1"] [list "y2" [list "z1"][list "z2"]] [list "y3"]]]]

[define [test-df p x y xh yh] [list [cat "h" xh "v" yh "?"] p x "1" y "1"]]


[define [html-table-test1]                                            
  [let* [[ts1 [mk-table-stream-block test-df "t1" 1 test-rs 1 test-cs]]
         [ts2 [mk-table-header-block test-header]]
         [name "html-table-test1"]
         [out-file-html [open-output-file [cat "../" name ".html"] #:exists 'replace]]]
    [disp-stream ts1]
    [disp-stream ts2]
    [write-html
      [mk-html [list] [list]
        [list [mk-css-link [cat "./" name ".css"]]]
        [list]
        [list
          [list [html:p [list] [list
                [html:span [list [list 'class "diff-mod"]] [list "T"]]
                "est"
                [html:span [list [list 'class "diff-ins"]] [list "ing"]]]]]
          [mk-table-divs ts1]
          [list [html:p [list] [list "New table"]]]
          [mk-table-divs ts2]
          ]]
      out-file-html]
    [close-output-port out-file-html]
    [file<-stream [cat "../" name ".css"] displayln
      [stream-append diff-css table-div-css [mk-table-css [stream ts1 ts2]]]]
    ]]

[html-table-test1]
