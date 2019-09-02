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
      
[define [mk-table-div-id-css p l] [stream
  [cat "#cell-" p "-" [list-ref l 1] "-" [list-ref l 3] " {"]
  [cat "  grid-area: " [list-ref l 1] " / " [list-ref l 3] " / span " [list-ref l 2] " / span " [list-ref l 4] ";"] 
  [cat "}"]]]

[define [mk-table-div-element p l]
  [html:div [list [cons 'id [cat "cell-" p "-" [list-ref l 1] "-" [list-ref l 3]]]]
            [list [list-ref l 0]]]]


[define [mk-table-stream df row-start row-strm col-start col-strm]
  [let [[nrs [stream-zip row-strm [stream-map [lambda [x] [number->string x]] [stream-from row-start]]]]
        [ncs [stream-zip col-strm [stream-map [lambda [x] [number->string x]] [stream-from col-start]]]]]
  [stream-concat
  [stream-map
    [lambda [x]
      [stream-map
        [lambda [y]  [df [cadr x] [cadr y] [car x] [car y]] ]
        nrs]]
    ncs]]]]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define [test-css dl] 
  [stream-append
    diff-css
    table-div-css
    [stream-concat [stream-map [lambda [x] [mk-table-div-id-css "t1" x]] [car dl]] ]
    [stream-concat [stream-map [lambda [x] [mk-table-div-id-css "t2" x]] [cadr dl]]]
    ]]
 
[define [test-divs p d]
  [list<-stream
  [stream-map [lambda [x] [mk-table-div-element p x]] d]]]

[define [test-df x y xh yh] [list 
                             [cat "h" xh "v" yh "?"] x "1" y "1"]]

[define test-rs [stream-map [lambda [x] [pad x 2]] [stream-range 21 30]]]
[define test-cs [stream-map [lambda [x] [pad x 2]] [stream-range 70 81]]]

[define [html-table-test1]                                            
  [let* [[ts1 [mk-table-stream test-df  1 test-rs 1 test-cs]]
         [ts2 [stream<-list [map [lambda [x] [cons [car x] [map number->string [cdr x]]]]
                                 [walk-header-tree rl 0]]
                           ]]
         [name "html-table-test1"]
         [out-file-html
           [open-output-file [cat "../" name ".html"] #:exists 'replace]]]
    [write-html
      [mk-html
        [list]
        [list]
        [list [mk-css-link [cat "./" name ".css"]]]
        [list]
        [list
          [list [html:p [list] [list
                [html:span [list [list 'class "diff-mod"]] [list "T"]]
                "est"
                [html:span [list [list 'class "diff-ins"]] [list "ing"]]]]]
          [html:div
            [list [cons 'class "grid-container"]]
            [test-divs "t1" ts1]]
          [list [html:p [list] [list "est"]]]
          [html:div
            [list [cons 'class "grid-container"]]
            [test-divs "t2" ts2]]
          ]]
      out-file-html]
    [close-output-port out-file-html]
    [file<-stream [cat "../" name ".css"] displayln [test-css [list ts1 ts2]]]

    ]]


[define rl [list "a"
                 [list "s1" [list "x1"] [list "x2"] [list "x3"]]
                 [list "s2" [list "y1"] [list "y2" [list "z1"][list "z2"]] [list "y3"]]]]


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


[html-table-test1]
