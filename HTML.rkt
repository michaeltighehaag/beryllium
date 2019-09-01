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
      
[define [mk-table-div-id-css l] [stream
  [cat "#cell" [list-ref l 0] "-" [list-ref l 1] " {"]
  [cat "  grid-area: " [list-ref l 0] " / " [list-ref l 1] " / span 1 / span 1;"] 
  [cat "}"]]]

[define [mk-table-div-element l]
  [html:div [list [cons 'id [cat "cell" [list-ref l 0] "-" [list-ref l 1]]]]
            [list [list-ref l 2]]]]


[define [mk-table-stream df row-start row-strm col-start col-strm]
  [let [[nrs [stream-zip [stream-map [lambda [x] [number->string x]] [stream-from row-start]] row-strm]]
        [ncs [stream-zip [stream-map [lambda [x] [number->string x]] [stream-from col-start]] col-strm]]]
  [stream-concat
  [stream-map
    [lambda [x]
      [stream-map
        [lambda [y] [list [car x] [car y] [df [cadr x] [cadr y]] ]]
        nrs]]
    ncs]]]]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define [test-css d] 
  [stream-append
    diff-css
    table-div-css
    [stream-concat [stream-map mk-table-div-id-css d]] ]]
 
[define [test-divs d]
  [list<-stream
  [stream-map mk-table-div-element d]]]

[define [test-df x y] [list x y
                             [cat "h" x "v" y "?"]]]

[define test-rs [stream-map [lambda [x] [pad x 2]] [stream-range 21 30]]]
[define test-cs [stream-map [lambda [x] [pad x 2]] [stream-range 30 41]]]

[define [html-table-test]                                            
  [let* [[ts [mk-table-stream test-df  1 test-rs 1 test-cs]]
         [name "html-table-test"]
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
            [test-divs ts]]]] out-file-html]
    [close-output-port out-file-html]
    [file<-stream [cat "../" name ".css"] displayln [test-css ts]]

    ]]



[define [html-table-test2]                                            
  [let* [[ts2 [stream [list "1" "3" "head"]  [list "2" "3" "head2"]
                      [list "3" "1" "rh"][list "3" "2" "rh2"]]]
         [ts1 [mk-table-stream test-df  3 test-rs 3 test-cs]]
         [ts [stream-append ts2 ts1 ]]
         [name "html-table-test2"]
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
            [test-divs ts]]]] out-file-html]
    [close-output-port out-file-html]
    [file<-stream [cat "../" name ".css"] displayln [test-css ts]]

    ]]

[define cl [list "a"
                 [list "s1" [list "x1"] [list "x2"] [list "x3"]]
                 [list "s2" [list "y1"] [list "y2" [list "z1"]] [list "y3"]]]]
[define rl [list "a"
                 [list "s1" [list "x1"] [list "x2"] [list "x3"]]
                 [list "s2" [list "y1"] [list "y2" [list "z1"][list "z2"]] [list "y3"]]]]

[define [rmk-cell i l]
  [if [not [null? l]]
    [displayln [list i [leaf-count [car l]] [caar l]]]
    [rmk-cell [+ i [leaf-count l]] [cdr l]]]]

[define [leaf-count t]
  [if [null? [cdr t]] 1
    [apply + [map leaf-count [cdr t]]]]]
    
[define [tree-depth t]
  [if [null? [cdr t]] 1
    [+ 1 [apply max [map tree-depth [cdr t]]]]]]
    
[define [walk-level t lev]
  [if [equal? 1 lev] [list [car t] lev [leaf-count t]]
    [apply list [map [lambda [x] [walk-level x [sub1 lev]]] [cdr t]]]]] 

[define [nw t r cd td]
  [if [null? [cdr t]] [cons [list [car t] cd [- td [tree-depth t]]] r]
    [void]]
]


[define [walk-header-tree t [p 0]]
  [wht-h t [list] 1 1 [add1  [tree-depth t]] p]]

[define [wht-h t r lev i d p]
  [let* [[slev [if [equal? p 1] 1 [- d [+ [sub1 lev] [tree-depth t]]]]]
         [n [cons [list [car t] lev slev i [leaf-count t]] r]]]
    [if [null? [cdr t]] n [wht-l [cdr t] n [+ slev lev] i d p]]]]

[define [wht-l l r lev i d p]
  [let [[n [wht-h [car l] r lev i d p]]]
    [if [null? [cdr l]] n [wht-l [cdr l] n lev [+ i [leaf-count [car l]]] d p]]]]

[displayln [walk-header-tree rl]]
           
[displayln [walk-header-tree rl 1]]
       
[walk-level rl 1]
[walk-level rl 2]
[walk-level rl 3]
[walk-level rl 4]
[define [html-table-test3]                                            
  [let* [[ts2 [stream<-list [map [lambda [x] [list [number->string [cadr x]]
                                                   [number->string [cadddr x]][car x]]] [walk-header-tree rl 0]]
                           ]]
         [ts1 [mk-table-stream test-df  3 test-rs 3 test-cs]]
         [ts [stream-append ts2 ]]
         [name "html-table-test4"]
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
            [test-divs ts]]]] out-file-html]
    [close-output-port out-file-html]
    [file<-stream [cat "../" name ".css"] displayln [test-css ts]]

    ]]
[html-table-test3]