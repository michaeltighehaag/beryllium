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


[define [mk-table-stream-block df p row-strm col-strm]
  [let [[nrs [stream-zip row-strm  [stream-from 1]]]
        [ncs [stream-zip col-strm [stream-from 1]]]]
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

[define [mk-table-header-block t tid ]
  [stream<-list
    [map [lambda [x] [cons [car x] [cons tid [cdr x]]]]
         [walk-header-tree t 0]]]]

[define [shift-block xs ys s]
  [stream-map
    [lambda [z] [list [car z] [cadr z]
                  [+ xs [list-ref z 2]] [list-ref z 3]
                  [+ ys [list-ref z 4]] [list-ref z 5]]]  s]]

[define [flip-block-x s]
  [let [[xmin [list-ref [[stream-argext < [lambda [z] [list-ref z 4]]] s] 4]]
        [xmax [list-ref [[stream-argext > [lambda [z] [+ [list-ref z 4] [sub1 [list-ref z 5]]]]] s] 4]]]
    [stream-map 
      [lambda [z]
        [list [list-ref z 0] [list-ref z 1] [list-ref z 2] [list-ref z 3]
              [+ 1 xmin xmax [- [+ [list-ref z 4] [list-ref z 5]]]]
              [list-ref z 5] ]] s]]]

[define [flip-block-y s]
  [let [[ymin [list-ref [[stream-argext < [lambda [z] [list-ref z 2]]] s] 2]]
        [ymax [list-ref [[stream-argext > [lambda [z] [+ [list-ref z 2] [sub1 [list-ref z 3]]]]] s] 2]]]
    [stream-map 
      [lambda [z]
        [list [list-ref z 0] [list-ref z 1] 
              [+ 1 ymin ymax [- [+ [list-ref z 2] [list-ref z 3]]]]
              [list-ref z 3]
              [list-ref z 4] [list-ref z 5] ]] s]]]

[define [stream-argext r f]
  [lambda [s] [stream-argext-help r f [void] s]]]

[define [stream-argext-help r f c s]
  [if [stream-null? s] c
    [if [equal? c [void]]
      [stream-argext-help r f [stream-car s] [stream-cdr s]]
      [if [r [f [stream-car s]] [f c]]
        [stream-argext-help r f [stream-car s] [stream-cdr s]]
        [stream-argext-help r f c [stream-cdr s]]]]]]
    
[define [set-block s]
  [stream-map
    [lambda [z] [cons [car z] [cons [cadr z] [map number->string [cddr z]]]]] s]]

[define [rx-pos-filter s n]
  [lambda [x] [regexp-match [regexp s] [list-ref x n]]]]

[define [val-pos-filter v n]
  [lambda [x] [equal? v [list-ref x n]]]]

[define [pred-pos-sort pred n]
  [lambda [x y] [pred [list-ref x n] [list-ref y n]]]]

[define str-as-num<? [lambda [x y] [< [string->number x][string->number y]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define test-rs [stream-map [lambda [x] [pad x 2]] [stream-range 21 30]]]
[define test-cs [stream-map [lambda [x] [pad x 2]] [stream-range 70 81]]]

[define test-header
  [list "a"
    [list [list "s1" [html:br [list] [list]] "new line"] [list "x1"] [list "x2"] [list "x3"]]
    [list "s2" [list "y1"] [list "y2" [list "z1"][list "z2"]] [list "y3"]]]]

[define [test-df p x y xh yh] [list [cat "h" xh "v" yh "?"] p x 1 y 1]]

[define [transpose-cells l]
  [list [car l] [cadr l] [list-ref l 4] [list-ref l 5] [list-ref l 2][list-ref l 3]]]

[define [html-table-test]                                            
  [let* [[ts2 [shift-block 0 4 [mk-table-header-block test-header "tf"]]]
         [ts2b [flip-block-x ts2]]
         [ts3 [stream-map transpose-cells ts2]]
         [ts4 [stream-map car [stream-sort [pred-pos-sort < 4] [stream-filter [val-pos-filter 1 5] ts2b ]]]]
         [ts5 [stream-map car [stream-sort [pred-pos-sort < 2] [stream-filter [val-pos-filter 1 3] ts3 ]]]]
         [ts6 [shift-block 4 4 [mk-table-stream-block test-df "tf" ts4 ts5]]]
         [tf [set-block  [stream-append ts2b ts3 ts6]]]
         [name "html-table-test"]
         [out-file-html [open-output-file [cat "../" name ".html"] #:exists 'replace]]]
    [disp-stream ts2]
    [disp-stream ts3]
    [disp-stream ts4]
    [disp-stream ts5]
    [disp-stream ts6]
    [newline]
    [displayln [[stream-argext < [lambda [z] [list-ref z 4]]] ts2]]
    [displayln [[stream-argext > [lambda [z] [list-ref z 4]]] ts2]]
    [newline]
    [disp-stream tf]
    [write-html
      [mk-html [list] [list]
        [list [mk-css-link [cat "./" name ".css"]]]
        [list]
        [list
          [list [html:p [list] [list
                [html:span [list [list 'class "diff-mod"]] [list "T"]]
                "est"
                [html:span [list [list 'class "diff-ins"]] [list "ing"]]]]]
          [mk-table-divs tf]
          ]]
      out-file-html]
    [close-output-port out-file-html]
    [file<-stream [cat "../" name ".css"] displayln
      [stream-append diff-css table-div-css [mk-table-css [stream tf ]]]]
    ]]


;[html-table-test]
