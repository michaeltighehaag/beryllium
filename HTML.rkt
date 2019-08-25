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

[define diff-css [stream
".mc {"
"  background-color:rgb(255,219,219);"
"}"
]]          

[define [mk-dcss l] [stream
[cat "#id" [list-ref l 0] "-" [list-ref l 1] " {"]
[cat "  grid-area: " [list-ref l 0] " / " [list-ref l 1] " / span 1 / span 1;"] 
[cat "}"]]]

[define [test-css d] 
  [stream-append
    table-div-css
    [stream-concat [stream-map mk-dcss d]]
    ]]

[define [mk-div l]
  [html:div [list [cons 'id [cat "id" [list-ref l 0] "-" [list-ref l 1]]]] [list [list-ref l 2]]]]
  
[define [test-divs d]
  [list<-stream
  [stream-map mk-div d]]]

[define mtd [stream
[list "1" "1" "a"]
[list "1" "2" "b"]
[list "2" "1" "c"]
[list "2" "2" "d"]
]]


[define [html-table-test]                                            
  [let [[out-file-html [open-output-file [string-append "../html-table-test" ".html"] #:exists 'replace]]]
    [write-html [mk-html [list]
                         [list]
                         [list [html:link [list [cons 'rel "stylesheet"]
                                                [cons 'type "text/css"]
                                                [cons 'href "./html-table-test.css"]]
                                          [list]]]
                         [list]
                         [list
    [html:div
      [list [cons 'class "grid-container"]]
      [test-divs mtd]]]] out-file-html]
    [close-output-port out-file-html]
    [file<-stream "../html-table-test.css" displayln [test-css mtd]]

    ]]



