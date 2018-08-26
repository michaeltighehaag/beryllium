#lang racket


[require html-writing]

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
[define [html:body al el] [cons 'body [cons [cons '@ al] el]]]
[define [html:p al el] [cons 'p [cons [cons '@ al] el]]]


[define [mk-html-page head-list body-list]
  [html:html [list]
              [list
      [html:head [list] head-list]
      [html:body [list] body-list]
  ]]]



