#lang racket
[require "common.rkt"]
[require "stream.rkt"]
[require "XBRT.rkt"]
[require "time.rkt"]
[require "XML.rkt"]
[require "HTML.rkt"]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Provided functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[provide [all-defined-out]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SVG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   functions to return svg sxml   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [svg:svg w h al el]
  [cons 'svg [cons [cons '@ [append
    [list [list 'version "1.1"]]
    [list [list 'viewBox [cat "0 0 " [number->string [* w 128]] " " [number->string [* h 128]]]]
          [list 'width  [cat [number->string w] "in"]]
          [list 'height [cat [number->string h] "in"]]]
    [list [list 'xmlns "http://www.w3.org/2000/svg"]]
     al]] el]]]

[define [svg:path fc sc sw d] 
  [cons 'path [cons [cons '@ [list 
    [list 'fill fc]
    [list 'stroke sc]
    [list 'stroke-width [number->string sw]]
    [list 'd d]
    [list 'stroke-linejoin "round"]
    ]] [list]]]]

[define [svg:text x y s t] 
  [cons 'text [cons [cons '@ [list
    [list 'x [number->string x]]
    [list 'y [number->string y]]
    [list 'font-size [number->string s]]
    ]] [list t]]]]

[define [svg:rect x y w h r fc sc sw el] 
  [cons 'rect [cons [cons '@ [list
    [list 'x [number->string x]]
    [list 'y [number->string y]]
    [list 'width  [number->string w]]
    [list 'height [number->string h]]
    [list 'rx [number->string r]]
    [list 'fill fc]
    [list 'stroke sc]
    [list 'stroke-width [number->string sw]]
    ]] el]]]

[define [svg:circle x y r fc sc sw el] 
  [cons 'circle [cons [cons '@ [list 
    [list 'cx[number->string  x]]
    [list 'cy [number->string y]]
    [list 'r [number->string r]]
    [list 'fill fc]
    [list 'stroke sc]
    [list 'stroke-width [number->string sw]]
    ]] el]]]

[define [svg:animate attr vs dur] 
  [cons 'animate [cons [cons '@ [list
    [list 'attributeName attr]
    [list 'values vs]
    [list 'dur dur]
    [list 'repeatCount "indefinite"]
    ]] [list]]]]

[define [svg:animateTransform vs dur] 
  [cons 'animateTransform [cons [cons '@ [list
    [list 'attributeName "transform"]
    [list 'attributeType "XML"]
    [list 'type "translate"]
    [list 'values vs]
    [list 'dur [cat [number->string dur] "s"]]
    [list 'repeatCount "indefinite"]
    ]] [list]]]]
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [rgba r g b a]
  [cat "rgb(" [number->string r] ", "
              [number->string g] ", "
              [number->string b] ", "
              [number->string a] ")"]]

[define [mk-abs-mv x y]
  [cat "M " [number->string x] " "
            [number->string y] " "]]
[define [mk-mv x y]
  [cat "m " [number->string x] " "
            [number->string y] " "]]
[define [mk-abs-line x y]
  [cat "L " [number->string x] " "
            [number->string y] " "]]
[define [mk-line x y]
  [cat "l " [number->string x] " "
            [number->string y] " "]]
[define [mk-arc r a o x y]
  [cat "a " [number->string r] " "
            [number->string r] " "
            [number->string a] " "
            "0" " "
            [number->string o] " "
            [number->string x] "," [number->string y] " "]]

[define [invc n d]
  [- 255 [round [* [/ n d] 255]]]]
[define [mk-red n d [o 1.0]]
  [rgba 255 [invc n d] [invc n d] o]]
[define [mk-yellow n d [o 1.0]]
  [rgba 255 255 [invc n d] o]]
[define [mk-green n d [o 1.0]]
  [rgba [invc n d] 255 [invc n d] o]]
[define [mk-cyan n d [o 1.0]]
  [rgba [invc n d] 255 255 o]]
[define [mk-blue n d [o 1.0]]
  [rgba [invc n d] [invc n d] 255 o]]
[define [mk-magenta n d [o 1.0]]
  [rgba 255 [invc n d] 255 o]]
[define [mk-black n d [o 1.0]]
  [rgba [invc n d] [invc n d] [invc n d] o]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


[define tpv [apply cat [list
  [mk-abs-mv 600 400]
  [mk-line 100 10]
  [mk-line 20 200]
  ""]
  ]]
[define st
  [svg:svg 12 6 [list] [list 
  [svg:text 40 45 30 "testingtext"]
  [svg:path [mk-black 1 1 0.0] [mk-black 2 4 1.0] 10 tpv] 
  [svg:rect 120 120 90 90 10 [mk-red 2 4] [mk-red 4 4] 10 [list]]
  [svg:rect 220 120 90 90 10 [mk-yellow 2 4] [mk-yellow 4 4] 10 [list]]
  [svg:rect 320 120 90 90 10 [mk-green 2 4] [mk-green 4 4] 10 [list]]
  [svg:rect 420 120 90 90 10 [mk-cyan 2 4] [mk-cyan 4 4] 10 [list]]
  [svg:rect 520 120 90 90 10 [mk-blue 2 4] [mk-blue 4 4] 10 [list]]
  [svg:rect 620 120 90 90 10 [mk-magenta 2 4] [mk-magenta 4 4] 10 [list]]
  [svg:rect 120 220 90 90 10 [mk-yellow 4 4] [mk-black 4 4] 10 [list]]
  [svg:rect 220 220 90 90 10 [mk-yellow 3 4] [mk-black 3 4] 10 [list]]
  [svg:rect 320 220 90 90 10 [mk-yellow 2 4] [mk-black 2 4] 10 [list]]
  [svg:rect 420 220 90 90 10 [mk-yellow 1 4] [mk-black 1 4] 10 [list]]
  [svg:rect 120 420 90 90 10 [mk-magenta 4 4] [mk-blue 4 4] 10 [list]]
  [svg:rect 220 420 90 90 10 [mk-magenta 3 4] [mk-blue 3 4] 10 [list]]
  [svg:rect 320 420 90 90 10 [mk-magenta 2 4] [mk-blue 2 4] 10 [list]]
  [svg:rect 420 420 90 90 10 [mk-magenta 1 4] [mk-blue 1 4] 10 [list]]
  [svg:circle 100 100 40 [mk-red 3 4] [mk-green 3 4] 10
    [list [svg:animate "r" "20;40;20" 2.5]]]
  ]
  ]]

[define [svg-test]
  [file<-string "svg-test-01.svg" [xml-string<-sxml st] 'replace]]
                             

