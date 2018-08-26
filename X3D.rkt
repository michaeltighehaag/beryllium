#lang racket
;[require racket/require [path-up "stream.rkt"]]
[require racket/require [path-up "XML.rkt"]]
[require racket/require [path-up "HTML.rkt"]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Provided functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[provide [all-defined-out]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; x3dom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [mk-x3dom-head title x3dom-js x3dom-css]
  [list
    [html:meta [list [list 'http-equiv "X-UA-Compatible"] [list 'content "chrome=1"]][list]] 
    [html:title [list] [list title]]
    [html:script [list [list 'type "text/javascript"] [list 'src x3dom-js]][list]]
    [html:link [list [list 'rel "stylesheet"] [list 'type "text/css"] [list 'href x3dom-css]][list]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   functions to return x3d sxml   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [x3d:Shape al el] [cons 'Shape [cons [cons '@ al] el]]]
[define [x3d:Transform al el] [cons 'Transform [cons [cons '@ al] el]]]
[define [x3d:Sphere al el] [cons 'Sphere [cons [cons '@ al] el]]]
[define [x3d:PointSet al el] [cons 'PointSet [cons [cons '@ al] el]]]
[define [x3d:Coordinate al el] [cons 'Coordinate [cons [cons '@ al] el]]]
[define [x3d:Color al el] [cons 'Color [cons [cons '@ al] el]]]
[define [x3d:IndexedLineSet al el] [cons 'IndexedLineSet [cons [cons '@ al] el]]]
[define [x3d:IndexedFaceSet al el] [cons 'IndexedFaceSet [cons [cons '@ al] el]]]
[define [x3d:Collision al el] [cons 'Collision [cons [cons '@ al] el]]]
[define [x3d:Scene al el] [cons 'Scene [cons [cons '@ al] el]]]
[define [x3d:HEAD al el] [cons 'HEAD [cons [cons '@ al] el]]]
[define [x3d:X3D al el] [cons 'X3D [cons [cons '@ al] el]]]
[define [x3d:Appearance al el] [cons 'Appearance [cons [cons '@ al] el]]]
[define [x3d:Material al el] [cons 'Material [cons [cons '@ al] el]]]

[define [app c]            
  [x3d:Appearance [list] [list
    [x3d:Material [list ;      [list 'DEF "Bx_Material"]
      [list 'diffuseColor c]
      [list 'specularColor "0.5 0.5 0.5"]
      [list 'emissiveColor "0.0 0.0 0.0"]
      [list 'ambientIntensity "0.0"]
      [list 'shininess "0.2"]
      [list 'transparency "0.0"]] [list]] ]] ]

[define [mk-x3d scene-list] 
  [x3d:X3D [list
      [list 'xmlns:xsd "http://www.w3.org/2001/XMLSchema-instance" ]
      [list 'version "3.0" ]
      [list 'profile "Immersive" ]
      [list 'xsd:noNamespaceSchemaLocation "http://www.web3d.org/specifications/x3d-3.0.xsd"]
      [list 'x "0px"] [list 'y "0px"]
      [list 'width "960px"] [list 'height "540px"] ]
    [list
      [x3d:HEAD [list][list]]
      [x3d:Scene [list
          [list 'render "true"]
          [list 'bboxCenter "0,0,0"]
          [list 'bboxSize "-1,-1,-1"]
          [list 'pickMode "idBuf"]
          [list 'doPickPass "true"]]
        [list
          [x3d:Collision [list [list 'enabled "false"]] scene-list] ]] ]] ]

[define [sphere r p c]
  [x3d:Transform [list [list 'translation p ]] [list
    [x3d:Shape [list] [list
      [app c]  
      [x3d:Sphere [list [list 'radius r]] [list]] ]] ]] ]

[define [points l]
  [x3d:Transform [list] [list
    [x3d:Shape [list] [list
      [x3d:PointSet [list] [list
        [x3d:Coordinate [list [list 'point l]] [list]] ]] ]] ]] ]

[define [lines i p c]
  [x3d:Transform [list [list 'translation "4.5 0 0"]] [list
    [x3d:Shape [list] [list 
       [x3d:IndexedLineSet 
         [list [list 'colorPerVertex "false"] 
               [list 'coordIndex i]]
         [list [x3d:Coordinate [list [list 'point p]] [list]]
               [x3d:Color [list [list 'color c]][list]] ]] ]] ]] ]

[define [face i p c]
  [x3d:Transform [list] [list 
    [x3d:Shape [list] [list 
      [app  c]    
      [x3d:IndexedFaceSet
        [list [list 'solid "true" ]
              [list 'coordIndex i]]
        [list [x3d:Coordinate [list [list 'point p]][list]] ]] ]] ]] ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


[define x3d-test-list [list
  [sphere "0.9" "-0.315570 -2.552557 0.743604"  "1 1 0 "]
  [points "0.0 -7.0 -1.0 -1.75 -7.0 -0.5 -4.0 -7.0 0.5 -5.0 -6.5 1.5"]
  [lines "0 1 2 3 0 -1 4 5 6 7 4 -1 0 4 -1 1 5 -1 2 6 -1 3 7 -1"
         "-1 1 1 1 1 1 1 1 -1 -1 1 -1 -1 -1 1 1 -1 1 1 -1 -1 -1 -1 -1"
         "1 0.058824 0.117647 0.878431 0.447059 0 1 0.992157 0.141176 0.101961 0.721569 0 0.12549 0 0.901961 0.878431 0 0.843137 0.294118 0 0.341176 1 0.980392 0.992157 1 1 1"]
  [face "0 1 2 3 -1, 4 7 6 5 -1, 0 4 5 1 -1, 1 5 6 2 -1, 2 6 7 3 -1, 4 0 3 7 -1, "
        "1.000000 1.000000 -1.000000, 1.000000 -1.000000 -1.000000, -1.000000 -1.000000 -1.000000, -1.000000 1.000000 -1.000000, 1.000000 0.999999 1.000000, 0.999999 -1.000001 1.000000, -1.000000 -1.000000 1.000000, -1.000000 1.000000 1.000000, "
        " 0.10 0.10 0.50"]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [write-x3d name scene-list]                                            
  [let [[out-file [open-output-file [string-append name ".x3d"] #:exists 'replace]]]
    [srl:sxml->xml  [list [mk-x3d scene-list]] out-file]
    [close-output-port out-file]]]

[define [write-x3d-html name x3dom-js x3dom-css scene-list]                                            
  [let [[out-file [open-output-file [string-append name ".html"] #:exists 'replace]]]
    [write-html [mk-html-page [mk-x3dom-head "x3d-test" x3dom-js x3dom-css] [list [mk-x3d scene-list]]] out-file]
    [close-output-port out-file]]]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


