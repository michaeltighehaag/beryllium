#lang racket
[require "common.rkt"]
[require "stream.rkt"]
[require "XBRT.rkt"]
;[require "polydex.rkt"]
[require "time.rkt"]
[require "XML.rkt"]
[require "HTML.rkt"]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Provided functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[provide [all-defined-out]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; x3dom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [mk-x3dom-head title x3dom-js x3dom-css jq-js cust-js]
  [list
    [html:meta [list [list 'http-equiv "X-UA-Compatible"] [list 'content "chrome=1"]][list]] 
    [html:title [list] [list title]]
    [html:link [list [list 'rel "stylesheet"] [list 'type "text/css"] [list 'href x3dom-css]][list]]
    [html:script [list [list 'type "text/javascript"] [list 'src x3dom-js]][list]]
    [html:script [list [list 'type "text/javascript"] [list 'src jq-js]][list]]
    [html:script [list][list cust-js]]
  ]]

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
[define [x3d:Text al el] [cons 'Text [cons [cons '@ al] el]]]
[define [x3d:FontStyle al el] [cons 'FontStyle [cons [cons '@ al] el]]]
[define [x3d:Billboard al el] [cons 'Billboard [cons [cons '@ al] el]]]
[define [x3d:Collision al el] [cons 'Collision [cons [cons '@ al] el]]]
[define [x3d:Scene al el] [cons 'Scene [cons [cons '@ al] el]]]
[define [x3d:HEAD al el] [cons 'HEAD [cons [cons '@ al] el]]]
[define [x3d:X3D al el] [cons 'X3D [cons [cons '@ al] el]]]
[define [x3d:Appearance al el] [cons 'Appearance [cons [cons '@ al] el]]]
[define [x3d:Material al el] [cons 'Material [cons [cons '@ al] el]]]
[define [x3d:ColorInterpolator al el] [cons 'ColorInterpolator [cons [cons '@ al] el]]]
[define [x3d:TimeSensor al el] [cons 'TimeSensor [cons [cons '@ al] el]]]
[define [x3d:ROUTE al el] [cons 'ROUTE [cons [cons '@ al] el]]]
[define [x3d:Background al el] [cons 'Background [cons [cons '@ al] el]]]

[define [x3d:mk-elem elem-name al el] [cons elem-name [cons [cons '@ al] el]]]

[define [billboard p s t c]
  [x3d:Transform [list [list 'translation p] [list 'scale [cat [number->string s] " " [number->string s] " " [number->string s]]]] [list
    [x3d:Billboard [list [list 'axisOfRotation "0 0 0"]] [list
      [x3d:Shape [list] [list
        [x3d:Text [list [list 'string t]] [list
          [x3d:FontStyle [list [list 'family "SANS"]] [list]]]]
        [x3d:Appearance [list] [list
          [x3d:Material [list [list 'diffuseColor c] [list 'ambientIntensity "0"] [list 'shininess "0"]] [list]]]]
            ]]]]]]]

[define [appr-od c]            
  [x3d:Appearance [list] [list
    [x3d:Material [list 
      ;[list 'DEF "Bx_Material"]
      [list 'diffuseColor c]
      [list 'specularColor "0.5 0.5 0.5"]
      [list 'emissiveColor "0.0 0.0 0.0"]
      [list 'ambientIntensity "0.0"]
      [list 'shininess "0.2"]
      [list 'transparency "0.0"]] [list]] ]] ]

[define [appr-te c]
  [x3d:Appearance [list] [list
    [x3d:Material [list
      ;[list 'DEF mat-name]   
      [list 'diffuseColor  "0.0 0.0 0.0"]
      [list 'specularColor "0.0 0.0 0.0"]
      [list 'emissiveColor c]
      [list 'ambientIntensity "0.0"]
      [list 'shininess "0.0"]
      [list 'transparency "0.5"]] [list]]]]]

[define [mk-x3d scene-list] 
  [x3d:X3D [list
      [list 'xmlns:xsd "http://www.w3.org/2001/XMLSchema-instance" ]
      [list 'version "3.3" ]
      [list 'profile "Immersive" ]
      [list 'xsd:noNamespaceSchemaLocation "http://www.web3d.org/specifications/x3d-3.0.xsd"]
      [list 'x "0px"] [list 'y "0px"]
      [list 'width "960px"] [list 'height "540px"] ]
    [list
      [x3d:Scene [list
          [list 'render "true"]
          [list 'bboxCenter "0,0,0"]
          [list 'bboxSize "-1,-1,-1"]
          [list 'pickMode "idBuf"]
          [list 'doPickPass "true"]]
        scene-list] ]] ]

[define [sphere r p appr]
  [x3d:Transform [list [list 'translation p ]] [list
    [x3d:Shape [list] [list
      appr  
      [x3d:Sphere [list [list 'radius r]] [list]] ]] ]] ]

[define [points l]
  [x3d:Transform [list] [list
    [x3d:Shape [list] [list
      [x3d:PointSet [list] [list
        [x3d:Coordinate [list [list 'point l]] [list]] ]] ]] ]] ]

[define [lines i p c]
  [x3d:Transform [list] [list
    [x3d:Shape [list] [list 
       [x3d:IndexedLineSet 
         [list [list 'colorPerVertex "false"] 
               [list 'coordIndex i]]
         [list [x3d:Coordinate [list [list 'point p]] [list]]
               [x3d:Color [list [list 'color c]][list]] ]] ]] ]] ]

[define [face i p appr]
  [x3d:Transform [list] [list 
    [x3d:Shape [list] [list 
      appr    
      [x3d:IndexedFaceSet
        [list [list 'solid "false" ]
              [list 'coordIndex i]]
        [list [x3d:Coordinate [list [list 'point p]][list]] ]] ]] ]] ]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [write-x3d name scene-list]                                            
  [let [[out-file [open-output-file [string-append name ".x3d"] #:exists 'replace]]]
    [srl:sxml->xml  [list [mk-x3d scene-list]] out-file]
    [close-output-port out-file]]]

[define [write-x3d-html name x3dom-js x3dom-css jq-js c-js body-list]                                            
  [let [[out-file [open-output-file [string-append name ".html"] #:exists 'replace]]]
    [write-html [mk-html [list] [list] [mk-x3dom-head name x3dom-js x3dom-css jq-js c-js] [list] body-list] out-file]
    [close-output-port out-file]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define x3dom-css "./xlib/x3dom/x3dom-1.7.2/x3dom.css"
]
[define x3dom-js "./xlib/x3dom/x3dom-1.7.2/x3dom-full.js"
]
[define jq-js "./xlib/jquery/jquery-2.js"]

[define c-js [cat 
"function handleEvent(shape, event) \n"
"  { $('#lastObject').html($(shape).attr(\"def\")); } \n"
"  $(document).ready(function(){ \n"
"    $(\"shape\").each(function() { \n"
"      $(this).attr(\"onmouseover\", \"handleEvent(this)\"); }); }); \n"
]]

[define info-block [html:div [list][list [html:h3 [list][list "Last object:"]][html:span [list [list 'id "lastObject"]][list ""]]]]]

[define [list-range n]
  [for/list [[i [in-range n]]] i]]

[define [coord-index-str l]
  [apply cat [map [lambda [x] [cat " " [number->string x]]] l]]]

[define [mk-coord-node name points-val]
  [x3d:Coordinate [list [list 'DEF name] [list 'point points-val]] [list]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define [mk-face-block id p c cent-coord]
  [let* [[tag [pad id 5]]
         [shape-name [cat "shape-" tag]]
         [mat-name [cat "mat-" tag]]]
    [list [x3d:Transform [list [list 'translation cent-coord]]
      [list [x3d:Shape [list [list 'DEF shape-name] [list 'onmouseover "handleEvent(this, onmouseover)"] [list 'ispickable "true"]]
        [list [appr-te c]
              [x3d:IndexedFaceSet
                [list [list 'solid "false" ]
                      [list 'coordIndex " 0 1 2"]]
                [list [x3d:Coordinate [list [list 'point p]][list]]]] 
        ]]]]]]]

;;https://www.web3d.org/x3d/content/examples/Vrml2.0Sourcebook/Chapter13PointsLinesFaces/Figure13_12IndexedFaceSetCubeIndex.html
[define [mk-x3d-face-test ]
  [let [[clock-name "clock-0000"]]
    [append
      [list [x3d:Background [list [list 'skyColor "0 0 0"]][list]]] 
      [list [mk-face-block 1 " -1.0 -1.0 -1.0 1.0 -1.0 -1.0 1.0 -1.0 1.0"
                                 " 0.0 0.5 0.0" " 0.0 0.0 0.0" ]
            [mk-face-block 2 " 1.0 1.0 1.0 -1.0 1.0 1.0 -1.0 1.0 -1.0"
                                 " 0.0 0.5 0.0" " 0.0 0.0 0.0" ]]
    ;[map 
   ]]]

[define [face-test name]
  [write-x3d-html [cat "../" name] x3dom-js x3dom-css jq-js c-js [list [mk-x3d [mk-x3d-face-test ]] info-block] ]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [mk-edge-block clock-name i-list tk kv cv]
  [let* [[edge-tag [cat [pad [car i-list] 5] [pad [cadr i-list] 5]]]
         [CIname [cat "CIn-" edge-tag]]
         [Mname [cat "Mn-" edge-tag]]
         [ESname [cat "En-" edge-tag]]]
    [list
  [x3d:ColorInterpolator [list [list 'DEF CIname] [list 'key tk] [list 'keyValue kv]] [list]]
  [x3d:ROUTE [list [list 'fromField "fraction_changed"] [list 'fromNode clock-name] [list 'toField "set_fraction"] [list 'toNode CIname]] [list]]
  [x3d:Shape [list [list 'DEF ESname]] [list 
    [x3d:Appearance [list][list [x3d:Material [list [list 'DEF Mname] ] [list]]]]
    [x3d:IndexedLineSet [list [list 'coordIndex [cat "0 1 -1"]]] 
      [list [x3d:Coordinate [list [list 'point [apply cat [map [lambda [z] [vector-ref cv z]] i-list]]]][list]]]]]]
  [x3d:ROUTE [list [list 'fromField "value_changed"] [list 'fromNode CIname] [list 'toField "emissiveColor"] [list 'toNode Mname]][list]]
]]]

[define [mk-shape-block clock-name id cent-coord rad tv kv]
  [let* [[tag [pad id 5]]
         [shape-name [cat "shape-" tag]]
         [mat-name [cat "mat-" tag]]
         [CIname [cat "CIn-" tag]]]
     [list
       [x3d:ColorInterpolator [list [list 'DEF CIname] [list 'key tv] [list 'keyValue kv]] [list]]
       [x3d:ROUTE [list [list 'fromField "fraction_changed"] [list 'fromNode clock-name] [list 'toField "set_fraction"] [list 'toNode CIname]] [list]]
       [x3d:Transform [list [list 'translation cent-coord]]
         [list [x3d:Shape [list [list 'DEF shape-name] [list 'onmouseover "handleEvent(this, onmouseover)"] [list 'ispickable "true"]]
           [list [x3d:Appearance [list][list [x3d:Material [list [list 'DEF mat-name] ] [list]]]]
                 [x3d:Sphere [list [list 'radius rad]][list]]]]]]
       [x3d:ROUTE [list [list 'fromField "value_changed"] [list 'fromNode CIname] [list 'toField "emissiveColor"] [list 'toNode mat-name]][list]]
     ]]]

[define [mk-x3d-test tk e-list s-list cv]
  [let [[clock-name "clock-0000"]]
    [append [list
              [x3d:Background [list [list 'skyColor "0 0 0"]][list]] 
              [x3d:TimeSensor [list [list 'DEF clock-name] [list 'cycleInterval "5.0"] [list 'loop "true"] [list 'enabled "true"] [list 'first "true"]] [list]]
              ;[mk-coord-node coord-name coord-str]
              ]
            [map [lambda [z] [mk-edge-block clock-name [car z] tk [cadr z] cv]] e-list]
            [map [lambda [z] [mk-shape-block clock-name [car z] [cadr z] "0.1" tk [caddr z]]] s-list]
            [list [billboard "0.0 0.0 0.0" 0.3 "testing x3d" "0.50 0.75 0.50"]]
            ]]]

[define mk-test-coord-vec [apply vector [map str<-vec [map list->vector [shuffle [all-comb 3 [list -1 1]]]]]]]

[define test-edge-list
  [let [[z [shuffle [uni-comb 2 [list 0 1 2 3 4 5 6 7]]]]]
    [for/list [[i [in-range 0 12]]]
      [let [[cl [take [shuffle [all-comb 3 [list 0 1]]] 3]]]
        [list [list-ref z i] [apply cat [map str<-vec [map list->vector [cons [list-ref cl 2] cl]]]]]]]]]

[define test-shape-list
  [for/list [[i [in-range 0 8]]]
    [let [[cl [take [shuffle [all-comb 3 [list 0 1]]] 3]]]
      [list [add1 i]
            [str<-vec [vector-map [curry * 6] [vector-map [lambda [x][+ [random] [ - 0.5]]] [make-vector 3 0]]]]
            [apply cat [map str<-vec [map list->vector [cons [list-ref cl 2] cl]]]]]]]]

[define x3d-test-list [mk-x3d-test "0.0 0.333 0.666 1.0" test-edge-list test-shape-list mk-test-coord-vec]]

[define [clock-test name]
  [write-x3d-html [cat "../" name] x3dom-js x3dom-css jq-js c-js [list [mk-x3d x3d-test-list] info-block] ]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



