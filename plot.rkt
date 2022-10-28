#lang racket
[require "common.rkt"]
[require "stream.rkt"]
[require "XBRT.rkt"]
[require "time.rkt"]
[require "XML.rkt"]
[require "HTML.rkt"]
[require "svg.rkt"]
[require "X3D.rkt"]

[define tdat [stream [list 10 30] [list 20 60] [list 30 40]]]

[define [scale-2d x y ext]
  [let [[xmin [car ext]]
        [xmax [cadr ext]]
        [ymin [caddr ext]]
        [ymax [cadddr ext]]]
  [let [[xs [/ [* x 128] [- xmax xmin]]]
        [xo [* xmin [/ [* x 128] [- xmax xmin]]]]
        [ys [/ [* -1 y 128] [- ymax ymin]]]
        [yo [* ymin [/ [* -1 y 128] [- ymax ymin]]]]]
  [lambda [z]
    [cons [+ xo [* xs [car z]]] [cons [+ yo [* ys [cadr z]]] z]]]]]]

[define [get-ext-2d strm]
    [let* [[get-x [lambda [z] [car z]]]
         [get-y [lambda [z] [cadr z]]]
         [xmax [get-x [[stream-argext > get-x] strm]]]
         [xmin [get-x [[stream-argext < get-x] strm]]]
         [ymax [get-y [[stream-argext > get-y] strm]]]
         [ymin [get-y [[stream-argext < get-y] strm]]]
        ]
      [list xmin xmax ymin ymax]]]

[define [plot-2d x y strm]
  [let [[ext [get-ext-2d strm]]]
    [displayln ext]
     [stream-cons ext
     [stream-map [scale-2d x y ext]
     strm]]]]

[define [mk-svg-seg x]
  [let [[a [cadr x]]
        [b [car x]]]
    [svg:path [mk-black 1 1 0.0] [mk-black 2 4 1.0] 2
      [apply cat [list
        [mk-abs-mv [car b] [cadr b]]
        [mk-abs-line [car a] [cadr a]]]]]]]

[define [mk-seg-stream strm]
  [stream-zip [stream-cdr strm] strm]]

[define [plot-test x y dat]
  [svg:svg x y [list]
    [list<-stream
    [stream-append
     [stream [svg:rect 0 0 [* x 128] [* y 128] 0 [mk-black 4 4 1.0] [mk-black 4 4 1.0] 0 [list]]] 
     [stream-map mk-svg-seg
     [mk-seg-stream [stream-cdr [plot-2d x y dat]]]]]]]]

[define td
  [stream-map [lambda [z] [list z [cos z]]]
  [stream-take 120 [stream-from 0.0 0.1]]]]

  [define [svg-plot-test]
  [file<-string "svg-test-03.svg" [xml-string<-sxml [plot-test 12 6 td]] 'replace]]

[svg-plot-test]

[define [lm2 x]
  [modulo [apply + [map [lambda [z] [/ z 2]] x]] 2]]

[disp-stream
 [stream-map [lambda [x] [list [lm2 x] x]]
 [stream-of [list i j k]
  [i in [stream-range -10 11 2]]
  [j in [stream-range -10 11 2]]
  [k in [stream-range -10 11 2]]]]]

[define [simplex-centroid vl]
  [map [list-scale [/ 1.0 [length vl]]]
    [map [lambda [z] [apply + z]] [apply list-zip vl]]]]

[define [test-interpolate c f p]
  [let [[a [car p]][b [cadr p]]]
    [let [[fa [f a]][fb [f b]]]
      [let [[ab [map - a b]][ad [- c fa]][bd [- c fb]]]
        [if [< 0 [* ad bd]]
          [list-scale [/ fa [+ fa fb]] ab]
          #f]]]]]
    
[define [list-scale a]
  [lambda [x] [* a x]]]

[define [list-norm l]
  [sqrt [map [lambda [x y] [* x y]] l]]]

[define vlist-2d
  [list
    [list
      [list [list -1 -1] [list -1  1] [list  1  1]]
      [list [list  1  1] [list  1 -1] [list -1 -1]]]
    [list
      [list [list  1 -1] [list  1  1] [list -1  1]]
      [list [list -1  1] [list -1 -1] [list  1 -1]]]]]

[define vlist-3d
  [list
    [list
      [list [list  1  1  1] [list -1  1  1] [list  1 -1  1][list  1  1 -1]]
      [list [list -1 -1  1] [list  1 -1  1] [list -1  1  1][list -1 -1 -1]]
      [list [list  1 -1 -1] [list -1 -1 -1] [list  1  1 -1][list  1 -1  1]]
      [list [list -1  1 -1] [list  1  1 -1] [list -1 -1 -1][list -1  1  1]]
      [list [list -1 -1 -1] [list -1  1  1] [list  1 -1  1][list  1  1 -1]]]
    [list
      [list [list -1  1  1] [list  1  1  1] [list -1 -1  1][list -1  1 -1]]
      [list [list  1 -1  1] [list -1 -1  1] [list  1  1  1][list  1 -1 -1]]
      [list [list -1 -1 -1] [list  1 -1 -1] [list -1  1 -1][list -1 -1  1]]
      [list [list  1  1 -1] [list -1  1 -1] [list  1 -1 -1][list  1  1  1]]
      [list [list  1 -1 -1] [list  1  1  1] [list -1 -1  1][list -1  1 -1]]]]]

;> [uni-comb 2 [list 1 2 3]]
;'((1 2) (1 3) (2 3))
;> [uni-comb 3 [list 1 2 3 4]]
 
[map simplex-centroid [append [car vlist-3d][cadr vlist-3d]]]
;[list [list  1  1  1] [list -1  1  1] [list  1 -1  1][list  1  1 -1]]]
;convert -interlace none -depth 8 -size 4x4+0 rgb:ti4.raw pic4.png
;cat ti4.raw | convert -interlace none -depth 8 -size 4x4+0 rgb:- png:- > pic5.png
;sox bwave-float_session.wav -r 44100 -e unsigned -b 16 -c 1 traw3.raw
;echo hbdfdfbsdngnfmfmhmh,gj,r,mello | hexdump -v -e '6/1 "%02X " "\n"'



;p 1 2
;c   2 3
;v   2 3
;s
;g

