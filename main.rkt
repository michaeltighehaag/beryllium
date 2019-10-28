#lang racket

[require "common.rkt"]
[require "stream.rkt"]
[require "XBRT.rkt"]
[require "polydex.rkt"]
[require "time.rkt"]
[require "XML.rkt"]
[require "X3D.rkt"]
[require "HTML.rkt"]
[require "align.rkt"]

[provide [all-from-out "common.rkt"]]
[provide [all-from-out "stream.rkt"]]
[provide [all-from-out "XBRT.rkt"]]
[provide [all-from-out "polydex.rkt"]]
[provide [all-from-out "time.rkt"]]
[provide [all-from-out "XML.rkt"]]
[provide [all-from-out "X3D.rkt"]]
[provide [all-from-out "HTML.rkt"]]
[provide [all-from-out "align.rkt"]]

[provide spider]
[provide spider-group]
[provide spider-test]
[provide spider-test-pool]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


[define spider-cl [list
     [cons "B004" "I003"]
     [cons "B005" "I004"]
     [cons "B006" "I003"]
     [cons "B003" "I002"]
     [cons "B002" "I001"]
     [cons "B002" "I002"]
     [cons "B001" "I001"]
     [cons "B008" "I008"]]]

[define [cswap x] [cons [cdr x] [car x]]]
[define spider-rcl
  [map cswap spider-cl]]

[define spider-test-pool
  [xbrt<-stream [lambda [z] [rbk<-string [car z]]] cdr xgen-cons
    [stream<-list [shuffle [append spider-cl spider-rcl]]]]]
;;;;


[define [spider-push l s] [if [null? l] s [spider-push [cdr l] [cons [car l] s]]]]
[define [spider-add x t] [xset t [rbk<-string x] x]]
[define [spider-get x t] [xgetv t [rbk<-string x]]]

[define [spider pool seen q cand rgrp ]
  [if [null? cand]
    [cons seen rgrp]
    [let [[cur [car cand]] [t [cdr cand]]]
      [if [equal? cur [spider-get cur rgrp]]
        [spider pool seen q t rgrp]
        [spider pool [spider-add cur seen] q [spider-push [q cur pool] t] [spider-add cur rgrp]]]]]]

[define [spider-check pool x q]
  [if [stream-null? [cdr x]]
    [cons [cons [caar x] null] [stream]]
    [let [[id [stream-car [cdr x]]]]
      [if [equal? id [spider-get id [caar x]]]
        [spider-check pool [cons [car x] [stream-cdr [cdr x]]] q]
        [cons [spider pool [caar x] q [list id] [mk-root xf-def]]
              [stream-cdr [cdr x]]]]]]]

[define [spider-group data-pool q tests]
  [stream-unfold
    [lambda [x] [cdar x]] 
    [lambda [x] [not [stream-null? [cdr x]]]] 
    [lambda [x] [spider-check data-pool x q]]
    [cons [spider data-pool [mk-root xf-def] q [list [stream-car tests]] [mk-root xf-def]]
          tests]]]   

;;;;;

[define [spider-test]
  [let [[mqt [lambda [p d] [xgetv d [rbk<-string p]]]]]
    [disp-stream
      [stream<-xbrt #:vtf cstr-kvps-pre spider-test-pool]]
    [disp-stream
      [stream-map [lambda [x] [stream<-xbrt #:vtf cstr-kvps-pre x]]
      [spider-group spider-test-pool mqt [stream-map car [stream<-xbrt #:vtf cstr-kvps-pre spider-test-pool]]]]]
  ]]


