#lang racket

[require sxml]

[provide [all-from-out sxml]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Provided functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[provide [all-defined-out]]

[define [mk-sxml-top namespace-list root-element]
  [list '*TOP*
  [list '@ [cons '*NAMESPACES* namespace-list]]
  [list '*PI* 'xml "version=\"1.0\" encoding=\"UTF-8\""]
  root-element]]

[define [mk-sxml-elem root attr-list children-list]
  [cons root
  [cons [cons '@ attr-list]
  children-list]]]
  
  
