#lang racket
[require "common.rkt"]

[require sxml]
[provide [all-from-out sxml]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Provided functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[provide [all-defined-out]]

[define xml-string<-sxml srl:sxml->xml] 

[define [mk-sxml-top namespace-list root-element]
  [list '*TOP*
  [list '@ [cons '*NAMESPACES* namespace-list]]
  [list '*PI* 'xml "version=\"1.0\" encoding=\"UTF-8\""]
  root-element]]

[define [mk-sxml-elem root attr-list children-list]
  [cons root
  [cons [cons '@ attr-list]
  children-list]]]
  

[define [mk-sxml nsl doc-root]
  [list '*TOP* [list '@ [cons '*NAMESPACES* nsl]] xml-dec doc-root]]

[define xml-dec [list '*PI* 'xml "version=\"1.0\" encoding=\"UTF-8\""]]

[define [nsl->attr-string nsl]
  [apply cat
  [map [lambda [x] [cat " xmlns:" [symbol->string [car x]] "=" "\"" [cadr x] "\""]]
  nsl]]]

[define [sxml-fix-prfx nsl s]
 [let [[uri-assocs
         [make-immutable-hash
         [map [lambda [x] [cons [cadr x][symbol->string [car x]]]]
         nsl]]]]
  [regexp-replaces s
  [map [lambda [x] [list [regexp [cadr x]] [hash-ref uri-assocs [caddr x]]]]
  [map [lambda [x] [regexp-match #rx"xmlns:(prfx.*?)=\"(.*?)\"" x]]
  [regexp-match #rx"xmlns:prfx.*?=\".*?\"" s]
  ]]]]]   

[define [xml-mv-ns-to-root nsl x]
  [let [[new 
      [regexp-match #rx"(<.xml version=\"1.0\" encoding=\"UTF-8\".>[^<]*<[^ ]*)(.*)" 
      [regexp-replace* #rx"xmlns:[^=]*=\"[^\"]*\"" x ""]]]]
  [cat [cadr new] [nsl->attr-string nsl] [caddr new]]]]
