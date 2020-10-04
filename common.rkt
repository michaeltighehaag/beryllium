#lang racket
#|
A variety of useful functions.
|#

[require math]
[provide [all-defined-out]]


[require file/tar]
[provide [all-from-out file/tar]]
[require file/untgz]
[provide [all-from-out file/untgz]]
[require sxml]
[provide [all-from-out sxml]]
[require html-parsing]
[provide [all-from-out html-parsing]]
[require json-parsing]
[provide [all-from-out json-parsing]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define cat string-append]
[define string<-number number->string]
[define list-zip [lambda lists [apply map list lists]]]
[define [list-get n l] [list-ref l n]]
[define [list-sort pred l] [sort l pred]]
[define [list-take n l] [take l n]]
[define [list-drop n l] [drop l n]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;xml 

[define [file<-string fn str [ex 'replace]]
  [call-with-output-file fn [lambda [out] [display str out]] #:exists ex]]

[define [string<-file fn]
  [port->string [open-input-file fn]]]

[define [sxml<-filename fn [nsl null]] [call-with-input-file fn [lambda [in] [ssax:xml->sxml in nsl]]]]
[define [sxml<-string str [nsl null]] [ssax:xml->sxml [open-input-string str] nsl]]

[define [jsx<-filename fn] [call-with-input-file fn [lambda [in] [json->sxml in ]]]]
[define [jsx<-string str] [json->sxml [open-input-string str]]]

[define [xml<-sxml-elem body [nsl null]]
  [srl:sxml->xml [list '*TOP* [list '*PI* 'xml "version=\"1.0\" encoding=\"UTF-8\""]
                              [list '@ [cons '*NAMESPACES* nsl]]
                              body]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define [jsx-str s] [list 'string s]]
[define [jsx-mem k v] [list 'member [list '@ [list 'name k]] v]]
[define [jsx-obj l] [cons 'object l]]
[define [jsx-arr l] [cons 'array l]]

[define [jsx->json o p [i 0]]
  [case [car o]
    ['d [display [cadr o] p]]
    ['string [display [cat "\""[cadr o]"\""] p]]
    ['member [begin [display [cat "\n"[make-string i #\ ]] p]
                    [display [cat "\""[cadr[cadadr o]]"\""] p]
                    [display ": " p]
                    [jsx->json [caddr o] p i]]]
    ['object [begin [display [cat "\n"[make-string i #\ ]"{"] p]
                    [jsx->json [cadr o] p [+ i 2]]
                    [map [lambda [x] [jsx->json x p [+ i 2]]] [delimit-list [cddr o] [list 'd ","]]]
                    [display [cat "\n"[make-string i #\ ]"}"] p]]]
    ['array  [begin [display [cat "\n"[make-string i #\ ]"["] p]
                    [jsx->json [cadr o] p [+ i 2]]
                    [map [lambda [x] [jsx->json x p [+ i 2]]][delimit-list [cddr o] [list 'd ","]]]
                    [display [cat "\n"[make-string i #\ ]"]"] p]]]
    [else [display "unrec error"]]]]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [nth-tsv n] [lambda [x] [list-ref [regexp-match* #rx"[^\t\n]*" x] [* 2 n]]]]

[define [n<-f f] [string->number f]]

[define [rcomp p m n]  [lambda [r1 r2] [p [m [[nth-tsv n] r1]]
                                          [m [[nth-tsv n] r2]]]]]

[define [rxms r n] [lambda [s] [list-ref [regexp-match [regexp r] s] n]]]

[define [delimit-list l d [r null]]
  [if [null? l] [reverse r] [delimit-list [cdr l] d [cons [car l] [cons d r]]]]]

[define rx-swaps regexp-replaces]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;making padded numeric strings
[define [pad n d [r 10]]
  [let [[s [number->string n r]]]
    [cat [make-string [+ d [- [string-length s]]] #\0] s ]]]

[define [str-lpad s d [c #\0]]
  [cat [make-string [+ d [- [string-length s]]] c] s ]]
[define [str-rpad s d [c #\0]]
  [cat s [make-string [+ d [- [string-length s]]] c]]]  
;returns a counter function that sequentially returns handy padded integers 
[define [make-seq-counter [start 0] [digits 10] [radix 10]]
  [let [[count start]] 
    [lambda [] [set! count [+ count 1]] [pad count digits radix] ]]]
    
[define [str<-vec x]
  [apply cat [vector->list [vector-map [lambda [y] [cat " " [number->string y]]] x]]]]   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;[enum-trees [list "a" "b" "c"]]
[define [enum-trees labels]
  [if [equal? [length labels] 1]
       labels
      [for*/list [[i [in-range 1 [length labels]]]
                  [left [in-list [enum-trees [take labels i]]]]
                  [right [in-list [enum-trees [drop labels i]]]]]
       [list left right]]]]

;[uni-comb 2 [list "a" "b" "c"]] m unique choices from list
[define [uni-comb m lst]
  [if [equal? m 0]
    [list [list]]
    [if [null? lst] '[]
      [append [map [lambda [y] [cons [car lst] y]]
                   [uni-comb [- m 1] [cdr lst]]]
              [uni-comb m [cdr lst]]]]]]

;[all-comb 2 [list "a" "b" "c"]] all perms with duplicates
[define [all-comb d v-list]
  [for/fold [[ret-list [list [list]]]]
            [[n [in-range d]]]
    [apply append 
      [map [lambda [x] 
        [for/list [[f v-list]] 
           [cons f x]]] ret-list]]]]

;[all-perm [list 1 2 3]]
[define [all-perm l]
  [if [> 2 [length l]]
    [list l] 
    [apply append 
      [map [lambda [x] 
             [for/list [[i [in-range 0 [length  l]]]]
               [append [take x i] [list [car l]] [drop x i]]]]
           [all-perm [cdr l]]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define test-str "\n[define x 23]\n[define [f1 d f] [+ d f]]\n[define y [f1 x 3]]\n"]

[define swap-list [list
  [list "f1" "g2"]
  [list "mess" "clean"]]]

[define rx-delim-list [list "\t" "\n" " " "\\[" "\\]" "\"" "," "`" "'" ";"]]

[define [delim-swaps s sl dl]
  [let [[dp [cat "(" [apply cat [cdr [delimit-list rx-delim-list "|"]]] ")"]]
        [mk-rx-reps [lambda [sl ds] [map [lambda [x] [list [regexp [cat ds [car x] ds]] [cat "\\1" [cadr x] "\\2"]]] sl]]]]
    [regexp-replaces s [mk-rx-reps swap-list dp]]]]

;[delim-swaps test-str swap-list rx-delim-list]
