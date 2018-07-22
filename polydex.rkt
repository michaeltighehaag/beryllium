#lang racket
#|
Implementation of Polydex functions.
|#

[require racket/require
  [path-up "common.rkt"]]
[require racket/require
  [path-up "stream.rkt"]]
[require racket/require
  [path-up "XBRT.rkt"]]

[require sxml]
[require html-writing]
; (require racket/unsafe/ops)
[require math]
;[require plot]

[require [prefix-in bs: bitsyntax]]

[provide [all-defined-out]]



[define [int->bv v s] [bs:integer->bit-string v s #t]]
[define [char-str->bv s]
  [let [[sl [string-length s]]]
    [int->bv [if [equal? sl 0] 0 [string->number s 2]] sl]]]
[define [bv->char-str bs] 
  [let [[bsl [bs:bit-string-length bs]]]
    [if [equal? bsl 0] "" [pad [bs:bit-string->unsigned-integer bs #t] bsl 2]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [bv-ref v i] [bs:bit-string-ref v i]]
[define [bv-l v] [bs:bit-string-length v]]

[define [bv-tl v] [bs:sub-bit-string v 0  [/ [bv-l v] 2]]]
[define [bv-tr v] [bs:bit-string-drop v [/ [bv-l v] 2]]]

[define [bv-cat a b] [bs:bit-string-append a b]]

[define [eblg a] [sub1 [integer-length a]]]
[define [p2 n] [expt 2 n]]
[define [mod2 x] [modulo x 2]]

[define [bv-xor a b]
  [int->bv
    [bv-xor-rec [bs:bit-string->unsigned-integer a #t] [bs:bit-string->unsigned-integer b #t]]
    [bv-l a]]]

[define [bv-xor-rec ai bi]
  [if [and [equal? ai 0] [equal? bi 0]] 0
    [let [[aib [mod2 ai]] [bib [mod2 bi]]]
      [+ [* 2 [bv-xor-rec [/ [- ai aib] 2] [/ [- bi bib] 2]]] [mod2 [+ aib bib]]]]]]

[define [bv-dist v1 v2 sp]
  [for/sum [[i [in-range sp [bv-l v1]]]]
    [mod2 [+ [bv-ref v1 i] [bv-ref v2 i]]]]]

[define [bv-par v] [mod2 [for/sum [[i [in-range 0 [bv-l v]]]] [bv-ref v i]]]]

[define [sufpt n] [bv->char-str [int->bv [- [expt 2 n] 1] n]]]
[define [sufp n] [int->bv [- [expt 2 n] 1] n]]

#|
                      001
                   001   001
                001   002   001
             001   003   003   001
          001   004   006   004   001

          ^00   ^01   ^05   ^11   ^15
          ^15   ^14   ^10   ^04   ^00
           01    04    06    04    00 
           15    07    03    01    00

       001   005   010   010   005   001
    001   006   015   020   015   006   001
 001   007   021   035   035   021   007   001

|#


[define [corRM v lc]
  [let [[ll [eblg [bv-l v]]]]
    [if [equal? lc -1]
      [int->bv 0 [p2 ll]]
      [if [equal? lc ll] v
        [let [[L [bv-tl v]]
              [R [bv-tr v]]]
          [let [[Lp [corRM L lc]]
                [Rp [corRM R lc]]
                [D [bv-xor L R]]]
             [let [[Sdp [corRM D [sub1 lc]]]]
               [let [[M [bv-xor Sdp D]]]
                 [let [[LM [bv-xor M [bv-xor L Lp]]]
                       [RM [bv-xor M [bv-xor R Rp]]]]
                   [let [[LMdp [corRM LM [sub1 lc]]]
                         [RMdp [corRM RM [sub1 lc]]]]
                     [let [[c1 [bs:bit-string-append Lp [bv-xor Lp [bv-xor Sdp RMdp]]]]
                           [c2 [bs:bit-string-append [bv-xor Rp [bv-xor Sdp LMdp]] Rp]]]
                       [let [[d1 [bv-dist v c1 1]]
                             [d2 [bv-dist v c2 1]]]
                         [if [< d1 d2]
                             ;[begin [displayln [bin-char-str<-gbk [rbk<-bits [bv-cat v [sufp lc]]]]]
                             ;       [displayln [bv->char-str c1]][newline] c1]
                             c1
                             ;[begin [displayln [bin-char-str<-gbk [rbk<-bits [bv-cat v [sufp lc]]]]]
                              ;      [displayln [bv->char-str c2]][newline]  c2]
                             c2
                             
                             ]]]]]]]]]]]]]




[define [dycorRM T v lc]
  [let [[ll [eblg [bv-l v]]]]
    [if [equal? lc -1]
      [values T [int->bv 0 [p2 ll]]]
      [if [equal? lc ll] [values T v]
       [let [[lkup [xgetv T [rbk<-bits [bv-cat v [sufp lc]]]]]]
       [if [null? lkup] 
        [let [[L [bv-tl v]]
              [R [bv-tr v]]]
          [let-values [[[T1 Lp][dycorRM T L lc]]]
            [let-values [[[T2 Rp][dycorRM T1 R lc]]]
              [let [[D [bv-xor L R]]]
             [let-values [[[T3 Sdp][dycorRM T2 D [sub1 lc]]]]
               [let [[M [bv-xor Sdp D]]]
                 [let [[LM [bv-xor M [bv-xor L Lp]]]
                       [RM [bv-xor M [bv-xor R Rp]]]]
                   [let-values [[[T4 LMdp][dycorRM T3 LM [sub1 lc]]]]
                     [let-values [[[T5 RMdp][dycorRM T4 RM [sub1 lc]]]]
                     [let [[c1 [bs:bit-string-append Lp [bv-xor Lp [bv-xor Sdp RMdp]]]]
                           [c2 [bs:bit-string-append [bv-xor Rp [bv-xor Sdp LMdp]] Rp]]]
                       [let [[d1 [bv-dist v c1 1]]
                             [d2 [bv-dist v c2 1]]]
                         ;[display ll][display " " ][displayln lc]
                         [if [< d1 d2]
                           ;  [begin [displayln [bin-char-str<-gbk [rbk<-bits [bv-cat v [sufp lc]]]]]
                           ;         [displayln [bv->char-str c1]][newline] [values [xset T5 [rbk<-bits [bv-cat v [sufp lc]]] c1] c1]]
                           [values [xset T5 [rbk<-bits [bv-cat v [sufp lc]]] c1] c1]
                           ;  [begin [displayln [bin-char-str<-gbk [rbk<-bits [bv-cat v [sufp lc]]]]]
                           ;         [displayln [bv->char-str c2]][newline] [values [xset T5 [rbk<-bits [bv-cat v [sufp lc]]] c2] c2]]
                           [values [xset T5 [rbk<-bits [bv-cat v [sufp lc]]] c2] c2]
                         ]]]]]]]]]]]]
       ; [begin [displayln [bin-char-str<-gbk [rbk<-bits [bv-cat v [sufp lc]]]]]
       ;        [displayln [bv->char-str lkup]][displayln "lk"] [values T lkup]]
       [values T lkup]
       ]]]]]]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[define [rb32] [pad [inexact->exact [floor [* [random] [p2 32]]]] 32 2]]

[define [crb n] [if [equal? n 0] [rb32] [string-append [crb [- n 1]][crb [- n 1]]]]]

[define [tc v a ] 
  [let [[v [char-str->bv v]]]
    [bv->char-str [corRM v a ]]]]

[define [run b]
  [let [[i [crb [- b 5]]]]
    [for [[c [in-range 0 [+ 1 b]]]]
      [let [[r [time [tc i c ]]]]
        [displayln r]]]
    [displayln i]]]


;[run 7]
;[define tr [test]]
;[xget tr [rbk<-bin-char-str "11"]]

[define bt [mk-root xf-def]]
[define [gcrm v lc ll t]
  [let [[tt [xget t v]]]
    ;[for/vector [[i [in-range 4]]] i]
    [if [null? tt] [corRM v lc ] tt]]]


[define [rec-dc T v c]
  [if [< c 0] T
  [let-values [[[t r][dycorRM [mk-root xf-def] v c]]]
    [displayln [bv->char-str r]]
    [rec-dc t v [sub1 c]]]]]

[define [rev-rec-dc T v c f]
  [if [equal? c [add1 f]] T
  [let-values [[[t r][dycorRM [mk-root xf-def] v c]]]
    [displayln [bv->char-str r]]
    [rev-rec-dc t v [add1 c] f]]]]

[define [drun b c]
  [let [[i [crb [- b 5]]]]
[let [[v [char-str->bv i]]]

;    [time [let [[r [corRM [char-str->bv i] c ]]]
;      [displayln [bv->char-str r]]
;      [displayln i]]]

;[time    [for [[c [in-range 0 [+ 1 c]]]]
;    [let [[r [corRM [char-str->bv i] c ]]]
;      [displayln [bv->char-str r]]
;      ]  ]]  
    
;    [time [let-values [[[t r][dycorRM [mk-root xf-def] [char-str->bv i] c]]]
;      [displayln [bv->char-str r]]
;      [displayln i]
;    ]]

    [time [rec-dc [mk-root xf-def] v c]]
    [time [rev-rec-dc [mk-root xf-def] v 0 c]]
[void]
    ]]]
