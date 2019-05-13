#lang racket
[require sxml]

;; based on pseudocode from https://en.wikipedia.org/wiki/Hirschberg%27s_algorithm ~ 2019/05/10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Hirschberg      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[define [str-rev x] [list->string [reverse [string->list x]]]]
[define [vec-rev x] [list->vector [reverse [vector->list x]]]]
[define [vec-argmax v]
  [vector-member [vector-argmax [lambda [x] x] v] v]]
[define [del x] -2]
[define [ins y] -2]
[define [sub x y] [if [equal? x y] 2 -1]]
[define [dcons x y]
  [let [[r [string-append "Ret: " x " _ " y]]]
    [cons x y]]] 

[define [NeedlemanWunsch-score X Y]
  [let [[lx [string-length X]][ly [string-length Y]]]
    [let [[Score0 [make-vector [add1 ly] 0]]
          [Score1 [make-vector [add1 ly] 0]]] 
      [for [[j [in-range 1 [add1 ly]]]]
        [vector-set! Score0 j [+ [vector-ref Score0 [sub1 j]] [ins [string-ref Y [sub1 j]]]]]]
      [for [[i [in-range 1 [add1 lx]]]]  
        [vector-set! Score1 0 [+ [vector-ref Score0 0] [del [string-ref X [sub1 i]]]]]
        [for [[j [in-range 1 [add1 ly]]]]
          [let [[score-sub [+ [vector-ref Score0 [sub1 j]] [sub [string-ref X [sub1 i]] [string-ref Y [sub1 j]]]]]
                [score-del [+ [vector-ref Score0 j] [del [string-ref X [sub1 i]]]]]
                [score-ins [+ [vector-ref Score1 [sub1 j]] [ins [string-ref Y [sub1 j]]]]]]
         [vector-set! Score1 j [max score-sub score-del score-ins]]]]
         [for [[j [in-range 0 [add1 ly]]]]
           [vector-set! Score0 j [vector-ref Score1 j]]]]
      Score1]]]

[define [Hirschberg X Y]
  [let [[lx [string-length X]][ly [string-length Y]]]
    [if [equal? 0 lx] 
      [dcons [make-string ly #\-] Y]
      [if [equal? 0 ly]
        [dcons X [make-string lx #\-] ]
        [if [or [equal? 1 lx] [equal? 1 ly]]
          [NeedlemanWunsch-length1-case X Y]
          [let* [[xmid  [quotient lx 2]]
                 [score-left  [NeedlemanWunsch-score [substring X 0 xmid] Y]]
                 [score-right [NeedlemanWunsch-score [str-rev [substring X xmid lx]] [str-rev Y]]]
                 [ymid  [vec-argmax [vector-map + score-left [vec-rev score-right]]]]]
            [let [[pre [Hirschberg [substring X 0 xmid] [substring Y 0 ymid]]]
                  [suf [Hirschberg [substring X xmid lx] [substring Y  ymid ly]]]]
              [dcons [string-append [car pre] [car suf]][string-append [cdr pre] [cdr suf]]]]]
          ]]]]]
        
[define [NeedlemanWunsch-length1-case X Y] 
  [if [equal? 1 [string-length X]]
      [let [[m [regexp-match-positions X Y]]]
        [if [equal? m #f] [dcons [string-append X [make-string [sub1 [string-length Y]] #\-]] Y]
                          [dcons [string-append [make-string [caar m] #\-] X [make-string [- [string-length Y] [add1 [caar m]]] #\-]] Y]]]
      [let [[m [regexp-match-positions Y X]]]
        [if [equal? m #f] [dcons X [string-append Y [make-string [sub1 [string-length X]] #\-]]]
                          [dcons X [string-append [make-string [caar m] #\-] Y [make-string [- [string-length X] [add1 [caar m]]] #\-]]]]]]]
[define [test-H]
  [let [[r [Hirschberg  "skgosyenjhdfognrrrhdy" "skfgosgenjhdfoffgnrhy"]]]
    [displayln [car r]]
    [displayln [cdr r]]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; based on pseudocode from https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm ~ 2019/05/10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; NeedlemanWunsch      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


[define [mm-ref M s i j]
  [vector-ref M [+ i [* j s]]]]
  
[define [sim x y] [if [equal? x y] 1 -1]]

[define [NeedlemanWunsch d sf X Y]
  [let* [[lx [string-length X]]
         [ly [string-length Y]]
         [F [make-vector [* [add1 lx] [add1 ly]] 0]]
         [s [add1 lx]]]   
    [for [[i [in-range 0 [add1 lx]]]]
      [vector-set! F i [* d i]]]
    [for [[j [in-range 0 [add1 ly]]]]
      [vector-set! F [* j [add1 lx]] [* d j]]]
    [for* [[i [in-range 1 [add1 lx]]]
          [j [in-range 1 [add1 ly]]]]
      [let [[match [+ [sf [string-ref X [sub1 i]][string-ref Y [sub1 j]]]
                      [mm-ref F s [sub1 i] [sub1 j]]]]
            [delete [+ d [mm-ref F s [sub1 i] j]]]
            [insert [+ d [mm-ref F s i [sub1 j]]]]]
         [vector-set! F [+ i [* j [add1 lx]]] [max match insert delete]]]]
      F]]

[define [align-rec d sf i j LX LY X Y s F]
  [if [or [> i 0] [> j 0]]
    [if [and [> i 0] [> j 0]
             [equal? [mm-ref F s i j]
                     [+ [mm-ref F s [sub1 i] [sub1 j]]
                        [sf [string-ref X [sub1 i]] [string-ref Y [sub1 j]]]]]]
      [align-rec d sf [sub1 i] [sub1 j] [cons [string-ref X [sub1 i]] LX] [cons [string-ref Y [sub1 j]] LY] X Y s F]
      [if [and [> i 0] [equal? [mm-ref F s i j] [+ [mm-ref F s [sub1 i] j] d]]]
        [align-rec d sf [sub1 i] j [cons [string-ref X [sub1 i]] LX] [cons 'gap LY] X Y s F]
        [align-rec d sf i [sub1 j] [cons 'gap LX] [cons [string-ref Y [sub1 j]] LY] X Y s F]]]
    [cons LX LY]]]

[define [align X Y d sf]
  [align-rec d sf [string-length X] [string-length Y] [list] [list] X Y [add1 [string-length X]] [NeedlemanWunsch d sf X Y]]]





[define [align->sxml lX lY nX nY]
  [if [null? lX]
    [cons [reverse [map mk-string-helper nX]]
          [reverse [map mk-string-helper nY]]]
    [if [equal? [car lX][car lY]]
      [align->sxml [cdr lX] [cdr lY] [cons-helper 'char [car lX] nX] [cons-helper 'char [car lY] nY]] 
      [if [equal? [car lX] 'gap]
        [align->sxml [cdr lX] [cdr lY] nX [cons-helper 'indel [car lY] nY]] 
        [if [equal? [car lY] 'gap]
          [align->sxml [cdr lX] [cdr lY] [cons-helper 'indel [car lX] nX] nY] 
          [align->sxml [cdr lX] [cdr lY] [cons-helper 'diff [car lX] nX] [cons-helper 'diff [car lY] nY]]]]]]] 

[define [mk-string-helper x]
  [if [char? [car x]]
    [list->string [reverse x]]
    [cons 'span
          [cons [list '@ [list 'class [symbol->string [car x]]]]
          [list [list->string [reverse [cdr x]]]]]]]]

[define [cons-helper s c cl]
  [if [equal? s 'char]
    [if [null? cl]
      [list [list c]]
      [if [char? [caar cl]]
        [cons [cons c [car cl]] [cdr cl]]
        [cons [list c] cl]]]
    [if [equal? s 'diff]
      [if [null? cl]
        [list [list 'diff c]]
        [if [equal? 'diff [caar cl]]
          [cons [cons 'diff [cons c [cdar cl]]] [cdr cl]]
          [cons [list 'diff c] cl]]]
      [if [null? cl]
        [list [list 'indel c]]
        [if [equal? 'indel [caar cl]]
          [cons [cons 'indel [cons c [cdar cl]]] [cdr cl]]
          [cons [list 'indel c] cl]]]]]]

[define [string-diff x y] 
  [let [[r [align x y -1 sim]]]
    [let [[nr [align->sxml [car r][cdr r] [list] [list]]]]
      [list [srl:sxml->xml-noindent [car nr]]
            [srl:sxml->xml-noindent [cdr nr]]]]]]

[define [test-NW]
  [let [[r [align "skgosyenjhdfognrrrhdy" "skfgosgenjhdfoffgnrhy" -1 sim]]]
    [displayln [list->string [map [lambda [x] [if [equal? x 'gap] #\- x]] [car r]]]]
    [displayln [list->string [map [lambda [x] [if [equal? x 'gap] #\- x]] [cdr r]]]]
    [let [[nr [align->sxml [car r][cdr r] [list] [list]]]]
      [displayln  [srl:sxml->xml-noindent [car nr]]]
      [displayln  [srl:sxml->xml-noindent [cdr nr]]]]]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


