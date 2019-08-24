#lang racket
[require racket/require
  [path-up "beryllium/main.rkt"]]
  

[define [rec-list<? x y]
  [if [< [car x][car y]] #t
    [if [< [car y][car x]] #f
      [if [or [null? [cdr x]] [null? [cdr y]]] #f
        [rec-list<? [cdr x] [cdr y]]]]]]

[define [dps2 ud]
  [stream-sort rec-list<?
  [stream-of [list aa a b]        
    [a in [stream-range 0 ud]]
    [b in [stream-range 0 ud]]
    [aa is [+ a b]]         
   ]]]
[define [dps3 ud]
  [stream-sort rec-list<?
  [stream-of [list aa a b c]        
    [a in [stream-range 0 ud]]
    [b in [stream-range 0 ud]]
    [c in [stream-range 0 ud]]
    [aa is [+ a b c]]         
   ]]]
[define [dps4 ud]
  [stream-sort rec-list<?
  [stream-of [list aa a b c d]        
    [a in [stream-range 0 ud]]
    [b in [stream-range 0 ud]]
    [c in [stream-range 0 ud]]
    [d in [stream-range 0 ud]]
    [aa is [+ a b c d]]         
   ]]]
[define [dps5 ud]
  [stream-sort rec-list<?
  [stream-of [list aa a b c d e]        
    [a in [stream-range 0 ud]]
    [b in [stream-range 0 ud]]
    [c in [stream-range 0 ud]]
    [d in [stream-range 0 ud]]
    [e in [stream-range 0 ud]]
    [aa is [+ a b c d e]]         
   ]]]


[define [ddps dps ud]
  [stream-map [lambda [x] [stream-cons [stream-length x]
                                       [stream [stream-car x]]]]
  [stream-group [lambda [x y] [equal? [car x][car y]]]
  [dps ud]]]]


  
[disp-stream [ddps dps2 8]]
[disp-stream [ddps dps3 8]]
[disp-stream [ddps dps4 8]]
[disp-stream [ddps dps5 8]]
