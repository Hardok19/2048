#lang racket/gui
(include "logic.rkt")

; 1. Create a top-level window (frame)
(define game (new frame% [label "Example GUI"] [width 1300] [height 900] [style '(no-resize-border)]))


; 2. Add a message (label) to the frame
(define msg (new message% [parent game] [label "Hello, Racket!"]))






; 3. Main horizontal container (holds row1 + col side by side)
(define main-panel
  (new horizontal-panel%
       [parent game]
       [stretchable-width #t]
       [stretchable-height #t]))

; 4. Left area (row1) — takes remaining space
(define row1
  (new vertical-panel%
       [parent main-panel]
       [style '(border)]
       [stretchable-width #t]
       [stretchable-height #t]))

; 5. Right column — fixed 300px wide
(define col
  (new vertical-panel%
       [parent main-panel]
       [min-width 300]
       [style '(border)]
       [stretchable-width #f]
       [stretchable-height #t]))





; 3. Add a button
(new button% [parent col]
             [min-width 50]	 
   	     [min-height 50]
             [label "↑"]
             [callback (lambda (btn event) (void))])

; 3. Add a button
(new button% [parent col]
             [min-width 50]	 
   	     [min-height 50]
             [label "←"]
             [callback (lambda (btn event) (void))])

; 3. Add a button
(new button% [parent col]
             [min-width 50]	 
   	     [min-height 50]
             [label "↓"]
             [callback (lambda (btn event) (void))])

; 3. Add a button
(new button% [parent col]
             [min-width 50]	 
   	     [min-height 50]
             [label "→"]
             [callback (lambda (btn event) (void))])



; 5. Show the window
(send game show #t)
