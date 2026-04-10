#lang racket/gui
(require "logic.rkt")




;; -------------------------------------------------------------
;; FUNCIONES DE PRESENTACIÓN
;;
;; Estas funciones no tienen efectos de lado: reciben datos y
;; devuelven valores. Se separan del resto para dejar claro que
;; no interactúan con el estado ni con los widgets.
;; -------------------------------------------------------------

;; Las celdas con valor 0 se muestran vacías para que el tablero
;; luzca limpio. Cualquier otro valor se convierte a string.
(define (valor->label valor)
  (cond
    ((<= valor 0) "")
    (else (number->string valor))))

;; La condición de victoria se verifica recorriendo el tablero
;; fila por fila. Se separa en dos funciones para mantener la
;; recursión sobre listas simple y legible.
(define (hay-ganador? matriz)
  (cond
    ((null? matriz) #f)
    ((fila-tiene-2048? (car matriz)) #t)
    (else (hay-ganador? (cdr matriz)))))

(define (fila-tiene-2048? fila)
  (cond
    ((null? fila) #f)
    ((= (car fila) 2048) #t)
    (else (fila-tiene-2048? (cdr fila)))))


;; -------------------------------------------------------------
;; FUNCIONES DE ACTUALIZACIÓN DEL GUI
;;
;; Estas funciones sí producen efectos de lado porque manipulan
;; widgets de Racket/GUI. El patrón que siguen es siempre el
;; mismo: limpiar el contenido previo del panel y reconstruirlo
;; desde cero a partir del estado nuevo. Esto evita tener que
;; rastrear qué widget corresponde a qué celda, simplificando
;; la sincronización entre estado y vista.
;; -------------------------------------------------------------

;; Para limpiar un panel se obtiene la lista de sus hijos y se
;; eliminan uno a uno con delete-child. Se usa recursión sobre
;; esa lista en lugar de un loop imperativo.
(define (limpiar-hijos panel hijos)
  (cond
    ((null? hijos) (void))
    (else
     (send panel delete-child (car hijos))
     (limpiar-hijos panel (cdr hijos)))))

(define (limpiar-panel panel)
  (limpiar-hijos panel (send panel get-children)))

;; Cada celda del tablero se representa como un message% dentro
;; de un vertical-panel% con borde. El panel intermedio es
;; necesario porque message% por sí solo no soporta bordes
;; visibles en Racket/GUI.
(define (crear-celda valor fila-panel)
  (new message%
       (parent (new vertical-panel%
                    (parent fila-panel)
                    (style '(border))
                    (min-width 90)
                    (min-height 90)
                    (stretchable-width #f)
                    (stretchable-height #f)))
       (label (valor->label valor))
       (font (make-font #:size 15 #:weight 'bold))))

;; Cada fila de la matriz lógica se dibuja dentro de un
;; horizontal-panel% nuevo, de modo que las celdas queden
;; alineadas horizontalmente y cada fila ocupe su propio espacio
;; vertical. dibujar-celdas recorre la fila recursivamente.
(define (dibujar-celdas fila fila-panel)
  (cond
    ((null? fila) (void))
    (else
     (crear-celda (car fila) fila-panel)
     (dibujar-celdas (cdr fila) fila-panel))))

(define (dibujar-fila fila tablero-panel)
  (dibujar-celdas fila
                  (new horizontal-panel%
                       (parent tablero-panel)
                       (stretchable-width #t)
                       (stretchable-height #f))))

(define (dibujar-filas filas tablero-panel)
  (cond
    ((null? filas) (void))
    (else
     (dibujar-fila (car filas) tablero-panel)
     (dibujar-filas (cdr filas) tablero-panel))))

;; Punto de entrada para redibujar el tablero completo.
;; Siempre limpia antes de dibujar para evitar que widgets
;; del estado anterior queden visibles sobre los nuevos.
(define (actualizar-tablero! tablero-panel matriz)
  (limpiar-panel tablero-panel)
  (dibujar-filas matriz tablero-panel))

;; El panel de información se reconstruye junto con el tablero
;; en cada jugada, de modo que el puntaje siempre refleje el
;; estado actual sin necesidad de mantener una referencia al
;; widget de puntaje entre llamadas.
(define (actualizar-info! info-panel estado)
  (limpiar-panel info-panel)
  (new message%
       (parent info-panel)
       (label (string-append "Puntaje: " (number->string (calcular-puntaje estado))))
       (font (make-font #:size 20 #:weight 'bold)))
  (new message%
       (parent info-panel)
       (label "Flechas o WASD para mover")
       (font (make-font #:size 12))))

;; El diálogo de victoria se crea y muestra en el momento en que
;; se detecta la condición ganadora, bloqueando la ventana padre
;; hasta que el usuario lo cierre. No altera el estado del juego.
(define (mostrar-ganador! ventana-padre)
  (define ventana (new dialog%
                       (label "¡Ganaste!")
                       (parent ventana-padre)
                       (width 300)
                       (height 150)))
  (new message%
       (parent ventana)
       (label "¡Llegaste a 2048!")
       (font (make-font #:size 20 #:weight 'bold)))
  (new button%
       (parent ventana)
       (label "Cerrar")
       (callback (lambda (b e) (send ventana show #f))))
  (send ventana show #t))


;; -------------------------------------------------------------
;; CLASE PRINCIPAL: juego%
;;
;; Se modela el juego como una clase que extiende frame% en lugar
;; de definir el frame y el estado por separado a nivel global.
;; Esto permite que el estado del tablero quede encapsulado como
;; un campo de la instancia, visible solo dentro de la clase, en
;; lugar de flotar como una variable global mutable.
;;
;; La lógica de cada movimiento sigue el flujo:
;;   estado actual → función pura de logic.rkt → estado nuevo
;;                → insertar tile aleatorio → actualizar vista
;;
;; Ese flujo vive en procesar-movimiento!, que es el único lugar
;; del archivo donde el estado cambia.
;; -------------------------------------------------------------

(define juego%
  (class frame%
    (field (estado (insertar-n-2 (basematriz 10) 2)))

    (super-new (label "2048")
               (width 1200)
               (height 900)
               (style '(no-resize-border)))

    (define main-panel
      (new horizontal-panel%
           (parent this)
           (stretchable-width #t)
           (stretchable-height #t)))

    (define tablero-panel
      (new vertical-panel%
           (parent main-panel)
           (style '(border))
           (stretchable-width #t)
           (stretchable-height #t)))

    (define info-panel
      (new vertical-panel%
           (parent main-panel)
           (min-width 300)
           (style '(border))
           (stretchable-width #f)
           (stretchable-height #t)))

    ;; fn-mover es una función de logic.rkt (mover-arriba, etc.).
    ;; Se pasa como argumento para que procesar-movimiento! no
    ;; necesite conocer qué tecla se presionó — eso lo resuelve
    ;; on-subwindow-char antes de llamar acá.
    (define (procesar-movimiento! fn-mover)
      (define nuevo-estado (insertar-n-2 (fn-mover estado) 1))
      (set-field! estado this nuevo-estado)
      (actualizar-info! info-panel nuevo-estado)
      (actualizar-tablero! tablero-panel nuevo-estado)
      (cond
        ((hay-ganador? nuevo-estado) (mostrar-ganador! this))
        (else (void))))

    ;; Se aceptan tanto las flechas del teclado (requerimiento
    ;; explícito del enunciado) como WASD como alternativa.
    ;; Cualquier otra tecla se pasa al manejador del padre para
    ;; no interferir con el comportamiento estándar del frame.
    (define/override (on-subwindow-char receiver event)
      (define tecla (send event get-key-code))
      (cond
        ((or (equal? tecla 'up)    (equal? tecla #\w)) (procesar-movimiento! mover-arriba)    #t)
        ((or (equal? tecla 'down)  (equal? tecla #\s)) (procesar-movimiento! mover-abajo)     #t)
        ((or (equal? tecla 'left)  (equal? tecla #\a)) (procesar-movimiento! mover-izquierda) #t)
        ((or (equal? tecla 'right) (equal? tecla #\d)) (procesar-movimiento! mover-derecha)   #t)
        (else (super on-subwindow-char receiver event))))

    ;; iniciar! se llama una vez después de crear la instancia
    ;; para poblar los paneles con el estado inicial. No se hace
    ;; en el constructor porque los paneles deben existir primero.
    (define/public (iniciar!)
      (actualizar-info! info-panel estado)
      (actualizar-tablero! tablero-panel estado))))



;; ARRANQUE


(define ventana (new juego%))
(send ventana iniciar!)
(send ventana show #t)