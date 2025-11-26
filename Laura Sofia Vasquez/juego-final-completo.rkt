(require (lib "graphics.ss" "graphics"))
(open-graphics)
; ventana la ventana real
(define ventana (open-viewport "Farm Bee" 1180 800))
; buffer invisible
(define buffer (open-pixmap "Buffer" 1180 800))

; portada y boton play
((draw-pixmap ventana) "fondo.jpg" (make-posn 0 0))
((draw-solid-ellipse ventana) (make-posn 440 600) 300 60 "gold")
((draw-ellipse ventana) (make-posn 440 600) 300 60 "black")
((draw-string ventana) (make-posn 570 630) "PLAY")

; esperar click
(define clic (get-mouse-click ventana))
(define pos (mouse-click-posn clic))
(define x (posn-x pos))
(define y (posn-y pos))

(define (click-en-play? x y)
  (and (>= x 440) (<= x 740)
       (>= y 600) (<= y 660)))

(define (procesar-click x y)
  (if (click-en-play? x y)
      (nivel-1)
      (displayln "clic fuera del boton")))

; abejitas que funcionan como las vidas del jugador
(define (abeja-mini x y)
  ((draw-solid-ellipse buffer)(make-posn x y) 30 18 "yellow")
  ((draw-solid-rectangle buffer)(make-posn (+ x 8) (+ y 3)) 6 12 "black")
  ((draw-solid-rectangle buffer)(make-posn (+ x 18) (+ y 3)) 6 12 "black")
  ((draw-solid-ellipse buffer)(make-posn (+ x 5) (- y 8)) 15 10 "lightblue")
  ((draw-solid-ellipse buffer)(make-posn (+ x 15) (- y 8)) 15 10 "lightblue"))

(define (dibujar-vidas vidas)
  (cond
    [(= vidas 3)
     (abeja-mini 1000 20)
     (abeja-mini 1045 20)
     (abeja-mini 1090 20)]
    [(= vidas 2)
     (abeja-mini 1000 20)
     (abeja-mini 1045 20)]
    [(= vidas 1)
     (abeja-mini 1000 20)]
    [(= vidas 0) 'ok]))

; abeja en movimiento
(define (dibujar-abeja x y)
  ((draw-solid-ellipse buffer)(make-posn x y) 60 40 "yellow")
  ((draw-solid-rectangle buffer)(make-posn (+ x 10) (+ y 5)) 10 30 "black")
  ((draw-solid-rectangle buffer)(make-posn (+ x 30) (+ y 5)) 10 30 "black")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) (- y 15)) 30 20 "lightblue")
  ((draw-solid-ellipse buffer)(make-posn (+ x 5) (- y 15)) 30 20 "lightblue"))

;florecitas
(define (flor-rosa-pequena x y)
  ((draw-solid-ellipse buffer)(make-posn (- x 20) y)      20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) y)      20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn x (- y 20))      20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn x (+ y 20))      20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (- y 14)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (- y 14)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (+ y 14)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (+ y 14)) 20 20 "pink")
  ((draw-solid-ellipse buffer)(make-posn x y) 20 20 "yellow")
  ((draw-solid-rectangle buffer) (make-posn (+ x 7)(+ y 38)) 5 40 "green"))

(define (flor-margarita x y)
  ((draw-solid-ellipse buffer)(make-posn (- x 20) y)      20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) y)      20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn x (- y 20))      20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn x (+ y 20))      20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (- y 14)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (- y 14)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (+ y 14)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (+ y 14)) 20 20 "yellow")
  ((draw-solid-ellipse buffer)(make-posn x y) 20 20 "brown")
  ((draw-solid-rectangle buffer) (make-posn (+ x 7)(+ y 38)) 5 40 "green"))

(define (flor-moradita x y)
  ((draw-solid-ellipse buffer)(make-posn (- x 20) y)      20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) y)      20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn x (- y 20))      20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn x (+ y 20))      20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (- y 14)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (- y 14)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (+ y 14)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (+ y 14)) 20 20 "purple")
  ((draw-solid-ellipse buffer)(make-posn x y) 20 20 "pink")
  ((draw-solid-rectangle buffer) (make-posn (+ x 7)(+ y 38)) 5 40 "green"))

(define (flor-roja x y)
  ((draw-solid-ellipse buffer)(make-posn (- x 20) y)      20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (+ x 20) y)      20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn x (- y 20))      20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn x (+ y 20))      20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (- y 14)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (- y 14)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (- x 14) (+ y 14)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn (+ x 14) (+ y 14)) 20 20 "red")
  ((draw-solid-ellipse buffer)(make-posn x y) 20 20 "black")
  ((draw-solid-rectangle buffer) (make-posn (+ x 7)(+ y 38)) 5 40 "green"))

(define (flor-aleatoria x y)
  (cond [(= (random 4) 0)(flor-rosa-pequena x y)]
        [(= (random 4) 1)(flor-margarita x y)]
        [(= (random 4) 2)(flor-moradita x y)]
        [else (= (random 4) 3)(flor-roja x y)]))

;crea una semillas
(define (dibujar-semilla-pos x y)
  ((draw-solid-ellipse buffer)(make-posn x y) 30 50 "chocolate")
  ((draw-ellipse buffer)(make-posn x y) 30 50 "brown"))
;dibuja varias semillas a partir de la funcion anterior con diferentes posiciones
(define (dibujar-semillas)
  (dibujar-semilla-pos 200 300)
  (dibujar-semilla-pos 300 300)
  (dibujar-semilla-pos 400 300)
  (dibujar-semilla-pos 500 300)
  (dibujar-semilla-pos 600 300)
  (dibujar-semilla-pos 700 300)
  (dibujar-semilla-pos 800 300)
  (dibujar-semilla-pos 900 300))

(define (toca? abeja-x abeja-y semilla-x semilla-y)
  (and (< (abs (- abeja-x semilla-x)) 40)
       (< (abs (- abeja-y semilla-y)) 60)))

;revisa solo una semilla
(define (revisar-semilla abeja-x abeja-y semilla-x semilla-y)
  (if (toca? abeja-x abeja-y semilla-x semilla-y)
      (flor-aleatoria semilla-x semilla-y)))

;revisa varias semillas, en diferentes posiciones
(define (revisar-varias-semillas abeja-x abeja-y)
  (revisar-semilla abeja-x abeja-y 200 300)
  (revisar-semilla abeja-x abeja-y 300 300)
  (revisar-semilla abeja-x abeja-y 400 300)
  (revisar-semilla abeja-x abeja-y 500 300)
  (revisar-semilla abeja-x abeja-y 600 300)
  (revisar-semilla abeja-x abeja-y 700 300)
  (revisar-semilla abeja-x abeja-y 800 300)
  (revisar-semilla abeja-x abeja-y 900 300))

(define (loop-nivel-1)
  (begin
    ((draw-solid-rectangle buffer)(make-posn 0 0) 1180 800 "white")
    ;; fondo
    ((draw-pixmap buffer) "level.jpg" (make-posn 0 0))
    (dibujar-vidas 3)
    (dibujar-semillas)
    (let ((pos (query-mouse-posn ventana)))
      (revisar-varias-semillas (posn-x pos)(posn-y pos))
      (dibujar-abeja (posn-x pos) (posn-y pos)))
    ; copiar buffer -> ventana real
    (copy-viewport buffer ventana)
    (sleep 0.0001)
    (loop-nivel-1)))

(define (nivel-1)
  ((draw-pixmap ventana) "level.jpg" (make-posn 0 0))
  (dibujar-vidas 3)
  (dibujar-semillas)
  (sleep 0.2)
  (loop-nivel-1))

(procesar-click x y)
;YA ESTA BIEN SOLO FALTA LA FLOR SE QUEDE QUIETA CUANDO LA TOQUE Y LA MOSCA Y EL BOTON GO PA CAMBIAR DE NIVEL






