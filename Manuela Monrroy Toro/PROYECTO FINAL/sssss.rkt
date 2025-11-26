#lang racket

(require graphics/graphics)

;; Abrir ventana
(open-graphics)
(define WIDTH 900)
(define HEIGHT 800)
(define vent (open-viewport "PRUEBA CLICK" WIDTH HEIGHT))

;; Datos del botón
(define BOTON-X 300)
(define BOTON-Y 300)
(define BOTON-W 200)
(define BOTON-H 80)

;; Fondo
(define (fondo)
  ((draw-solid-rectangle vent)
   (make-posn 0 0)
   WIDTH HEIGHT
   (make-rgb 0.25 0.71 1.0)))

;; Dibujar botón como rectángulo rojo
(define (dibujar-boton)
  ((draw-solid-rectangle vent)
   (make-posn BOTON-X BOTON-Y)
   BOTON-W BOTON-H
   (make-rgb 0.9 0.2 0.3)))

;; ¿Click dentro del botón?
(define (click-en-boton? pos)
  (and (posn? pos)
       (let ([x (posn-x pos)]
             [y (posn-y pos)])
         (display "Click en: ")   ; DEBUG
         (display x)
         (display ", ")
         (display y)
         (newline)
         (and (<= BOTON-X x (+ BOTON-X BOTON-W))
              (<= BOTON-Y y (+ BOTON-Y BOTON-H))))))

;; Esperar hasta que el click sea sobre el botón
(define (esperar-click-valido)
  (let loop ()
    (displayln "Esperando click dentro del botón...")
    (define pos (get-mouse-click vent))
    (if (click-en-boton? pos)
        (displayln "✅ Click válido, saliendo del loop.")
        (begin
          (displayln "❌ Click fuera del botón, vuelve a intentar.")
          (loop)))))

;; Pantalla de inicio
(define (pantalla-inicio)
  (clear-viewport vent)
  (fondo)
  (dibujar-boton)
  (esperar-click-valido)
  (clear-viewport vent)
  ((draw-string vent)
   (make-posn 300 400)
   "PASASTE DE PANTALLA"
   "black"))

;; 🔥 LLAMAR A LA PANTALLA
(pantalla-inicio)
