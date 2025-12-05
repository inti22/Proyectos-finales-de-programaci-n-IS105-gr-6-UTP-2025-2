#lang racket
(require 2htdp/image)
(require 2htdp/universe)

;CONSTANTES
;Dimensiones de la ventana del juego
(define WIDTH 1024)
(define HEIGHT 604)

;Configuración del juego
(define MAX-FOOD 6)          ; Número máximo de peces-comida en pantalla
(define PENDING_TICKS 4)     ; Ticks de delay antes de procesar que comiste
(define TICK-RATE 1/60)      ; 60 frames por segundo para fluidez


;SPRITES
;Pez jugador 
(define FISH-SMALL (scale 0.12 (bitmap "fish-right.png")))
(define FISH-SMALL-L (scale 0.12 (bitmap "fish-left.png")))

;Peces de comida (5 tipos diferentes, cada uno con versión derecha e izquierda)
(define FOOD-FISH-1-R (scale 0.2 (bitmap "food-fish-1-right.png")))
(define FOOD-FISH-1-L (scale 0.2 (bitmap "food-fish-1-left.png")))
(define FOOD-FISH-2-R (scale 0.1 (bitmap "food-fish-2-right.png")))
(define FOOD-FISH-2-L (scale 0.1 (bitmap "food-fish-2-left.png")))
(define FOOD-FISH-3-R (scale 0.1 (bitmap "food-fish-3-right.png")))
(define FOOD-FISH-3-L (scale 0.1 (bitmap "food-fish-3-left.png")))
(define FOOD-FISH-4-R (scale 0.1 (bitmap "food-fish-4-right.png")))
(define FOOD-FISH-4-L (scale 0.1 (bitmap "food-fish-4-left.png")))
(define FOOD-FISH-5-R (scale 0.1 (bitmap "food-fish-5-right.png")))
(define FOOD-FISH-5-L (scale 0.1 (bitmap "food-fish-5-left.png")))

;Enemigo - Medusa
(define ENEMY-SPRITE (scale 0.3 (bitmap "jellyfish.png")))


;FONDOS PRE-RENDERIZADOS (OPTIMIZACIÓN)
;O sea se crea la imagen de fondo una vez para que no se este generando constantemente :D

;Fondo del juego - escala desde 2048x1208 a las dimensiones de la ventana
(define BACKGROUND-IMAGE (scale/xy (/ WIDTH 2048) (/ HEIGHT 1208) (bitmap "ocean-bg.png")))
(define BACKGROUND-SCENE (place-image BACKGROUND-IMAGE 
                                      (/ WIDTH 2) 
                                      (/ HEIGHT 2) 
                                      (empty-scene WIDTH HEIGHT)))

;Pantalla de inicio 
(define MENU-IMAGE (scale/xy (/ WIDTH 2048) (/ HEIGHT 1208) (bitmap "menu.png")))
(define MENU-SCENE (place-image MENU-IMAGE 
                                (/ WIDTH 2) 
                                (/ HEIGHT 2) 
                                (empty-scene WIDTH HEIGHT)))

;Imagen de Game Over
(define GAMEOVER-IMAGE (scale/xy (/ WIDTH 651) (/ HEIGHT 384) (bitmap "gameover.png")))


;ESTRUCTURAS DE DATOS
;Definición de las estructuras que representan los objetos del juego

;fish: representa al pez jugador
;facing: dirección que mira ('right o 'left)
(struct fish (x y vx vy size facing) #:transparent)

;food: representa un pez-comida
;id: identificador único
;vx: velocidad horizontal (ya que se mueve solo en x)
;direction: dirección del movimiento ('right o 'left)
;fish-type: tipo de pez (1-5) para elegir sprite
(struct food (id x y vx direction fish-type) #:transparent)

;enemy: representa una medusa enemiga
;vx, vy: velocidad (se mueve en diagonal)
(struct enemy (x y vx vy) #:transparent)

;world: representa el estado completo del juego
;alive?: está vivo el jugador?
;pending-eat-id: ID del pez que estás por comer (buffer)
;pending-ticks: ticks restantes antes de procesar la comida
;next-food-id: próximo ID para asignar a nueva comida
;game-state: estado del juego ('menu, 'playing, 'gameover)
(struct world (fish foods enemies score alive?
                 pending-eat-id pending-ticks next-food-id game-state) #:transparent)

;FUNCIONES DE RENDERIZADO


;;render-fish: fish -> image
;;Devuelve la imagen del pez jugador escalada según su tamaño y orientada según la dirección que mira :b
(define (render-fish f)
  (scale (/ (fish-size f) 20)  ; Escala proporcional al tamaño
         (if (equal? (fish-facing f) 'right)
             FISH-SMALL
             FISH-SMALL-L)))

;render-food-fish: food -> image
;Devuelve la imagen correcta del pez-comida según su tipo (1-5) y dirección (right/left)
(define (render-food-fish fd)
  (define fish-type (food-fish-type fd))
  (define direction (food-direction fd))
  (cond
    [(and (= fish-type 1) (equal? direction 'right)) FOOD-FISH-1-R]
    [(and (= fish-type 1) (equal? direction 'left)) FOOD-FISH-1-L]
    [(and (= fish-type 2) (equal? direction 'right)) FOOD-FISH-2-R]
    [(and (= fish-type 2) (equal? direction 'left)) FOOD-FISH-2-L]
    [(and (= fish-type 3) (equal? direction 'right)) FOOD-FISH-3-R]
    [(and (= fish-type 3) (equal? direction 'left)) FOOD-FISH-3-L]
    [(and (= fish-type 4) (equal? direction 'right)) FOOD-FISH-4-R]
    [(and (= fish-type 4) (equal? direction 'left)) FOOD-FISH-4-L]
    [(and (= fish-type 5) (equal? direction 'right)) FOOD-FISH-5-R]
    [(and (= fish-type 5) (equal? direction 'left)) FOOD-FISH-5-L]
    [else FOOD-FISH-1-R])) ; Imagen por defecto en caso de error

;UTILIDADES MATEMÁTICAS :0
;dist: number number number number -> number
;Calcula la distancia euclidiana entre dos puntos (x1,y1) y (x2,y2)
;Usado para detectar colisiones
(define (dist x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2))
           (sqr (- y1 y2)))))

;clamp: number number number -> number
;Limita un valor entre un mínimo (lo) y un máximo (hi)
;Si val < lo, devuelve lo. Si val > hi, devuelve hi. Sino devuelve val.
;Usado para mantener objetos dentro de los límites de la pantalla
(define (clamp val lo hi)
  (max lo (min hi val)))

;GENERACIÓN DE OBJETOS (SPAWN :D)
;spawn-food: number number -> food
;Crea un nuevo pez-comida con ID único y score actual
(define (spawn-food id score)
  (define direction (if (< (random 2) 1) 'left 'right))
  (define start-x (if (equal? direction 'left) WIDTH 0))
  (define speed (+ 1 (random 3)))
  (define fish-type (cond
                      [(>= score 30) (+ 1 (random 5))]
                      [(>= score 15) (+ 1 (random 4))]
                      [else (+ 1 (random 3))]))
  (food id 
        start-x
        (+ 50 (random (- HEIGHT 100)))  ; Altura aleatoria (no en bordes)
        (if (equal? direction 'left) (- speed) speed)
        direction
        fish-type))

;spawn-enemy: -> enemy
;Crea una nueva medusa enemiga en posición aleatoria
;Velocidades aleatorias entre -3 y 3
;Nunca con velocidad 0 en ambos ejes y si sale 0 se reemplaza por -2 o 2 aleatorio >:D
;Para que todas las medusas se muevan diagonalmente c:
(define (spawn-enemy)
  (define vx-raw (- (random 6) 3))
  (define vy-raw (- (random 6) 3))
  ;Si alguna velocidad es 0, asignar valor aleatorio no-cero
  (define vx (if (= vx-raw 0)
                 (if (< (random 2) 1) -2 2)
                 vx-raw))
  (define vy (if (= vy-raw 0)
                 (if (< (random 2) 1) -2 2)
                 vy-raw))
  (enemy (random WIDTH)
         (random HEIGHT)
         vx
         vy))

;spawn-food-if-needed: list number number -> list
;Verifica si hay menos de MAX-FOOD peces en pantalla
;Si es así, genera uno nuevo y lo agrega a la lista
;Recibe: lista de comidas, próximo ID, score actual
(define (spawn-food-if-needed foods next-id score)
  (if (< (length foods) MAX-FOOD)
      (cons (spawn-food next-id score) foods)
      foods))

;spawn-enemy-if-needed: list number -> list
;Verifica cuántas medusas debe haber según el score
(define (spawn-enemy-if-needed enemies score)
  (define max-enemies (cond
                        [(>= score 30) 5]
                        [(>= score 15) 4]
                        [else 3]))
  (if (< (length enemies) max-enemies)
      (cons (spawn-enemy) enemies)
      enemies))

;MOVIMIENTO

;move-fish: fish -> fish
;Mueve el pez jugador según su velocidad
;Usa clamp para mantenerlo dentro de los límites de la pantalla
(define (move-fish f)
  (fish (clamp (+ (fish-x f) (fish-vx f)) 0 WIDTH)
        (clamp (+ (fish-y f) (fish-vy f)) 0 HEIGHT)
        (fish-vx f)
        (fish-vy f)
        (fish-size f)
        (fish-facing f)))

;move-food: food -> (or food #f)
;Mueve un pez-comida horizontalmente
;Si sale de la pantalla devuelve #f para eliminarlo
;Esto permite que se generen nuevos peces continuamente >:D
(define (move-food fd)
  (define new-x (+ (food-x fd) (food-vx fd)))
  (if (or (< new-x -50) (> new-x (+ WIDTH 50)))
      #f  ;Marcar para eliminar
      (food (food-id fd)
            new-x
            (food-y fd)
            (food-vx fd)
            (food-direction fd)
            (food-fish-type fd))))

;move-enemy: enemy -> enemy
;Cuando toca un borde, invierte su velocidad en ese eje (rebota)
;Usa clamp para mantenerla dentro de los límites
(define (move-enemy e)
  (define new-x (+ (enemy-x e) (enemy-vx e)))
  (define new-y (+ (enemy-y e) (enemy-vy e)))
  ;Invertir velocidad si toca un borde
  (define new-vx (if (or (< new-x 0) (> new-x WIDTH))
                     (- (enemy-vx e))
                     (enemy-vx e)))
  (define new-vy (if (or (< new-y 0) (> new-y HEIGHT))
                     (- (enemy-vy e))
                     (enemy-vy e)))
  (enemy (clamp new-x 0 WIDTH)
         (clamp new-y 0 HEIGHT)
         new-vx
         new-vy))

;DETECCIÓN DE COLISIONES


;detect-food-collision: fish list -> (or number #f)
;Verifica si el pez jugador está tocando algún pez-comida
;Calcula distancia entre el pez y cada comida
;Si la distancia es menor que (25 + 0.4*tamaño), hay colisión
;Devuelve el ID del primer pez-comida tocado, o #f si ninguno
(define (detect-food-collision f foods)
  (define (close? fd)
    (< (dist (fish-x f) (fish-y f)
             (food-x fd) (food-y fd))
       (+ 25 (* 0.4 (fish-size f)))))
  (let ([hit (filter close? foods)])
    (if (empty? hit) #f (food-id (first hit)))))

;fish-hit-enemies?: fish list -> boolean
;Verifica si el pez jugador está tocando alguna medusa
;Si la distancia es menor que 40 píxeles hay colisión
;Devuelve #t si toca alguna medusa, #f si no
;Cuando devuelve #t, el jugador muere
(define (fish-hit-enemies? f enemies)
  (ormap
   (λ (e)
     (< (dist (fish-x f) (fish-y f)
              (enemy-x e) (enemy-y e))
        40))
   enemies))

;SISTEMA DE COMER (BUFFER)

;process-pending-eat: world -> world
;Procesa la comida que el pez está por comer
;Remueve el pez-comida de la lista
;Aumenta el tamaño del jugador en 1 unidad por cada comida
;Incrementa el score
;Limpia el buffer de comida pendiente
(define (process-pending-eat w)
  (define pid (world-pending-eat-id w))
  (if (not pid)
      w
      (let* ([foods (world-foods w)]
             [remaining (filter (λ (fd) (not (= (food-id fd) pid))) foods)]
             [ate (- (length foods) (length remaining))]
             [f (world-fish w)]
             [new-fish (fish (fish-x f)
                             (fish-y f)
                             (fish-vx f)
                             (fish-vy f)
                             (+ (fish-size f) (* ate 1))  ; Crece 1 unidad
                             (fish-facing f))])
        (world new-fish
               remaining
               (world-enemies w)
               (+ (world-score w) ate)
               (world-alive? w)
               #f
               0
               (world-next-food-id w)
               (world-game-state w)))))

;TICK - ACTUALIZACIÓN DEL JUEGO

;tick: world -> world
;Función principal que actualiza el estado del juego cada frame
;Se ejecuta 60 veces por segundo
 
;FLUJO:
;1. Si no está en modo 'playing, no hace nada
;2. Si el jugador está muerto, no hace nada
;3. Procesa el buffer de comida pendiente (cuenta down de ticks)
;4. Mueve todos los objetos (pez, comida, medusas)
;5. Filtra comida que salió de pantalla
;6. Detecta colisiones con comida
;7. Genera nueva comida y medusas si es necesario
;8. Detecta colisiones con medusas (muerte)
;9. Si hay nueva comida detectada, inicia el buffer de delay
;10. Si el pez murió, cambia estado a 'gameover
(define (tick w)
  (if (not (equal? (world-game-state w) 'playing))
      w
      (if (not (world-alive? w))
          w
          (let* ([pending-id (world-pending-eat-id w)]
                 [pending-t (world-pending-ticks w)]
                 
                 ; Procesar buffer: decrementar contador y ejecutar cuando llegue a 0
                 [w2 (if (and pending-id (> pending-t 0))
                         (let ([new-t (sub1 pending-t)])
                           (if (= new-t 0)
                               (process-pending-eat (struct-copy world w [pending-ticks 0]))
                               (struct-copy world w [pending-ticks new-t])))
                         w)]

                 ; Mover todos los objetos
                 [f (move-fish (world-fish w2))]
                 [moved-foods-raw (map move-food (world-foods w2))]
                 [moved-foods (filter (λ (fd) fd) moved-foods-raw)]  ; Eliminar #f
                 [moved-enemies (map move-enemy (world-enemies w2))]

                 ; Detectar colisión con comida (solo si no hay comida pendiente)
                 [maybe-id (if (not (world-pending-eat-id w2))
                               (detect-food-collision f moved-foods)
                               #f)]

                 ; Generar nueva comida y medusas si es necesario
                 [foods2 (spawn-food-if-needed moved-foods
                                               (world-next-food-id w2)
                                               (world-score w2))]
                 [next-id (if (> (length foods2) (length moved-foods))
                              (+ (world-next-food-id w2) 1)
                              (world-next-food-id w2))]
                 [enemies2 (spawn-enemy-if-needed moved-enemies (world-score w2))]

                 ; Detectar muerte por medusa
                 [dead? (fish-hit-enemies? f enemies2)]

                 ; Crear nuevo estado base
                 [base (world f foods2 enemies2
                              (world-score w2)
                              (not dead?)
                              (world-pending-eat-id w2)
                              (world-pending-ticks w2)
                              next-id
                              'playing)]

                 ; Si se detectó comida nueva, iniciar buffer de delay
                 [final (if (and maybe-id (not (world-pending-eat-id w2)))
                            (struct-copy world base
                              [pending-eat-id maybe-id]
                              [pending-ticks PENDING_TICKS])
                            base)])

            ; Si murió, cambiar a estado gameover
            (if dead?
                (struct-copy world final [alive? #f] [game-state 'gameover])
                final)))))

;DIBUJO - RENDERIZADO DE LA PANTALLA

;draw-world: world -> image
;Dibuja el estado actual del juego en pantalla
;Maneja tres estados diferentes :b

;1. MENU: Muestra la pantalla de inicio
;2. GAMEOVER: Muestra el juego congelado + imagen de Game Over superpuesta
;3. PLAYING: Dibuja todo el juego (fondo, comida, medusas, pez, score)

;OPTIMIZACIÓN: Usa escenas pre-renderizadas para el fondo
(define (draw-world w)
  (cond
    [(equal? (world-game-state w) 'menu)
     MENU-SCENE]
    
    [(equal? (world-game-state w) 'gameover)
     ; Dibuja el juego exactamente como estaba al morir
     ; Luego superpone la imagen de Game Over (PNG transparente)
     (let* ([base-scene BACKGROUND-SCENE]
            [scene-with-food
             (foldl (λ (fd scene)
                      (place-image (render-food-fish fd) (food-x fd) (food-y fd) scene))
                    base-scene
                    (world-foods w))]
            [scene-with-enemies
             (foldl (λ (e scene)
                      (place-image ENEMY-SPRITE (enemy-x e) (enemy-y e) scene))
                    scene-with-food
                    (world-enemies w))]
            [f (world-fish w)]
            [scene-with-fish 
             (place-image (render-fish f) (fish-x f) (fish-y f) scene-with-enemies)]
            [scene-with-score
             (place-image (text (string-append "Score: " (number->string (world-score w))) 
                                20 "white")
                          70 30 
                          scene-with-fish)])
       ; Superponer Game Over
       (place-image GAMEOVER-IMAGE 
                    (/ WIDTH 2) 
                    (/ HEIGHT 2) 
                    scene-with-score))]
    
    [else
     ; Dibuja el juego normalmente
     ; foldl aplica place-image a cada elemento de la lista
     (let* ([base-scene BACKGROUND-SCENE]
            [scene-with-food
             (foldl (λ (fd scene)
                      (place-image (render-food-fish fd) (food-x fd) (food-y fd) scene))
                    base-scene
                    (world-foods w))]
            [scene-with-enemies
             (foldl (λ (e scene)
                      (place-image ENEMY-SPRITE (enemy-x e) (enemy-y e) scene))
                    scene-with-food
                    (world-enemies w))]
            [f (world-fish w)]
            [scene-with-fish 
             (place-image (render-fish f) (fish-x f) (fish-y f) scene-with-enemies)])
       ; Score en la esquina superior izquierda
       (place-image (text (string-append "Score: " (number->string (world-score w))) 
                          20 "white")
                    70 30 
                    scene-with-fish))]))

;MANEJO DE ENTRADA - TECLADO

;handle-key: world string -> world
;Maneja las teclas presionadas
;En el menú, las teclas no hacen nada
;Arriba/Abajo: cambia velocidad vertical (vy)
;Izquierda/Derecha: cambia velocidad horizontal (vx) y dirección (facing)
;Velocidad de 4 píxeles por frame en cada dirección
(define (handle-key w k)
  (if (equal? (world-game-state w) 'menu)
      w
      (let ([f (world-fish w)])
        (cond
          [(key=? k "up")    (struct-copy world w [fish (fish (fish-x f) (fish-y f) (fish-vx f) -4 (fish-size f) (fish-facing f))])]
          [(key=? k "down")  (struct-copy world w [fish (fish (fish-x f) (fish-y f) (fish-vx f)  4 (fish-size f) (fish-facing f))])]
          [(key=? k "left")  (struct-copy world w [fish (fish (fish-x f) (fish-y f) -4 (fish-vy f) (fish-size f) 'left)])]
          [(key=? k "right") (struct-copy world w [fish (fish (fish-x f) (fish-y f)  4 (fish-vy f) (fish-size f) 'right)])]
          [else w]))))

;handle-key-release: world string -> world
;Maneja cuando se suelta una tecla
(define (handle-key-release w k)
  (if (equal? (world-game-state w) 'menu)
      w
      (let ([f (world-fish w)])
        (cond
          [(or (key=? k "up") (key=? k "down"))
           (struct-copy world w [fish (fish (fish-x f) (fish-y f) (fish-vx f) 0 (fish-size f) (fish-facing f))])]
          [(or (key=? k "left") (key=? k "right"))
           (struct-copy world w [fish (fish (fish-x f) (fish-y f) 0 (fish-vy f) (fish-size f) (fish-facing f))])]
          [else w]))))

;MANEJO DE ENTRADA - RATÓN

;handle-mouse: world number number string -> world
;En MENU: cualquier click inicia el juego (cambia a 'playing)
;En GAMEOVER: cualquier click reinicia completamente el juego
(define (handle-mouse w x y event)
  (cond
    [(and (equal? (world-game-state w) 'menu)
          (equal? event "button-down"))
     (struct-copy world w [game-state 'playing])]
    
    [(and (equal? (world-game-state w) 'gameover)
          (equal? event "button-down"))
     ; Reiniciar todo desde cero
     (world (fish 400 300 0 0 6 'right)
            '()
            (list (spawn-enemy) (spawn-enemy) (spawn-enemy))
            0
            #t
            #f
            0
            0
            'playing)]
    
    [else w]))

;MUNDO INICIAL

;Estado inicial del juego cuando se ejecuta el programa
;Pez pequeño (tamaño 6) en el centro (400, 300)
;Sin velocidad inicial (0, 0)
;Sin comida en pantalla (se genera automáticamente)
;3 medusas enemigas
;Score en 0
;Estado: MENU (muestra pantalla de inicio)
(define initial-world
  (world (fish 400 300 0 0 6 'right)
         '()
         (list (spawn-enemy) (spawn-enemy) (spawn-enemy))
         0
         #t
         #f
         0
         0
         'menu))

;INICIAR EL JUEGO

;big-bang inicia el loop del juego con:
;to-draw: dibuja el estado actual (60 FPS)
;on-tick: actualiza el estado cada frame
;on-key: maneja teclas presionadas
;on-release: maneja teclas soltadas
;on-mouse: maneja clicks del ratón
(big-bang initial-world
  [to-draw draw-world]
  [on-tick tick TICK-RATE]
  [on-key handle-key]
  [on-release handle-key-release]
  [on-mouse handle-mouse])