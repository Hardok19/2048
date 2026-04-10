#lang racket
(provide basematriz
         insertar-n-2
         mover-arriba
         mover-abajo
         mover-izquierda
         mover-derecha
         calcular-puntaje
        )

;; =============================================================
;; LÓGICA DEL JUEGO — logic.rkt
;;
;; Este módulo implementa todas las reglas del juego 2048 como
;; funciones puras. No hay efectos de lado, mutación de estado
;; ni dependencias de la interfaz gráfica. Cada función recibe
;; un valor y devuelve un valor nuevo, dejando el original
;; intacto.
;;
;; El tablero se representa como una lista de listas de números:
;; cada sublista es una fila, y cada número es el valor de una
;; celda (0 significa celda vacía). Por ejemplo, un tablero 4x4
;; con dos tiles iniciales se vería así:
;;
;;   ((2 0 0 0)
;;    (0 0 2 0)
;;    (0 0 0 0)
;;    (0 0 0 0))
;;
;; Esta representación permite usar car/cdr para navegar filas
;; y celdas sin estructuras de datos adicionales, lo cual es
;; consistente con el paradigma funcional.
;; =============================================================


;; -------------------------------------------------------------
;; CONSTRUCCIÓN DEL TABLERO
;;
;; El tablero inicial es una matriz NxN de ceros. Se construye
;; en dos pasos: primero se genera una fila de N ceros, luego
;; se repite esa fila N veces. Separar ambas responsabilidades
;; hace que cada función sea más fácil de razonar y probar.
;; -------------------------------------------------------------

(define (basematriz n)
  (basematriz-aux n n))

(define (crear-fila n)
  (cond
    ((<= n 0) '())
    (else (cons 0 (crear-fila (- n 1))))))

(define (basematriz-aux tamaño filas)
  (cond
    ((<= filas 0) '())
    (else (cons (crear-fila tamaño)
                (basematriz-aux tamaño (- filas 1))))))


;; -------------------------------------------------------------
;; INSERCIÓN DE TILES ALEATORIOS
;;
;; Después de cada jugada, el juego coloca un tile nuevo (valor 2)
;; en una celda vacía aleatoria. El enfoque elegido es: recolectar
;; primero todas las posiciones vacías en una lista de pares
;; (fila . col), seleccionar una al azar con random, y luego
;; reconstruir la matriz con ese valor modificado.
;;
;; Este enfoque evita tener que reintentar si la posición elegida
;; ya está ocupada, ya que se trabaja únicamente sobre el conjunto
;; de posiciones garantizadamente vacías.
;; -------------------------------------------------------------

(define (posiciones-vacias matriz)
  (posiciones-vacias-aux matriz 0))

(define (posiciones-vacias-aux matriz fila-idx)
  (cond
    ((null? matriz) '())
    (else (append (posiciones-vacias-fila (car matriz) fila-idx 0)
                  (posiciones-vacias-aux (cdr matriz) (+ fila-idx 1))))))

(define (posiciones-vacias-fila fila fila-idx col-idx)
  (cond
    ((null? fila) '())
    ((= (car fila) 0) (cons (cons fila-idx col-idx)
                            (posiciones-vacias-fila (cdr fila) fila-idx (+ col-idx 1))))
    (else (posiciones-vacias-fila (cdr fila) fila-idx (+ col-idx 1)))))

;; nth permite acceder a una posición arbitraria de la lista de
;; vacíos sin usar list-ref, manteniendo el estilo recursivo.
(define (nth lista n)
  (cond
    ((= n 0) (car lista))
    (else (nth (cdr lista) (- n 1)))))

;; reemplazar-col-en y reemplazar-fila reconstruyen la fila y la
;; matriz respectivamente, devolviendo estructuras nuevas en lugar
;; de modificar las existentes. Esto es lo que hace que la
;; inserción sea funcionalmente pura a pesar de "cambiar" el tablero.
(define (reemplazar-col-en fila col)
  (cond
    ((null? fila) '())
    ((= col 0) (cons 2 (cdr fila)))
    (else (cons (car fila) (reemplazar-col-en (cdr fila) (- col 1))))))

(define (insertar-en-posicion matriz par)
  (reemplazar-fila matriz
                   (car par)
                   (reemplazar-col-en (obtener-fila matriz (car par)) (cdr par))))

(define (insertar-2 matriz)
  (insertar-2-en matriz (posiciones-vacias matriz)))

(define (insertar-2-en matriz vacias)
  (cond
    ((null? vacias) matriz)
    (else (insertar-en-posicion matriz
                                (nth vacias (random (length vacias)))))))

;; insertar-n-2 permite insertar múltiples tiles en secuencia,
;; útil al inicializar el tablero con dos tiles de arranque.
;; Cada llamada recursiva trabaja sobre la matriz ya modificada.
(define (insertar-n-2 matriz cantidad)
  (cond
    ((<= cantidad 0) matriz)
    (else (insertar-n-2 (insertar-2 matriz) (- cantidad 1)))))

(define (obtener-fila matriz fila)
  (cond
    ((= fila 0) (car matriz))
    (else (obtener-fila (cdr matriz) (- fila 1)))))

(define (reemplazar-fila matriz fila nueva-fila)
  (cond
    ((null? matriz) '())
    ((= fila 0) (cons nueva-fila (cdr matriz)))
    (else (cons (car matriz)
                (reemplazar-fila (cdr matriz) (- fila 1) nueva-fila)))))


;; -------------------------------------------------------------
;; MOVIMIENTOS DEL TABLERO
;;
;; Los cuatro movimientos (izquierda, derecha, arriba, abajo) se
;; reducen todos a una misma operación base: comprimir una fila
;; hacia la izquierda. Las variaciones se logran así:
;;
;;   - Derecha:  invertir cada fila, comprimir, volver a invertir.
;;   - Arriba:   transponer el tablero, mover izquierda, transponer.
;;   - Abajo:    transponer el tablero, mover derecha, transponer.
;;
;; Esto evita duplicar la lógica de fusión para cada dirección.
;; La transposición convierte columnas en filas, permitiendo que
;; los movimientos verticales reutilicen exactamente el mismo
;; algoritmo horizontal.
;; -------------------------------------------------------------

;; invertir usa un acumulador para lograr tiempo lineal en lugar
;; de cuadrático. El acumulador actúa como una pila que va
;; construyendo el resultado de derecha a izquierda.
(define (invertir lista)
  (invertir-aux lista '()))

(define (invertir-aux lista acc)
  (cond
    ((null? lista) acc)
    (else (invertir-aux (cdr lista) (cons (car lista) acc)))))

;; obtener-primeros y obtener-restos son auxiliares de transponer.
;; Juntas permiten "girar" la matriz columna por columna sin map.
(define (obtener-primeros matriz)
  (cond
    ((null? matriz) '())
    (else (cons (car (car matriz)) (obtener-primeros (cdr matriz))))))

(define (obtener-restos matriz)
  (cond
    ((null? matriz) '())
    (else (cons (cdr (car matriz)) (obtener-restos (cdr matriz))))))

;; La transposición convierte la primera columna en la primera fila,
;; la segunda columna en la segunda fila, y así sucesivamente.
;; Se detiene cuando las filas quedan vacías (se consumieron todas
;; las columnas), condición que se detecta con (null? (car matriz)).
(define (transponer matriz)
  (transponer-aux matriz))

(define (transponer-aux matriz)
  (cond
    ((null? (car matriz)) '())
    (else
     (cons (obtener-primeros matriz)
           (transponer-aux (obtener-restos matriz))))))

;; comprimir-fila implementa la regla central del juego:
;; 1. Eliminar los ceros intercalados (filtrar-ceros).
;; 2. Fusionar pares de tiles iguales adyacentes, de izquierda a
;;    derecha. Un tile fusionado no puede volver a fusionarse en
;;    la misma jugada — esto se garantiza saltando dos posiciones
;;    con (cdr (cdr fila)) tras una fusión.
;; 3. Rellenar con ceros a la derecha hasta restaurar el ancho
;;    original de la fila.
(define (comprimir-fila fila)
  (rellenar-ceros-der (comprimir-aux (filtrar-ceros fila) '()) (length fila)))

(define (comprimir-aux fila acc)
  (cond
    ((null? fila) (invertir acc))
    ((= (car fila) 0) (comprimir-aux (cdr fila) acc))
    ((null? (cdr fila)) (invertir (cons (car fila) acc)))
    ((= (car fila) (car (cdr fila)))
     (comprimir-aux (cdr (cdr fila)) (cons (* (car fila) 2) acc)))
    (else
     (comprimir-aux (cdr fila) (cons (car fila) acc)))))

;; rellenar-ceros-der restaura el ancho de la fila tras la fusión.
;; Se calcula cuántos ceros faltan comparando el tamaño original
;; con el tamaño actual de la lista comprimida.
(define (rellenar-ceros-der lista tamaño-original)
  (agregar-ceros-al-final lista (restar-tamaños tamaño-original (length lista))))

(define (restar-tamaños a b)
  (cond
    ((< a b) 0)
    (else (- a b))))

(define (agregar-ceros-al-final lista cantidad)
  (cond
    ((null? lista) (crear-ceros-lista cantidad))
    ((<= cantidad 0) lista)
    (else (cons (car lista) (agregar-ceros-al-final (cdr lista) cantidad)))))

(define (crear-ceros-lista n)
  (cond
    ((<= n 0) '())
    (else (cons 0 (crear-ceros-lista (- n 1))))))

(define (filtrar-ceros lista)
  (cond
    ((null? lista) '())
    ((= (car lista) 0) (filtrar-ceros (cdr lista)))
    (else (cons (car lista) (filtrar-ceros (cdr lista))))))

;; procesar-filas aplica una función a cada fila de la matriz,
;; cumpliendo el rol de map pero implementado recursivamente para
;; no depender de funciones de orden superior prohibidas.
(define (procesar-filas func matriz)
  (cond
    ((null? matriz) '())
    (else (cons (func (car matriz)) (procesar-filas func (cdr matriz))))))

(define (mover-izquierda matriz)
  (procesar-filas comprimir-fila matriz))

(define (mover-derecha matriz)
  (procesar-filas revertir-comprimir-fila matriz))

(define (revertir-comprimir-fila fila)
  (invertir (comprimir-fila (invertir fila))))

(define (mover-arriba matriz)
  (transponer (mover-izquierda (transponer matriz))))

(define (mover-abajo matriz)
  (transponer (mover-derecha (transponer matriz))))


;; -------------------------------------------------------------
;; CÁLCULO DE PUNTAJE
;;
;; El puntaje de una celda se calcula a partir de su valor usando
;; suma-descomposicion, que reconstruye cuántos puntos debieron
;; haberse acumulado para producir ese tile. Por ejemplo, un tile
;; de valor 8 requirió fusionar dos 4s (cada uno fusión de dos 2s),
;; acumulando 4 + 8 = 12 puntos en el proceso.
;;
;; Este cálculo asume que todos los valores del tablero son
;; potencias de 2. Si por algún error de estado llegara un valor
;; impar, suma-descomposicion lanza un error explícito en lugar
;; de producir un resultado silenciosamente incorrecto.
;; -------------------------------------------------------------

(define (suma-descomposicion n)
  (cond
    ((<= n 2) 0)
    ((odd? n) (error "Solo potencias de 2"))
    (else (+ n (* 2 (suma-descomposicion (/ n 2)))))))

(define (puntaje-celda valor)
  (cond
    ((<= valor 0) 0)
    (else (suma-descomposicion valor))))

(define (puntaje-fila fila)
  (cond
    ((null? fila) 0)
    (else (+ (puntaje-celda (car fila))
             (puntaje-fila (cdr fila))))))

(define (calcular-puntaje matriz)
  (cond
    ((null? matriz) 0)
    (else (+ (puntaje-fila (car matriz))
             (calcular-puntaje (cdr matriz))))))