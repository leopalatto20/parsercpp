#lang racket
(require "cpp-parser.rkt")

;; Ejemplo de uso según la documentación
(define (parse-cpp-string str)
  (parse-cpp (open-input-string str)))

(define (parse-cpp-file filename)
  (call-with-input-file filename parse-cpp))

;; Función para mostrar resultados de manera legible
(define (mostrar-resultado descripcion str)
  (printf "~n=== ~a ===~n" descripcion)
  (printf "Entrada: ~s~n" str)
  (printf "Resultado: ~s~n" (parse-cpp-string str)))

;; Ejemplos de uso
(module+ main
  (mostrar-resultado "Función simple" "int main() { return 0; }")
  (mostrar-resultado "Declaraciones con inicialización" "int x = 5; float y = x + 3.2 * (10 - 4);")
  (mostrar-resultado "Función con parámetros" "int suma(int a, int b) { return a + b; }")
  (mostrar-resultado "Expresiones aritméticas" "int z = (x + y) * (a - b) / 2;"))

;; Tests unitarios
(module+ test
  (require rackunit)
  
  (check-equal? 
    (parse-cpp-string "int main() { return 0; }")
    '(program ((function int main () ((return 0))))))
  
  (check-equal?
    (parse-cpp-string "int x = 5; float y = x + 3.2 * (10 - 4);")
    '(program ((declare int ((init x 5)))
               (declare float ((init y (add x (mul 3.2 (sub 10 4)))))))))
  
  (println "Todos los tests pasaron!"))
