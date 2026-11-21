#lang eopl
(require racket/string)

;; ============================================================================
;; GRAMÁTICA BNF — LOGICFUSION
;; ============================================================================
;;
;; programa     ::= "GLOBALS" "{" { definicion } "}"
;;                  "PROGRAM" "{" { definicion } "}"
;;
;; definicion   ::= tipo identificador "=" expresion ";"
;;                | "const" tipo identificador "=" expresion ";"
;;
;; tipo         ::= "int" | "float" | "string" | "bool" | "void" | "sat" 
;;                | "proc" < nombre > = "lambda" "(" < tipos >< parametros > ")" "{" "return" < cuerpo > "}"
;;                | "list" "<" tipo ">"
;;                | "vector" "<" tipo ">"
;;                | "dict" "<" tipo "," tipo ">"
;;
;; expresion    ::= number | float-lit | string-lit | identifier
;;                | "True" | "False"
;;                | "(" expresion ")"
;;
;; expresion-aritmetica ::= "+" | "~" | "/" | "*" | "concat"
;;                        | ">" | "<" | ">=" | "<=" | "!=" | "=="
;;                        | "longitud" | "add1" | "sub1" | "neg"

;;
;; ============================================================================


;; ============================================================================
;; ESPECIFICACIÓN LÉXICA
;; ============================================================================
;;
;; Instrucciones para crear la especificación léxica
;; <scanner-spec      := ({<exp-reg-y-acción>}*)
;; <exp-reg-y-acción> := (<nombre> ({<exp-reg>}*) <salida>)
;; <nombre>           := <símbolo>
;; <exp-reg>          := <cadena> | letter | digit | whitespace | any
;;                    := (not <caracter>) | (or {<exp-reg>}*)
;;                    := (arbno <exp-reg>) | (concat {<exp-reg>}*)
;; <salida>           := skip | symbol | number | string

(define scanner-spec-logicfusion
  '((espacio-blanco (whitespace) skip)                                   
    (comentario ("%" (arbno (not #\newline))) skip)                      
    (decimal-positivo (digit (arbno digit) "." digit (arbno digit)) number)
    (decimal-negativo ("-" digit (arbno digit) "." digit (arbno digit)) number)  
    (entero-positivo (digit (arbno digit)) number)
    (entero-negativo ("-" digit (arbno digit)) number)
    (identificador (letter (arbno (or letter digit "?"))) symbol)        
    (texto ("\"" (arbno (not #\")) "\"") string) 
    ))
;; ============================================================================

;; ============================================================================
;; ESPECFICACIÓN SINTÁCTICA (GRAMATICA)
;; ============================================================================
;;
;; Instrucciones para crear la especificación sintática
;; <gramática>   := ({<producción>}*)
;; <producción>  := (<lhs> ({rhs-item>}*) <nombre-prod>)
;; <lhs>         := <símbolo>
;; <rhs-item>    := <símbolo> | <cadena>
;;               := (arbno {<rhs-item>}*)
;;               := (separated-list {<rhs-item>}* <cadena>)
;; <nombre-prod> := <símbolo>
;;
;; Nota: rhs = nombre de la especificación lexica si solo si esta definido en la léxica

(define gramatica-logicfusion
  '((programa ("GLOBALS" "{" (arbno definicion) "}" 
              "PROGRAM" "{" (arbno definicion) "}") 
             un-programa)

    ;; Definiciones (Variables y constantes)
    (definicion (tipo identificador "=" expresion ";") variable-definicion)
    (definicion ("const" tipo identificador "=" expresion ";") constante-definicion)

    ;; Tipos (recursivos para list<T>, vector<T>, dict<T,U>)
    (tipo ("int") entero-tipo)
    (tipo ("float") decimal-tipo)
    (tipo ("string") cadena-tipo)
    (tipo ("bool") booleano-tipo)
    (tipo ("void") void-tipo)
    (tipo ("sat") sat-tipo)
    (tipo ("proc" identificador "=" "lambda" "(" tipo identificador ")"
                  "{" "return" expresion "}")
          proc-tipo)
    (tipo ("list" "<" tipo ">") lista-tipo)
    (tipo ("vector" "<" tipo ">") vector-tipo)
    (tipo ("dict" "<" tipo "," tipo ">") diccionario-tipo)

    ;; Lista de valores
    (expresion ( "(" (separated-list expresion ",") ")") lista-de-valores)

    ;; Adendo - Listas
    ; 1. empty
    (expresion ("empty") lista-vacia)

    ; 2. empty? (lst)
    (expresion ("empty?" "(" expresion ")") lista-predicado)

    ; 3. make-list(elem, lst)
    (expresion ("make-list" "(" expresion  "," expresion ")" ) crear-lista)

    
                            
    
    

    ;; Expresiones base => Especificación Léxica
    (expresion (entero-negativo) numero-lit)
    (expresion (entero-positivo) numero-lit)
    (expresion (decimal-negativo) numero-lit)
    (expresion (decimal-positivo) numero-lit)
    (expresion (texto) cadena-lit)
    (expresion (identificador) variable-exp)
    (expresion ("True") true-lit)
    (expresion ("False") false-lit)
    ;; (expresion ("(" expresion ")") parentesis-exp)

    ;; Expresiones aritméticas
    (expresion-aritmetica ("+") suma)
    (expresion-aritmetica ("~") resta)
    (expresion-aritmetica ("/") division)
    (expresion-aritmetica ("*") multiplicacion)
    (expresion-aritmetica ("concat") concat)
    (expresion-aritmetica (">") mayor)
    (expresion-aritmetica ("<") menor)
    (expresion-aritmetica (">=") mayor-igual)
    (expresion-aritmetica ("<=") menor-igual)
    (expresion-aritmetica ("!=") diferente)
    (expresion-aritmetica ("==") igual)
    
    ; Primitiva Uninaria
    (expresion-aritmetica ("longitud") longitud)
    (expresion-aritmetica ("add1") add1)
    (expresion-aritmetica ("sub1") sub1)
    (expresion-aritmetica ("neg") negacion-booleana)
    ))

;; ============================================================================
; La función show-the-datatypes permite consultar las definiciones de tipos de datos
; que corresponden a la gramática.

(sllgen:make-define-datatypes scanner-spec-logicfusion gramatica-logicfusion)
(define show-the-datatypes (lambda () (sllgen:list-define-datatypes scanner-spec-logicfusion gramatica-logicfusion)))

;; ============================================================================
(define scan&parse
   (sllgen:make-string-parser
     scanner-spec-logicfusion
     gramatica-logicfusion))

(define just-scan
  (sllgen:make-string-scanner
   scanner-spec-logicfusion
   gramatica-logicfusion))
;; ============================================================================

;; ============================================================================
;; INTERPRETADOR
;; ============================================================================

(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (globales locales)
                   (let* ((ambiente-globales (evaluar-definicion globales (ambiente-vacio)))
                          (ambiente-locales (evaluar-definicion locales (ambiente-globales))))
                   ambiente-vacio)))))

(define evaluar-definicion
  (lambda (def ambiente)
    (cases expresion def
      (numero-lit (n) n)
      ;; faltan los demas casos

      ;; Adendo - lista
      ; 1. emtpy
      (lista-vacia () '())

      ; 2. emtpy?(lst)
      (lista-predicado (exp) (if (list? exp) #t #f))

      ; 3. make-list
      (crear-lista (elemento lista)
                   (let ((cabeza (evaluar-definicion elemento ambiente))
                         (cola (evaluar-definicion lista ambiente)))
                     (cons elemento lista)))          
      
      (else (eopl:error 'evaluar-definicion "expresión no soportada: ~s" exp)))))
;; ============================================================================

;; ============================================================================
;; REFERENCIAS
;; ============================================================================

; tipo de dato
(define-datatype referencia referencia?
  (a-ref (posicion integer?)
         (vector vector?)))

; leer una referencia
(define deref
  (lambda (ref)
    (primitiva-deref ref)))

(define primitiva-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (posicion vector)
             (vector-ref vector posicion)))))

; mutar una referencia -> Paso por valor
(define setref!
  (lambda (ref val)
    (primitiva-setref ref val)))

(define primitiva-setref
  (lambda (ref val)
    (cases referencia ref
      (a-ref (posicion vector)
             (vector-set! vector posicion val)))))

;; ============================================================================
;; AMBIENTES
;; ============================================================================

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido (tipos vector?)
                      (simbolos (list-of symbol?))
                      (valores vector?)
                      (es-const vector?)
                      (amb ambiente?)))

(define aplicar-ambiente-referencia
  (lambda (amb simbolo)
    (cases ambiente amb
      (ambiente-vacio ()
          (eopl:error 'aplicar-ambiente-referencia "No esta ligada para ~s" simbolo))
      (ambiente-extendido (tipos simbolos valores es-const amb)
          (let ((posicion (rib-find-position simbolo simbolos)))
            (if (number? posicion)
                (a-ref posicion valores)
                (aplicar-ambiente-referencia amb simbolo)))))))

;; ============================================================================
;; FUNCIONES AUXILIARES
;; ============================================================================

; list-index:
; Proposito:
; P x L -> N o #f : retorna la posición (desde 0) del primer elemento de L que
; cumple el predicado P, o #f si ninguno lo cumple.
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((resto (list-index pred (cdr ls))))
              (if (number? resto) (+ resto 1) #f))))))

; list-find-position:
; Proposito:
; Sym x L -> N o #f : retorna la posición (desde 0) del símbolo en la lista de
; símbolos, o #f si no está.
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (s) (eqv? s sym)) los)))

; to-do: descripcion de la funcion 
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))