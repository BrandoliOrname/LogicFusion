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

    ;; Expresiones base => Especificación Léxica
    (expresion (entero-negativo) numero-lit)
    (expresion (entero-positivo) numero-lit)
    (expresion (decimal-negativo) numero-lit)
    (expresion (decimal-positivo) numero-lit)
    (expresion (texto) cadena-lit)
    (expresion (identificador) variable-exp)
    (expresion ("True") true-lit)
    (expresion ("False") false-lit)
    (expresion ("(" expresion ")") parentesis-exp)

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

;un programa solo tiene dos cosas, una lista de definiciones globales o de la definicion de program
;es por ello que usamos ambos en cases
;Más adelante, cuando tengamos proc y main, evaluar-programa invocará a main y lo que se vera será su valor de retorno.
(define evaluar-programa ;creo que ya esta
  (lambda(pgm)
    (cases programa pgm
      (un-programa (global-defs program-defs)
                   (let* ((ambiente-nuevo (recorrer-lista-definicion global-defs (ambiente-vacio))))
                          (recorrer-lista-definicion program-defs ambiente-nuevo))    
        )
      )
    )
  )

;esta solo recorre la lista de las defs pero invoca a la fn importante evaluar-definicion
(define recorrer-lista-definicion
  (lambda (defs amb)
    (if (null? defs) amb
        ;sino, llamado recursivo a la fn, con el cuerpo de la lista de defs, y el ambiente nuevo generado en evaluar-definicion
        (recorrer-lista-definicion (cdr defs) (evaluar-definicion (car defs) amb))
    ) 
  )
 )

;evaluar-definicion devuelve solamente un ambiente extendido, pero con el valor de la expresion guardado dentro
; proximamente hay que validar los tipos (chequeo de tipos), pero solo es insertar un paso a lo que ya existe aqui.
;(se requiere crear la funcion de validar-tipos y hacer unas comparaciones)
(define evaluar-definicion
  (lambda (def ambiente)
    (cases definicion def
      (variable-definicion (tipo identificador expresion)
                            (ambiente-extendido
                             (vector tipo)
                             (list identificador)
                             (vector (evaluar-expresion expresion ambiente))
                             (vector #f)
                             ambiente))
      
      (constante-definicion (tipo identificador expresion)
                             (ambiente-extendido
                              (vector tipo)
                              (list identificador)
                              (vector (evaluar-expresion expresion ambiente))
                              (vector #t)
                              ambiente))
     )
    )
  )

(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (n) n) ;funciona
      (variable-exp (id) (aplicar-ambiente amb id)) ;funciona
      (else (eopl:error 'evaluar-expresion "expresión no soportada: ~s" exp))
    )
  )
)

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

(define aplicar-ambiente
  (lambda (amb simbolo)
    (deref (aplicar-ambiente-referencia amb simbolo))
    )
  )
       
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




;Esta funcion debe conocer a todo lo demas del interpretador, por eso va al final.
(define logicfusion-interpretador 
  (sllgen:make-rep-loop  "-->" evaluar-programa
                         (sllgen:make-stream-parser
                          scanner-spec-logicfusion
                          gramatica-logicfusion)))

(logicfusion-interpretador)