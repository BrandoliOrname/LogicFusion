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

    ;; Definiciones (globales y locales)
    (definicion (tipo identificador "=" expresion ";") variable-deficion)
    (definicion ("const" tipo identificador "=" expresion ";") constante-deficion)

    ;; Tipos (recursivos para list<T>, vector<T>, dict<T,U>)
    (tipo ("int") entero-tipo)
    (tipo ("float") decimal-tipo)
    (tipo ("string") cadena-tipo)
    (tipo ("bool") booleano-tipo)
    (tipo ("void") void-tipo)
    (tipo ("sat") sat-tipo)
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
