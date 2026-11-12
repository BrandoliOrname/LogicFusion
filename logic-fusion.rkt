#lang eopl
(require racket/string)


;*******************************************************************************************
; Interpretador

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>          := <expresion>
;;                         un-program (exp)
;;  <expresion>         := <numero>
;;                         numero-lit (num)
;;                      := <texto>
;;                         text-lit (txt)
;;                      := <identificador>
;;                         var-exp (id)
;;                      := (<expresion> <primitiva-binaria> <expresion>)
;;                         primapp-bin-exp (exp1 prim-binaria exp2)
;;                      := <primitiva-unaria> (<expresion>)
;;                         primapp-un-exp (prim-unaria exp)
;;                      := si <expresion> {<expresion>} sino {<expresion>}
;;                         condicional-exp
;;                      := declarar (arbno <var-exp> = <exp> ;) {<exp>}
;;                         variable-local-exp
;;                      := procedimiento (separated-list <identificador> ,) {<exp>}
;;                      := procedmiento-exp
;;                      := evaluar
;;                      := letrec {<identificador> [ {<identificador>}*(,) ] = <expresion> ;}* in <expresion> ;
;;                         <letrec-exp proc-names idss bodies letrec-body>

;                       Nota: los corchetes [ ] y los ; son terminales literales en el letrec, asi implementamos la recursion.

;;  <primitiva-binaria> := + (primitiva-suma)
;;                      := ~ (primitiva-resta)
;;                      := / (primitiva-div)
;;                      := * (primitiva-multi)
;;                      := concat (primitiva-concat)
;;                      := > (primitiva-mayor)
;;                      := < (primitiva-menor)
;;                      := >= (primitiva-mayor-igual)
;;                      := <= (primitiva-menor-igual)
;;                      := =! (primitiva-diferente)
;;                      := == (primitiva-comparador-igual)

;;  <primitiva-unaria>  := longitud (primitiva-longitud)
;;                      := add1 (primitiva-add1)
;;                      := sub1 (primitiva-sub1)
;;                      := neg (primitiva-negacion-booleana)

;*******************************************************************************************


;*******************************************************************************************
; Especificación Léxica

; Instrucciones para crear la especificación léxica
; <scanner-spec      := ({<exp-reg-y-acción>}*)
; <exp-reg-y-acción> := (<nombre> ({<exp-reg>}*) <salida>)
; <nombre>           := <símbolo>
; <exp-reg>          := <cadena> | letter | digit | whitespace | any
;                    := (not <caracter>) | (or {<exp-reg>}*)
;                    := (arbno <exp-reg>) | (concat {<exp-reg>}*)
; <salida>           := skip | symbol | number | string

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

;*******************************************************************************************

;*******************************************************************************************
; Especificacion Sintáctica (Gramatica)

; Instrucciones para crear la especificación sintática
; <gramática>   := ({<producción>}*)
; <producción>  := (<lhs> ({rhs-item>}*) <nombre-prod>)
; <lhs>         := <símbolo>
; <rhs-item>    := <símbolo> | <cadena>
;               := (arbno {<rhs-item>}*)
;               := (separated-list {<rhs-item>}* <cadena>)
; <nombre-prod> := <símbolo>

; Nota: rhs = nombre de la especificación lexica si solo si esta definido en la léxica

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

    ;; Expresiones base
    (expresion (number) entero-lit)
    (expresion (float-lit) decimal-lit)
    (expresion (string-lit) cadena-lit)
    (expresion (identifier) variable-exp)
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
;*******************************************************************************************
;
