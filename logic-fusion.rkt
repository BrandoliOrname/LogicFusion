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
;;                | "(" expresion primitiva-binaria expresion ")"
;;                | primitiva-unaria "(" expresion ")"
;;
;; primitiva-binaria ::= "+" | "~" | "/" | "*" | "concat"
;;                     | ">" | "<" | ">=" | "<=" | "!=" | "=="
;;
;; primitiva-unaria  ::= "longitud" | "add1" | "sub1" | "neg"

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
    (identificador (letter (arbno (or letter digit "?" "-"))) symbol)
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
    (tipo ("proc") proc-tipo)
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

    ;; Expresiones de lista
    ; 1. empty
    (expresion ("empty") lista-vacia)
    ; 2. empty?(lst)
    (expresion ("empty?" "(" expresion ")") vacio-predicado)
    ; 3. make-list(elem, lst)
    (expresion ("make-list" "(" expresion "," expresion ")" ) crear-lista)
    ; 4. list?(lst)
    (expresion ("list?" "(" expresion ")" ) lista-predicado)
    ; 5. head(lst) 
    (expresion ("head" "(" expresion ")" ) cabeza-lista)
    ; 6. tail(lst)
    (expresion ("tail" "(" expresion ")" ) cola-lista)
    ; 7. append(lst1, lst2)
    (expresion ("append" "(" expresion "," expresion ")" ) concatenar-listas)
    ; 8. ref-list(lst, pos)
    (expresion ("ref-list" "(" expresion "," expresion ")" ) referencia-lista)
    ; 9. length(lst)
    (expresion ("length" "(" expresion ")" ) longitud-lista)
    
    
    ;; Aplicación de primitivas: la binaria va infija y entre paréntesis,
    ;; la unaria va prefija con su operando entre paréntesis.
    ;; Nota: se eliminó (expresion ("(" expresion ")") parentesis-exp) porque
    ;; SLLGEN exige gramática LL(1) y esa producción empieza igual que la
    ;; binaria. La agrupación ya la da la propia forma "( e1 op e2 )".
    (expresion ("(" expresion primitiva-binaria expresion ")") primitiva-binaria-expresion)
    (expresion (primitiva-unaria "(" expresion ")") primitiva-unaria-expresion)

    ;; Primitivas binarias
    (primitiva-binaria ("+") suma)
    (primitiva-binaria ("~") resta)
    (primitiva-binaria ("/") division)
    (primitiva-binaria ("*") multiplicacion)
    (primitiva-binaria ("concat") concat)
    (primitiva-binaria (">") mayor)
    (primitiva-binaria ("<") menor)
    (primitiva-binaria (">=") mayor-igual)
    (primitiva-binaria ("<=") menor-igual)
    (primitiva-binaria ("!=") diferente)
    (primitiva-binaria ("==") igual)

    ;; Primitivas unarias
    (primitiva-unaria ("longitud") longitud)
    (primitiva-unaria ("add1") add1)
    (primitiva-unaria ("sub1") sub1)
    (primitiva-unaria ("neg") negacion-booleana)
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
(define evaluar-programa 
  (lambda(pgm)
    (cases programa pgm
      (un-programa (global-defs program-defs)
                   (let* ((ambiente-nuevo (recorrer-lista-definicion global-defs (ambiente-vacio))))
                          (recorrer-lista-definicion program-defs ambiente-nuevo))
        )
      )
    )
  )

(define recorrer-lista-definicion
  (lambda (defs amb)
    (if (null? defs) amb
        (recorrer-lista-definicion (cdr defs) (evaluar-definicion (car defs) amb))
    )
  )
 )


(define evaluar-definicion
  (lambda (def ambiente)
    (cases definicion def
      (variable-definicion (tipo identificador expresion)
                           (let ((valor-inicial (evaluar-expresion expresion ambiente) ))
                             ;si la validacion es correcta, extiende el ambiente
                             (if (validar-tipo tipo valor-inicial)
                                 (ambiente-extendido
                                  (vector tipo)
                                  (list identificador)
                                  (vector valor-inicial)
                                  (vector #f)
                                  ambiente)

                                (eopl:error 'evaluar-definicion "La variable ~s es de tipo ~s y recibio ~s"  identificador tipo valor-inicial))))

      (constante-definicion (tipo identificador expresion)
                            (let ((valor-constante (evaluar-expresion expresion ambiente)))
                              (if (validar-tipo tipo valor-constante)
                                  (ambiente-extendido
                                   (vector tipo)
                                   (list identificador)
                                   (vector valor-constante)
                                   (vector #t)
                                   ambiente)

                                  (eopl:error 'evaluar-definicion "La constante ~s es de tipo ~s y recibio ~s" identificador tipo valor-constante)))))
                            )
                          )

;falta completar con los tipos compuestos y  el caso 2, cuando se modifica en el ambiente (asignar, suponmgo que el set del profesor, nosotros usamos "->")
(define validar-tipo
  (lambda (tipo-dato valor)
    (cases tipo tipo-dato
      (entero-tipo () (entero? valor)) ;reutilizamos un predicado de nosotros
      (decimal-tipo () (real? valor)) ;para flotantes
      (cadena-tipo () (string? valor))
      (booleano-tipo () (boolean? valor))
      (void-tipo () #t)
      ;abajo iran los tipos compuestos
      (else (eopl:error 'validar-tipo "tipo ~s no soportdado aun" tipo-dato)
      )
    )
  )
)



(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (n) n) 
      (variable-exp (id) (aplicar-ambiente amb id)) 
      (true-lit () #t)
      (false-lit () #f)
      ;; Primitivas (Parte 1 - Brandon): se evalúan los operandos (de izquierda
      ;; a derecha) y luego se aplica la primitiva sobre los valores resultantes.
      (primitiva-binaria-expresion (exp1 prim exp2)
                                   (evaluar-primitiva-binaria prim
                                                              (evaluar-expresion exp1 amb)
                                                              (evaluar-expresion exp2 amb)))
      (primitiva-unaria-expresion (prim operando)
                                  (evaluar-primitiva-unaria prim
                                                            (evaluar-expresion operando amb)))
      ;; Expresiones de lista
      ; 1. empty
      (lista-vacia () '())
      ; 2. empty?(lst)
      (vacio-predicado (lista)
                       (null? (evaluar-expresion lista amb)))
      ; 3. make-list(elem, lst)
      (crear-lista (elemento lista)
                   (let ((cabeza (evaluar-expresion elemento amb))
                         (cola (evaluar-expresion lista amb)))
                     (cons cabeza cola)))
      ; 4. list?(lst)
      (lista-predicado (lista)
                       (list? (evaluar-expresion lista amb)))
      ; 5. head(lst)
      (cabeza-lista (lista)
                    (let ((l (evaluar-expresion lista amb)))
                      (if (null? l)
                          (eopl:error 'cabeza-lista "cabeza de la lista vacia")
                          (car l))))
      ; 6. tail(lst)
      (cola-lista (lista)
                  (let ((l (evaluar-expresion lista amb)))
                    (if (null? l)
                        (eopl:error 'cola-lista "cola de la lista vacia")
                        (cdr l))))
      ; 7. append(lst1, lst2)
      (concatenar-listas (lista1 lista2)
                         (let ((l1 (evaluar-expresion lista1 amb))
                               (l2 (evaluar-expresion lista2 amb)))
                           (if (and (list? l1) (list? l2))
                               (append l1 l2)
                               (eopl:error 'concatenar-listas "Los parametros no son listas"))))
                               
      ; 8. ref-list(lst, pos)
      (referencia-lista (lista posicion)
                        (let ((l (evaluar-expresion lista amb))
                              (pos (evaluar-expresion posicion amb)))
                          (if (or (< pos 0) (>= pos (length l)))
                              (eopl:error 'referencia-lista "posicion fuera del rango ~s" pos)
                              (list-ref l pos))))
      ; 9. length(lst)
      (longitud-lista (lista)
                      (let ((l (evaluar-expresion lista amb)))
                        (if (list? l)
                            (length l)
                            (eopl:error 'longitud-lista "No es una lista ~s" l))))
      
      
                     
      (else (eopl:error 'evaluar-expresion "expresión no soportada: ~s" exp))
    )
  )
)

;; ============================================================================
;; PRIMITIVAS (Parte 1 - Brandon)
;; ============================================================================
;; Base: evaluar-primitiva-binaria, evaluar-primitiva-unaria y condicional? de
;; interpretador.txt, adaptadas a las reglas del enunciado de LogicFusion:
;;   - +, ~, *, / operan sobre int y float.
;;   - Las relacionales (==, !=, >, <, >=, <=) retornan bool (#t / #f) y
;;     funcionan con int, float y bool.
;;   - concat y longitud operan sobre cadenas.
;;   - Si un operando no es del tipo esperado se lanza un error claro.
;; Los valores son los de Racket: int = entero exacto, float = decimal,
;; string = cadena, bool = #t / #f.

; evaluar-primitiva-binaria:
; Proposito:
; PB x V x V -> V : Procedimiento que recibe una primitiva binaria y los valores
; ya evaluados de sus dos operandos, y retorna el resultado de aplicarla.
; Suma, resta (~), multiplicacion y division operan sobre numeros; concat une
; dos cadenas; las comparaciones delegan en condicional? y retornan #t o #f.
;
; Gramatica:
;   <primitiva-binaria> := + (suma) | ~ (resta) | * (multiplicacion) | / (division)
;                       := concat (concat)
;                       := > (mayor) | < (menor) | >= (mayor-igual) | <= (menor-igual)
;                       := != (diferente) | == (igual)
(define evaluar-primitiva-binaria
  (lambda (prim valor1 valor2)
    (cases primitiva-binaria prim
      (suma ()           (operar-numeros '+ + valor1 valor2))
      (resta ()          (operar-numeros '~ - valor1 valor2))
      (multiplicacion () (operar-numeros '* * valor1 valor2))
      (division ()       (dividir valor1 valor2))
      (concat ()         (concatenar valor1 valor2))
      (mayor ()          (condicional? '>  valor1 valor2))
      (menor ()          (condicional? '<  valor1 valor2))
      (mayor-igual ()    (condicional? '>= valor1 valor2))
      (menor-igual ()    (condicional? '<= valor1 valor2))
      (diferente ()      (condicional? '!= valor1 valor2))
      (igual ()          (condicional? '== valor1 valor2)))))

; evaluar-primitiva-unaria:
; Proposito:
; PU x V -> V : Procedimiento que recibe una primitiva unaria y el valor ya
; evaluado de su operando, y retorna el resultado de aplicarla. longitud retorna
; la cantidad de caracteres de una cadena; add1 y sub1 suman y restan 1 a un
; numero (int o float); neg retorna la negacion booleana.
;
; Gramatica:
;   <primitiva-unaria> := longitud (longitud) | add1 (add1) | sub1 (sub1)
;                      := neg (negacion-booleana)
(define evaluar-primitiva-unaria
  (lambda (prim valor)
    (cases primitiva-unaria prim
      (longitud ()          (longitud-de-cadena valor))
      (add1 ()              (operar-numero 'add1 (lambda (n) (+ n 1)) valor))
      (sub1 ()              (operar-numero 'sub1 (lambda (n) (- n 1)) valor))
      (negacion-booleana () (negar-valor valor)))))

;; ----------------------------------------------------------------------------
;; Auxiliares de las primitivas
;; ----------------------------------------------------------------------------
;; Nota: no se pueden llamar suma, resta, division, multiplicacion, concat,
;; mayor, menor, mayor-igual, menor-igual, igual, diferente, longitud, add1,
;; sub1 ni negacion-booleana porque ya son constructores de la gramática.

; operar-numeros:
; Proposito:
; Sym x Proc x V x V -> N : Procedimiento que recibe el simbolo de la primitiva
; (solo para el mensaje de error), una operacion de Racket y dos valores. Si
; ambos son numeros (int o float) retorna la operacion aplicada a ellos; si no,
; lanza un error. Lo usan +, ~ y *.
(define operar-numeros
  (lambda (simbolo operacion valor1 valor2)
    (if (and (number? valor1) (number? valor2))
        (operacion valor1 valor2)
        (eopl:error 'evaluar-primitiva-binaria
                    "La primitiva ~s solo opera sobre int o float, se recibio: ~s y ~s"
                    simbolo valor1 valor2))))

; operar-numero:
; Proposito:
; Sym x Proc x V -> N : Version de operar-numeros para un solo operando. Lo usan
; add1 y sub1.
(define operar-numero
  (lambda (simbolo operacion valor)
    (if (number? valor)
        (operacion valor)
        (eopl:error 'evaluar-primitiva-unaria
                    "La primitiva ~s solo opera sobre int o float, se recibio: ~s"
                    simbolo valor))))

; dividir:
; Proposito:
; V x V -> N : Procedimiento que divide dos numeros. Si ambos son enteros
; retorna la division entera (int / int = int, como en C); si alguno es float
; retorna la division decimal. Se usa quotient y no / porque en Racket
; (/ 7 2) da la fraccion 7/2, que no es ni int ni float y romperia la
; validacion de tipos. Si el divisor es cero lanza un error.
(define dividir
  (lambda (valor1 valor2)
    (cond
      ((not (and (number? valor1) (number? valor2)))
       (eopl:error 'evaluar-primitiva-binaria
                   "La primitiva / solo opera sobre int o float, se recibio: ~s y ~s"
                   valor1 valor2))
      ((zero? valor2)
       (eopl:error 'evaluar-primitiva-binaria "Division por cero: ~s / ~s" valor1 valor2))
      ((and (entero? valor1) (entero? valor2)) (quotient valor1 valor2))
      (else (/ valor1 valor2)))))

; entero?:
; Proposito:
; V -> B : Retorna #t si el valor es un entero exacto (int) y #f en otro caso
; (por ejemplo 4.0 es float, no int).
(define entero?
  (lambda (valor)
    (and (integer? valor) (exact? valor))))

; concatenar:
; Proposito:
; V x V -> S : Procedimiento que une dos cadenas. Si algun operando no es una
; cadena lanza un error.
(define concatenar
  (lambda (valor1 valor2)
    (if (and (string? valor1) (string? valor2))
        (string-append valor1 valor2)
        (eopl:error 'evaluar-primitiva-binaria
                    "La primitiva concat solo opera sobre string, se recibio: ~s y ~s"
                    valor1 valor2))))

; longitud-de-cadena:
; Proposito:
; V -> N : Retorna la cantidad de caracteres de una cadena. Si el valor no es
; una cadena lanza un error.
(define longitud-de-cadena
  (lambda (valor)
    (if (string? valor)
        (string-length valor)
        (eopl:error 'evaluar-primitiva-unaria
                    "La primitiva longitud solo opera sobre string, se recibio: ~s"
                    valor))))

; negar-valor:
; Proposito:
; V -> B : Retorna la negacion booleana de un valor. Para un bool es #t <-> #f;
; para un numero sigue la convencion del lenguaje (0 es falso, cualquier otro
; valor es verdadero), asi que neg de 0 es #t y neg de cualquier otro numero
; es #f. Cualquier otro valor lanza un error.
(define negar-valor
  (lambda (valor)
    (cond
      ((boolean? valor) (not valor))
      ((number? valor) (zero? valor))
      (else (eopl:error 'evaluar-primitiva-unaria
                        "La primitiva neg solo opera sobre bool o numeros, se recibio: ~s"
                        valor)))))

; condicional?:
; Proposito:
; Sym x V x V -> B : Procedimiento que recibe el simbolo de un operador de
; comparacion (>, <, >=, <=, != o ==) y dos valores, y retorna #t si la
; comparacion se cumple y #f si no. Funciona con int, float y bool (False es
; menor que True); no se pueden comparar valores de categorias distintas (por
; ejemplo un int con un bool). Los numeros se comparan por valor, asi que
; (1 == 1.0) es #t. Es la condicional? de interpretador.txt, pero comparando
; directamente (sin restar los operandos) y retornando bool en vez de 1 / 0.
(define condicional?
  (lambda (operador valor1 valor2)
    (cond
      ((and (number? valor1) (number? valor2))
       (comparar-numeros operador valor1 valor2))
      ((and (boolean? valor1) (boolean? valor2))
       (comparar-numeros operador (if valor1 1 0) (if valor2 1 0)))
      (else
       (eopl:error 'evaluar-primitiva-binaria
                   "La primitiva ~s solo compara int, float o bool del mismo tipo, se recibio: ~s y ~s"
                   operador valor1 valor2)))))

; comparar-numeros:
; Proposito:
; Sym x N x N -> B : Aplica el operador de comparacion a dos numeros. Si el
; operador no es soportado lanza un error.
(define comparar-numeros
  (lambda (operador n1 n2)
    (cond
      ((equal? '>  operador) (>  n1 n2))
      ((equal? '<  operador) (<  n1 n2))
      ((equal? '>= operador) (>= n1 n2))
      ((equal? '<= operador) (<= n1 n2))
      ((equal? '== operador) (=  n1 n2))
      ((equal? '!= operador) (not (= n1 n2)))
      (else (eopl:error 'condicional? "Operador no soportado ~s" operador)))))

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




;; ============================================================================
;; EJEMPLOS DE scan&parse PARA LAS PRODUCCIONES DE PRIMITIVAS
;; (se dejan comentados para que no se ejecuten al cargar el archivo)
;; ============================================================================
;; primitiva-binaria-expresion (una por cada primitiva binaria):
;; (scan&parse "GLOBALS { int a = (5 + 3); } PROGRAM { }")             ; suma
;; (scan&parse "GLOBALS { int a = (5 ~ 3); } PROGRAM { }")             ; resta
;; (scan&parse "GLOBALS { int a = (5 * 3); } PROGRAM { }")             ; multiplicacion
;; (scan&parse "GLOBALS { int a = (6 / 3); } PROGRAM { }")             ; division
;; (scan&parse "GLOBALS { string s = (\"ho\" concat \"la\"); } PROGRAM { }") ; concat
;; (scan&parse "GLOBALS { bool b = (5 > 3); } PROGRAM { }")            ; mayor
;; (scan&parse "GLOBALS { bool b = (5 < 3); } PROGRAM { }")            ; menor
;; (scan&parse "GLOBALS { bool b = (5 >= 3); } PROGRAM { }")           ; mayor-igual
;; (scan&parse "GLOBALS { bool b = (5 <= 3); } PROGRAM { }")           ; menor-igual
;; (scan&parse "GLOBALS { bool b = (5 != 3); } PROGRAM { }")           ; diferente
;; (scan&parse "GLOBALS { bool b = (5 == 3); } PROGRAM { }")           ; igual
;; (scan&parse "GLOBALS { int a = ((2 + 3) * (10 ~ 4)); } PROGRAM { }") ; anidadas
;;
;; primitiva-unaria-expresion (una por cada primitiva unaria):
;; (scan&parse "GLOBALS { int n = longitud(\"hola\"); } PROGRAM { }")   ; longitud
;; (scan&parse "GLOBALS { int a = add1(7); } PROGRAM { }")             ; add1
;; (scan&parse "GLOBALS { int a = sub1(7); } PROGRAM { }")             ; sub1
;; (scan&parse "GLOBALS { bool b = neg(True); } PROGRAM { }")          ; negacion-booleana
;; (scan&parse "GLOBALS { int a = add1(sub1((1 + 2))); } PROGRAM { }") ; anidadas

;; ============================================================================
;; EJEMPLOS DE scan&parse PARA LAS PRODUCCIONES DE LISTAS
;; (se dejan comentados para que no se ejecuten al cargar el archivo)
;; ============================================================================

;; (scan&parse "GLOBALS { } PROGRAM { list<int> nums = empty; }")  ; lista vacia
;; (scan&parse "GLOBALS { } PROGRAM { bool b = empty?(empty); }")                ; True - lista vacia
;; (scan&parse "GLOBALS { } PROGRAM { bool b = empty?(make-list(1, empty)); }")  ; False - lista con elementos
;; (scan&parse "GLOBALS { } PROGRAM { list<int> nums = make-list(1, empty); }")                             ; (1)
;; (scan&parse "GLOBALS { } PROGRAM { list<int> nums = make-list(1, make-list(2, make-list(3, empty))); }") ; (1 2 3)
;; (scan&parse "GLOBALS { } PROGRAM { bool b = list?(empty); }")                 ; True
;; (scan&parse "GLOBALS { } PROGRAM { bool b = list?(make-list(1, empty)); }")   ; True
;; (scan&parse "GLOBALS { } PROGRAM { bool b = list?(42); }")                    ; False
;; (scan&parse "GLOBALS { } PROGRAM { int x = head(make-list(10, make-list(20, empty))); }") ; 10
;; (scan&parse "GLOBALS { } PROGRAM { int x = head(empty); }")                               ; error: cabeza de lista vacia
;; (scan&parse "GLOBALS { } PROGRAM { list<int> t = tail(make-list(10, make-list(20, make-list(30, empty)))); }")          ; (20 30)
;; (scan&parse "GLOBALS { } PROGRAM { list<int> t = tail(empty); }")                                                       ; error: cola de lista vacia
;; (scan&parse "GLOBALS { } PROGRAM { list<int> ab = append(make-list(1, make-list(2, empty)), make-list(3, make-list(4, empty))); }") ; (1 2 3 4)
;; (scan&parse "GLOBALS { } PROGRAM { list<int> ab = append(empty, make-list(1, empty)); }")                               ; (1)
;; (scan&parse "GLOBALS { } PROGRAM { list<int> ab = append(empty, empty); }")                                             ; ()
;; (scan&parse "GLOBALS { } PROGRAM { int x = ref-list(make-list(10, make-list(20, make-list(30, empty))), 0); }")         ; 10
;; (scan&parse "GLOBALS { } PROGRAM { int x = ref-list(make-list(10, make-list(20, make-list(30, empty))), 1); }")         ; 20
;; (scan&parse "GLOBALS { } PROGRAM { int x = ref-list(make-list(10, make-list(20, make-list(30, empty))), 2); }")         ; 30
;; (scan&parse "GLOBALS { } PROGRAM { int x = ref-list(make-list(10, empty), 5); }")                                       ; error: posicion fuera de rango
;; (scan&parse "GLOBALS { } PROGRAM { int n = length(empty); }")                                                           ; 0
;; (scan&parse "GLOBALS { } PROGRAM { int n = length(make-list(1, make-list(2, make-list(3, empty)))); }")                 ; 3

;; Anidadas
;; (scan&parse "GLOBALS { } PROGRAM { int x = head(tail(make-list(10, make-list(20, make-list(30, empty))))); }")          ; 20
;; (scan&parse "GLOBALS { } PROGRAM { bool b = empty?(tail(make-list(1, empty))); }")                                      ; True - tail de lista de un elemento es empty
;; ============================================================================
;Esta funcion debe conocer a todo lo demas del interpretador, por eso va al final.
(define logicfusion-interpretador
  (sllgen:make-rep-loop  "-->" evaluar-programa
                         (sllgen:make-stream-parser
                          scanner-spec-logicfusion
                          gramatica-logicfusion)))

 (logicfusion-interpretador)
