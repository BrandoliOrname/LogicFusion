#lang eopl
(require racket/string)



;; ============================================================================
;; GRAMÁTICA BNF — LOGICFUSION
;; ============================================================================
;;
;; programa     ::= "GLOBALS" "{" { definicion } "}"
;;                  "PROGRAM" "{" { definicion } "}"
;;   El resultado del programa es el valor que retorna main, que debe
;;   definirse como un proc sin parametros: proc main = lambda () { return e; };
;;
;; definicion   ::= tipo identificador "=" expresion ";"
;;                | "const" tipo identificador "=" expresion ";"
;;
;; tipo           ::= tipo-primitivo | tipo-compuesto
;; tipo-primitivo ::= "int" | "float" | "string" | "bool" | "void"
;; tipo-compuesto ::= "sat" | "proc"
;;                  | "list" "<" tipo-primitivo ">"
;;                  | "vector" "<" tipo-primitivo ">"
;;                  | "dict" "<" tipo-primitivo "," tipo-primitivo ">"
;;
;; expresion    ::= operando { primitiva-binaria operando }
;;                | "set" identificador "->" expresion
;;
;; operando     ::= dato-primitivo
;;                | identificador resto-identificador
;;                | "lambda" "(" { tipo identificador "," } ")"
;;                  "{" "return" expresion ";" "}"
;;                | "if" "(" expresion ")" "{" expresion "}"
;;                  "else" "{" expresion "}"
;;                | primitiva-unaria "(" expresion ")"
;;                | "(" contenido-parentesis ")"
;;                | "empty" | "empty?" "(" expresion ")"
;;                | "make-list" "(" expresion "," expresion ")"
;;                | "list?" "(" expresion ")"
;;                | "head" "(" expresion ")" | "tail" "(" expresion ")"
;;                | "append" "(" expresion "," expresion ")"
;;                | "ref-list" "(" expresion "," expresion ")"
;;                | "length" "(" expresion ")"
;;                | "BLOCK" "{" expresion ";" { expresion ";" } "}"
;;                | "LOCALS" "{" definicion { definicion } "}"
;;                  "{" expresion ";" { expresion ";" } "}"
;;
;; contenido-parentesis ::= <vacio> | expresion resto-parentesis
;; resto-parentesis     ::= <vacio> | "," { expresion "," }
;;   () es lista vacía, (e) agrupa, (e,) es lista de un elemento y
;;   (e1, e2, ...) es lista de varios elementos.
;;
;; resto-identificador ::= <vacio>                          (variable: x)
;;                       | "(" { expresion "," } ")"        (invocacion: f(a, b))
;;
;; dato-primitivo ::= entero | decimal | texto | "True" | "False"
;;
;; primitiva-binaria ::= "+" | "~" | "/" | "*" | "concat"
;;                     | ">" | "<" | ">=" | "<=" | "!=" | "=="
;;   Precedencia (de mayor a menor): * /  >  + ~  >  concat  >  comparaciones.
;;   Asociatividad a izquierda. Las comparaciones no se encadenan.
;;
;; primitiva-unaria  ::= "longitud" | "add1" | "sub1" | "neg"
;;
;; Nota: "set" es expresión, no operando (menor precedencia, como en C).
;; Dentro de una operación va entre paréntesis: 1 + (set y -> 5).
;; "if" si es operando: 1 + if (c) { 2 } else { 3 } es válido.
;;
;; Procedimientos (adaptacion de la guia): todos se definen igual, main incluido,
;;   proc f = lambda (int x, float y) { return e; };
;; El cuerpo es una sola expresion; para varias se usa return BLOCK {...}; o
;; return LOCALS {...} {...};. Los procedimientos son recursivos (no mutuamente).
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
    (comentario ("$" (arbno (not #\newline))) skip)
    (decimal-positivo (digit (arbno digit) "." digit (arbno digit)) number)
    (decimal-negativo ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (entero-positivo (digit (arbno digit)) number)
    (entero-negativo ("-" digit (arbno digit)) number)
    (identificador (letter (arbno (or letter digit "?" (concat "-" (or letter digit))))) symbol)
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

    (tipo (tipo-primitivo) tipo-simple)
    (tipo (tipo-compuesto) tipo-complejo)
    
    ;; Tipos Primitivos
    (tipo-primitivo ("int") entero-tipo)
    (tipo-primitivo ("float") decimal-tipo)
    (tipo-primitivo ("string") cadena-tipo)
    (tipo-primitivo ("bool") booleano-tipo)
    (tipo-primitivo ("void") void-tipo)

    ; Tipos Compuestos
    (tipo-compuesto ("sat") sat-tipo)
    (tipo-compuesto ("proc") proc-tipo)
    (tipo-compuesto ("list" "<" tipo-primitivo ">") lista-tipo)
    (tipo-compuesto ("vector" "<" tipo-primitivo ">") vector-tipo)
    (tipo-compuesto ("dict" "<" tipo-primitivo "," tipo-primitivo ">") diccionario-tipo)

    ;; Factorizar el paréntesis evita dos producciones con el mismo comienzo.
    (operando ("(" contenido-parentesis ")") operando-parentesis)
    (contenido-parentesis () contenido-vacio)
    (contenido-parentesis (expresion resto-parentesis) contenido-no-vacio)
    (resto-parentesis () agrupacion)
    (resto-parentesis ("," (separated-list expresion ",")) elementos-lista)

    ;; Expresiones base => Especificación Léxica
    (dato-primitivo (entero-negativo) numero-lit)
    (dato-primitivo (entero-positivo) numero-lit)
    (dato-primitivo (decimal-negativo) numero-lit)
    (dato-primitivo (decimal-positivo) numero-lit)
    (dato-primitivo (texto) cadena-lit)
    (dato-primitivo ("True") true-lit)
    (dato-primitivo ("False") false-lit)

    ;; Identificador: variable (x) o invocacion de procedimiento (f(a, b)).
    ;; Se factoriza (como contenido-parentesis) porque ambas empiezan con
    ;; identificador: el token siguiente decide. Por eso el identificador ya
    ;; no esta en dato-primitivo (antes era la variante variable-exp).
    (operando (identificador resto-identificador) operando-identificador)
    (resto-identificador () acceso-variable)
    (resto-identificador ("(" (separated-list expresion ",") ")") invocacion)

    ;; Procedimiento como valor (clausura). Se define como una variable de
    ;; tipo proc: proc f = lambda (int x, float y) { return x * y; };
    (operando ("lambda" "(" (separated-list tipo identificador ",") ")"
               "{" "return" expresion ";" "}")
              operando-lambda)

    ;; Expresiones de lista
    ; 1. empty
    (operando ("empty") lista-vacia)
    ; 2. empty?(lst)
    (operando ("empty?" "(" expresion ")") vacio-predicado)
    ; 3. make-list(elem, lst)
    (operando ("make-list" "(" expresion "," expresion ")" ) crear-lista)
    ; 4. list?(lst)
    (operando ("list?" "(" expresion ")" ) lista-predicado)
    ; 5. head(lst) 
    (operando ("head" "(" expresion ")" ) cabeza-lista)
    ; 6. tail(lst)
    (operando ("tail" "(" expresion ")" ) cola-lista)
    ; 7. append(lst1, lst2)
    (operando ("append" "(" expresion "," expresion ")" ) concatenar-listas)
    ; 8. ref-list(lst, pos)
    (operando ("ref-list" "(" expresion "," expresion ")" ) referencia-lista)
    ; 9. length(lst)
    (operando ("length" "(" expresion ")" ) longitud-lista)

    ;; Expresion de asignacion (modificar/actualizar variables)
    (expresion ("set" identificador "->" expresion) asignar-expresion)
    
    ;; Condicional if/else (cada rama es una expresion). Es un operando, como
    ;; BLOCK y LOCALS, para poder usarlo dentro de operaciones: 1 + if (...) {...} else {...}
    (operando ("if" "(" expresion ")" "{" expresion "}"
               "else" "{" expresion "}") operando-condicional)

    ;; Switch: compara el valor de la expresion con cada case en orden y evalua
    ;; el cuerpo del primero que coincida; si ninguno coincide evalua default.
    (operando ("switch" "(" expresion ")" "{"
               (arbno "case" expresion ":" "{" expresion "}")
               "default" ":" "{" expresion "}" "}")
              operando-switch)

    ;; BLOCK y LOCALS requieren un cuerpo con al menos una expresión.
    ;; LOCALS requiere al menos una definición.
    (operando ("BLOCK" "{" expresion ";" (arbno expresion ";") "}")
           bloque-expresion)
    (operando ("LOCALS" "{" definicion (arbno definicion) "}"
            "{" expresion ";" (arbno expresion ";") "}")
           locales-expresion)

    ;; Los operandos pueden ser datos, unarias, agrupaciones, listas o bloques.
    (expresion (operando (arbno continuacion-operacion)) expresion-operaciones)
    (continuacion-operacion (primitiva-binaria operando) una-operacion)
    (operando (dato-primitivo) operando-primitivo)
    (operando (primitiva-unaria "(" expresion ")") primitiva-unaria-expresion)

    ;; Primitivas binarias
    (primitiva-binaria ("+") suma)
    (primitiva-binaria ("~") resta)
    (primitiva-binaria ("/") division)
    (primitiva-binaria ("*") multiplicacion)
    (primitiva-binaria ("%") modulo)
    (primitiva-binaria ("concat") concat)
    (primitiva-binaria (">") mayor)
    (primitiva-binaria ("<") menor)
    (primitiva-binaria (">=") mayor-igual)
    (primitiva-binaria ("<=") menor-igual)
    (primitiva-binaria ("!=") diferente)
    (primitiva-binaria ("==") igual)
    (primitiva-binaria ("or") disyuncion)
    (primitiva-binaria ("and") conjuncion)
    

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
;El resultado del programa es lo que retorna el procedimiento main (debe estar definido).
(define evaluar-programa
  (lambda(pgm)
    (cases programa pgm
      (un-programa (global-defs program-defs)
                   (let* ((ambiente-globales (recorrer-lista-definicion global-defs (ambiente-vacio)))
                          (ambiente-final (recorrer-lista-definicion program-defs ambiente-globales)))
                     (invocar-main ambiente-final))))))

; invocar-main:
; Proposito:
; Amb -> V : Busca main en el ambiente final del programa y lo invoca sin
; argumentos; retorna su valor de retorno. Si main no existe o no es un
; procedimiento lanza un error.
(define invocar-main
  (lambda (amb)
    (if (existe-ligadura? amb 'main)
        (let ((main (aplicar-ambiente amb 'main)))
          (if (procedimiento? main)
              (aplicar-procedimiento main '() 'main)
              (eopl:error 'evaluar-programa
                          "main debe ser un procedimiento, es un ~a (~a)"
                          (nombre-tipo-valor main) (mostrar-valor main))))
        (eopl:error 'evaluar-programa
                    "El programa no tiene un procedimiento main. Definelo asi: proc main = lambda () { return ...; };"))))

;Provisional: ya no se usa (el resultado ahora es el de main). Pendiente decidir si se borra.

(define obtener-ultimo-valor
  (lambda (amb)
    (cases ambiente amb
      (ambiente-vacio ()
                      (eopl:error 'obtener-ultimo-valor "ambiente vacio"))
      (ambiente-extendido (tipos simbolos valores es-const amb-anterior)
                          (deref (a-ref 0 valores))))))
        
; recorrer-lista-definicion:
; Proposito:
; L(Def) x Amb -> Amb : Evalua las definiciones de un bloque (GLOBALS, PROGRAM o
; LOCALS) en orden, cada una en un eslabon nuevo, y retorna el ambiente final.
; Los procedimientos definidos con lambda en el bloque ven todo lo que se ha
; definido en el bloque hasta el momento (recursion y recursion mutua).
(define recorrer-lista-definicion
  (lambda (defs amb)
    (recorrer-definiciones-bloque defs amb '())))

; recorrer-definiciones-bloque:
; Proposito:
; L(Def) x Amb x L(Par) -> Amb : Hace el recorrido de recorrer-lista-definicion.
; pendientes es la lista de procedimientos del bloque definidos con lambda, como
; pares (vector-de-valores . clausura-que-se-guardo). Despues de cada definicion,
; todas esas clausuras se re-apuntan al ambiente mas reciente con
; reapuntar-clausuras. Asi, si esPar llama a esImpar y esImpar se define despues,
; cuando esImpar ya existe la clausura de esPar tambien la ve.
(define recorrer-definiciones-bloque
  (lambda (defs amb pendientes)
    (if (null? defs)
        amb
        (let* ((nuevo (evaluar-definicion (car defs) amb))
               (pendientes-con-nueva
                (if (definicion-lambda? (car defs))
                    (let ((valores (valores-eslabon nuevo)))
                      (cons (cons valores (vector-ref valores 0)) pendientes))
                    pendientes)))
          (recorrer-definiciones-bloque (cdr defs)
                                        nuevo
                                        (reapuntar-clausuras pendientes-con-nueva nuevo))))))

; reapuntar-clausuras:
; Proposito:
; L(Par) x Amb -> L(Par) : Para cada par (vector . clausura), si el vector todavia
; guarda esa misma clausura (eq?), la reemplaza (vector-set!) por una igual cuyo
; ambiente es amb, y retorna la lista con las clausuras nuevas. Es la idea de
; extend-env-recursively: se crea el eslabon y despues se guarda en el vector la
; clausura que apunta a el. Si el vector ya tiene otro valor (alguien le hizo set
; a la variable), ese par se descarta: no se toca un valor que no creo el lambda.
(define reapuntar-clausuras
  (lambda (pendientes amb)
    (if (null? pendientes)
        '()
        (let ((valores (car (car pendientes)))
              (anterior (cdr (car pendientes))))
          (if (eq? (vector-ref valores 0) anterior)
              (let ((nueva (cambiar-ambiente-clausura anterior amb)))
                (begin
                  (vector-set! valores 0 nueva)
                  (cons (cons valores nueva)
                        (reapuntar-clausuras (cdr pendientes) amb))))
              (reapuntar-clausuras (cdr pendientes) amb))))))

; definicion-lambda?:
; Proposito:
; Def -> B : Retorna #t si la expresion de la definicion (variable o constante)
; es un lambda escrito directamente (ver es-lambda?).
(define definicion-lambda?
  (lambda (def)
    (cases definicion def
      (variable-definicion (tipo identificador expresion) (es-lambda? expresion))
      (constante-definicion (tipo identificador expresion) (es-lambda? expresion)))))

; valores-eslabon:
; Proposito:
; Amb -> Vector : Retorna el vector de valores del primer eslabon de un ambiente
; extendido (el que acaba de crear evaluar-definicion).
(define valores-eslabon
  (lambda (amb)
    (cases ambiente amb
      (ambiente-vacio ()
        (eopl:error 'valores-eslabon "ambiente vacio"))
      (ambiente-extendido (tipos simbolos valores es-const amb-anterior)
        valores))))


(define evaluar-definicion
  (lambda (def ambiente)
    (cases definicion def
      (variable-definicion (tipo identificador expresion)
        (let* ((valor-sin-procesar (evaluar-expresion expresion ambiente))
               (valor-inicial (coercionar-tipo tipo valor-sin-procesar)))
          (if (validar-tipo tipo valor-inicial)
              (extender-ambiente-definicion tipo identificador valor-inicial #f ambiente)
              (eopl:error 'evaluar-definicion
                          "La variable ~s es de tipo ~s y recibio ~s"
                          identificador tipo valor-inicial))))
                           

      (constante-definicion (tipo identificador expresion)
                            (let ((valor-constante (evaluar-expresion expresion ambiente)))
                              (if (validar-tipo tipo valor-constante)
                                  (extender-ambiente-definicion tipo identificador valor-constante #t ambiente)

                                  (eopl:error 'evaluar-definicion "La constante ~s es de tipo ~s y recibio ~s" identificador tipo valor-constante)))))
                            )
                          )

; extender-ambiente-definicion:
; Proposito:
; T x Sym x V x B x Amb -> Amb : Crea el eslabon de una definicion (una sola
; ligadura) sobre el ambiente recibido. La recursion de los procedimientos no se
; resuelve aqui sino en recorrer-definiciones-bloque, que conoce todo el bloque.
(define extender-ambiente-definicion
  (lambda (tipo id valor es-constante amb)
    (ambiente-extendido (vector tipo) (list id) (vector valor)
                        (vector es-constante) amb)))

; es-lambda?:
; Proposito:
; E -> B : Retorna #t si la expresion es solo un lambda (una expresion-operaciones
; con un unico operando operando-lambda y sin operaciones), y #f en otro caso.
(define es-lambda?
  (lambda (exp)
    (cases expresion exp
      (expresion-operaciones (primero operaciones)
        (and (null? operaciones)
             (cases operando primero
               (operando-lambda (tipos parametros cuerpo) #t)
               (else #f))))
      (else #f))))

; cambiar-ambiente-clausura:
; Proposito:
; Proc x Amb -> Proc : Retorna una clausura con los mismos tipos, parametros y
; cuerpo, pero con el ambiente recibido.
(define cambiar-ambiente-clausura
  (lambda (proc nuevo-amb)
    (cases procedimiento proc
      (clausura (tipos parametros cuerpo amb)
        (clausura tipos parametros cuerpo nuevo-amb)))))

;falta completar con los tipos compuestos y  el caso 2, cuando se modifica en el ambiente (asignar, suponmgo que el set del profesor, nosotros usamos "->")
(define validar-tipo
  (lambda (tipo-dato valor)
    (cases tipo tipo-dato
      (tipo-simple (tp)
        (cases tipo-primitivo tp
          (entero-tipo () (entero? valor))
          (decimal-tipo () (real? valor))
          (cadena-tipo () (string? valor))
          (booleano-tipo () (boolean? valor))
          (void-tipo () #t)
          (else (eopl:error 'validar-tipo "tipo primitivo ~s no soportado" tp))))
      (tipo-complejo (tc)
        (cases tipo-compuesto tc
          (lista-tipo (t) (list? valor))
          (vector-tipo (t) (vector? valor))
          (proc-tipo () (procedimiento? valor))
          (else (eopl:error 'validar-tipo "tipo compuesto ~s no soportado" tc)))))))

(define evaluar-dato-primitivo
  (lambda (dato amb)
    (cases dato-primitivo dato
      (numero-lit (n) n)
      (cadena-lit (s) (quitar-comillas s)) ; el scanner deja las comillas en el lexema
      (true-lit () #t)
      (false-lit () #f)
    )))



(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (asignar-expresion (id expresion)
        (aplicar-asignacion id amb (evaluar-expresion expresion amb)))
      (expresion-operaciones (primero operaciones)
        (let* ((valor-inicial (evaluar-operando primero amb))
               (valores-adicionales
                (map (lambda (operacion)
                       (cases continuacion-operacion operacion
                         (una-operacion (op operando)
                           (evaluar-operando operando amb))))
                     operaciones))
               (operadores
                (map (lambda (operacion)
                       (cases continuacion-operacion operacion
                         (una-operacion (op operando) op)))
                     operaciones)))
          (evaluar-secuencia (cons valor-inicial valores-adicionales) operadores))))))

(define evaluar-operando
  (lambda (dato-operando amb)
    (cases operando dato-operando
      (operando-primitivo (dato) (evaluar-dato-primitivo dato amb))
      ;; x -> lee la variable; f(a, b) -> invoca el procedimiento f
      (operando-identificador (id resto)
        (cases resto-identificador resto
          (acceso-variable () (aplicar-ambiente amb id))
          (invocacion (argumentos)
            (evaluar-invocacion id argumentos amb))))
      ;; lambda crea la clausura con el ambiente actual (alcance estatico)
      (operando-lambda (tipos parametros cuerpo)
        (clausura tipos parametros cuerpo amb))
      (operando-parentesis (contenido)
        (cases contenido-parentesis contenido
          (contenido-vacio () '())
          (contenido-no-vacio (primera resto)
            (cases resto-parentesis resto
              (agrupacion () (evaluar-expresion primera amb))
              (elementos-lista (otras)
                (cons (evaluar-expresion primera amb)
                      (map (lambda (e) (evaluar-expresion e amb)) otras)))))))
      (operando-condicional (condicion rama-verdadera rama-falsa)
        (if (valor-verdad? (evaluar-expresion condicion amb))
            (evaluar-expresion rama-verdadera amb)
            (evaluar-expresion rama-falsa amb)))
      (operando-switch (exp-evaluada casos cuerpos cuerpo-default)
        (evaluar-switch (evaluar-expresion exp-evaluada amb)
                        casos cuerpos cuerpo-default amb))
      (bloque-expresion (primera restantes)
        (evaluar-cuerpo-secuencia primera restantes amb))
      (locales-expresion (primera-def otras-defs primera-exp otras-exps)
        (let ((ambiente-local
               (recorrer-lista-definicion (cons primera-def otras-defs) amb)))
          (evaluar-cuerpo-secuencia primera-exp otras-exps ambiente-local)))
      (primitiva-unaria-expresion (primitiva expresion)
        (evaluar-primitiva-unaria primitiva (evaluar-expresion expresion amb)))
      ;; Expresiones de lista
      (lista-vacia () '())
      (vacio-predicado (lista)
                       (null? (evaluar-expresion lista amb)))
      (crear-lista (elemento lista)
                   (let ((cabeza (evaluar-expresion elemento amb))
                         (cola (evaluar-expresion lista amb)))
                     (cons cabeza cola)))
      (lista-predicado (lista)
                       (list? (evaluar-expresion lista amb)))
      (cabeza-lista (lista)
                    (let ((l (evaluar-expresion lista amb)))
                      (if (null? l)
                          (eopl:error 'cabeza-lista "cabeza de la lista vacia")
                          (car l))))
      (cola-lista (lista)
                  (let ((l (evaluar-expresion lista amb)))
                    (if (null? l)
                        (eopl:error 'cola-lista "cola de la lista vacia")
                        (cdr l))))
      (concatenar-listas (lista1 lista2)
                         (let ((l1 (evaluar-expresion lista1 amb))
                               (l2 (evaluar-expresion lista2 amb)))
                           (if (and (list? l1) (list? l2))
                               (append l1 l2)
                               (eopl:error 'concatenar-listas "Los parametros no son listas"))))
      (referencia-lista (lista posicion)
                        (let ((l (evaluar-expresion lista amb))
                              (pos (evaluar-expresion posicion amb)))
                          (if (or (< pos 0) (>= pos (length l)))
                              (eopl:error 'referencia-lista "posicion fuera del rango ~s" pos)
                              (list-ref l pos))))
      (longitud-lista (lista)
                      (let ((l (evaluar-expresion lista amb)))
                        (if (list? l)
                            (length l)
                            (eopl:error 'longitud-lista "No es una lista ~s" l))))
      

      (else (eopl:error 'evaluar-operando "operando no soportado: ~s" dato-operando))
                     
      )))

; evaluar-invocacion:
; Proposito:
; Sym x L(E) x Amb -> V : Busca el identificador en el ambiente. Si su valor no
; es un procedimiento lanza un error (por ejemplo int f = 2; f(1)). Si lo es,
; evalua los argumentos de izquierda a derecha y lo aplica con
; aplicar-procedimiento.
(define evaluar-invocacion
  (lambda (id argumentos amb)
    (let ((proc (aplicar-ambiente amb id)))
      (if (procedimiento? proc)
          (aplicar-procedimiento
           proc
           (map (lambda (arg) (evaluar-expresion arg amb)) argumentos)
           id)
          (eopl:error 'evaluar-invocacion
                      "~a no es un procedimiento, es un ~a (~a)"
                      id (nombre-tipo-valor proc) (mostrar-valor proc))))))

;; Evalúa en orden y conserva el valor de la última expresión del cuerpo.
;; evaluar-secuencia, más abajo, se ocupa de la precedencia de operadores.
(define evaluar-cuerpo-secuencia
  (lambda (primera restantes amb)
    (let loop ((valor (evaluar-expresion primera amb))
               (pendientes restantes))
      (if (null? pendientes)
          valor
          (loop (evaluar-expresion (car pendientes) amb)
                (cdr pendientes))))))

    
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
      (modulo ()         (modular valor1 valor2))
      (concat ()         (concatenar valor1 valor2))
      (mayor ()          (condicional? '>  valor1 valor2))
      (menor ()          (condicional? '<  valor1 valor2))
      (mayor-igual ()    (condicional? '>= valor1 valor2))
      (menor-igual ()    (condicional? '<= valor1 valor2))
      (diferente ()      (condicional? '!= valor1 valor2))
      (igual ()          (condicional? '== valor1 valor2))
      (disyuncion ()     (or valor1 valor2))
      (conjuncion ()     (and valor1 valor2))
      )))

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
      (else
       (/ (exact->inexact valor1)
          (exact->inexact valor2))))))

(define modular
  (lambda (valor1 valor2)
    (cond
      ((not (and (number? valor1) (number? valor2)))
       (eopl:error 'evaluar-primitiva-binaria
                   "La primitiva mod solo opera sobre int y float, se recibio: ~s y ~s"
                   valor1 valor2))
      ((zero? valor2)
       (eopl:error 'evaluar-primitiva-binaria
                   "Modulo por cero: ~s mod ~s" valor1 valor2))
      ((and (integer? valor1) (integer? valor2)) remainder valor1 valor2)
      (else (modulo-decimal valor1 valor2)))))

(define (modulo-decimal a b)
  (- a (* b (floor (/ a b)))))

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

(define coercionar-tipo
  (lambda (tipo-dato valor)
    (cases tipo tipo-dato
      (tipo-simple (tp)
        (cases tipo-primitivo tp
          (decimal-tipo ()
            (if (number? valor)
                (exact->inexact valor)   ; int -> float automaticamente
                valor))
          (entero-tipo ()
            (if (and (real? valor) (not (entero? valor)))
                (inexact->exact (truncate valor))  ; float -> int (trunca)
                valor))
          (else valor)))                 ; string, bool, void: sin cambio
      (tipo-complejo (tc) valor))))      ; listas, vectores: sin cambio

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
      ;; Un bool contra un numero casi siempre viene de encadenar comparaciones
      ;; (1 < 2 < 3 se evalua como (1 < 2) < 3 = True < 3), por eso se explica.
      ((or (and (boolean? valor1) (number? valor2))
           (and (number? valor1) (boolean? valor2)))
       (eopl:error 'evaluar-primitiva-binaria
                   "La comparacion ~a no se puede aplicar entre un ~a (~a) y un ~a (~a). Las comparaciones no se encadenan: 1 < 2 < 3 se evalua como (1 < 2) < 3, es decir True < 3. Usa una sola comparacion por expresion."
                   operador
                   (nombre-tipo-valor valor1) (mostrar-valor valor1)
                   (nombre-tipo-valor valor2) (mostrar-valor valor2)))
      (else
       (eopl:error 'evaluar-primitiva-binaria
                   "La comparacion ~a solo acepta int, float o bool del mismo tipo; se recibio un ~a (~a) y un ~a (~a)."
                   operador
                   (nombre-tipo-valor valor1) (mostrar-valor valor1)
                   (nombre-tipo-valor valor2) (mostrar-valor valor2))))))

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

; nombre-tipo-valor:
; Proposito:
; V -> S : Retorna el nombre del tipo de LogicFusion al que corresponde un
; valor ("int", "float", "bool", "string", "list", "vector" o "proc"). Se usa para que
; los mensajes de error hablen en terminos del lenguaje y no de Racket.
(define nombre-tipo-valor
  (lambda (valor)
    (cond
      ((entero? valor) "int")
      ((real? valor) "float")
      ((boolean? valor) "bool")
      ((string? valor) "string")
      ((list? valor) "list")
      ((vector? valor) "vector")
      ((procedimiento? valor) "proc")
      (else "desconocido"))))

; mostrar-valor:
; Proposito:
; V -> V : Prepara un valor para mostrarlo en un mensaje de error con la
; sintaxis de LogicFusion: #t como True, #f como False y las cadenas entre
; comillas. Los demas valores se retornan tal cual.
(define mostrar-valor
  (lambda (valor)
    (cond
      ((eq? valor #t) "True")
      ((eq? valor #f) "False")
      ((string? valor) (string-append "\"" valor "\""))
      (else valor))))

; es-multiplicacion-division?:
; Proposito:
; PB -> B : Procedimiento que recibe una primitiva binaria y retorna #t si
; corresponde a una multiplicacion (*) o division (/). En caso contrario,
; retorna #f. Se utiliza para identificar las operaciones que tienen mayor
; prioridad al evaluar una secuencia de operaciones.
(define es-multiplicacion-division?
  (lambda (op)
    (cases primitiva-binaria op
      (multiplicacion () #t)
      (division () #t)
      (modulo () #t)
      (else #f))))

; es-suma-resta?:
; Proposito:
; PB -> B : Procedimiento que recibe una primitiva binaria y retorna #t si
; corresponde a una suma (+) o resta (~). En caso contrario, retorna #f.
; Se utiliza para identificar las operaciones que se evalúan después de
; las multiplicaciones y divisiones.
(define es-suma-resta?
  (lambda (op)
    (cases primitiva-binaria op
      (suma () #t)
      (resta () #t)
      (else #f))))

; es-concat?:
; PB -> B : Procedimiento que recibe una primitiva binaria y retorna #t si
; corresponde a una concatenación (concat). En caso contrario, retorna #f.
; Se utiliza para identificar las operaciones de concatenación que deben
; resolverse después de las operaciones aritméticas y antes de las
; comparaciones, durante la evaluación de una secuencia de operaciones.
(define es-concat?
  (lambda (op)
    (cases primitiva-binaria op
      (concat () #t)
      (else #f))))

; es-comparacion?:
; PB -> B : Procedimiento que recibe una primitiva binaria y retorna #t si
; corresponde a una operación de comparación (>, <, >=, <=, != o ==).
; En caso contrario, retorna #f. Se utiliza para identificar las operaciones
; relacionales que deben resolverse después de las operaciones aritméticas
; y de concatenación, durante la evaluación de una secuencia de operaciones.
(define es-comparacion?
  (lambda (op)
    (cases primitiva-binaria op
      (mayor () #t)
      (menor () #t)
      (mayor-igual () #t)
      (menor-igual () #t)
      (diferente () #t)
      (igual () #t)
      (else #f))))

; reducir-nivel:
; Proposito:
; L(V) x L(PB) x Pred -> L(L(V)) : Procedimiento que recibe una lista de
; valores, una lista de operadores y un predicado que determina cuáles
; operaciones debe resolver. Recorre las operaciones de izquierda a derecha,
; ejecuta las que cumplen el predicado y conserva las demás para evaluarlas
; posteriormente. Retorna una lista con los nuevos valores y los operadores
; pendientes.
(define reducir-nivel
  (lambda (valores operadores predicado)
    (letrec
        ((recorrer
          (lambda (resto-valores resto-operadores acumulado
                  nuevos-valores nuevos-operadores)

            (if (null? resto-operadores)

                (list (append nuevos-valores (list acumulado))
                      nuevos-operadores)

                (let ((op (car resto-operadores))
                      (siguiente (car resto-valores)))

                  (if (predicado op)

                      (recorrer
                       (cdr resto-valores)
                       (cdr resto-operadores)
                       (evaluar-primitiva-binaria
                        op acumulado siguiente)
                       nuevos-valores
                       nuevos-operadores)

                      (recorrer
                       (cdr resto-valores)
                       (cdr resto-operadores)
                       siguiente
                       (append nuevos-valores (list acumulado))
                       (append nuevos-operadores (list op)))))))))

      (recorrer (cdr valores)
                operadores
                (car valores)
                '()
                '()))))
; evaluar-secuencia:
; L(V) x L(PB) -> V : Procedimiento que recibe una lista de valores y una
; lista de operadores. Primero utiliza reducir-nivel para resolver las
; multiplicaciones y divisiones, después resuelve las sumas y restas.
; Finalmente, retorna el resultado de la expresión evaluada.
(define evaluar-secuencia
  (lambda (valores operadores)

    (let* ((nivel-1
            (reducir-nivel valores operadores
                           es-multiplicacion-division?))

           (nivel-2
            (reducir-nivel (car nivel-1) (cadr nivel-1)
                           es-suma-resta?))

           (nivel-3
            (reducir-nivel (car nivel-2) (cadr nivel-2)
                           es-concat?))

           (nivel-4
            (reducir-nivel (car nivel-3) (cadr nivel-3)
                           es-comparacion?)))

      (if (null? (cdr (car nivel-4)))
          (car (car nivel-4))
          (eopl:error 'evaluar-secuencia
                      "Quedaron operadores sin evaluar")))))
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

; existe-ligadura?:
; Proposito:
; Amb x Sym -> B : Retorna #t si el simbolo esta ligado en algun eslabon del
; ambiente y #f si no. Recorre igual que aplicar-ambiente-referencia, pero sin
; lanzar error (lo usa invocar-main para dar un mensaje claro).
(define existe-ligadura?
  (lambda (amb simbolo)
    (cases ambiente amb
      (ambiente-vacio () #f)
      (ambiente-extendido (tipos simbolos valores es-const amb-anterior)
        (if (number? (rib-find-position simbolo simbolos))
            #t
            (existe-ligadura? amb-anterior simbolo))))))

;funcion auxiliar para armar la lista que nos ayuda a setear/asignar "->". Es "hermana" de aplicar-ambiente-referencia, solo cambia lo que retorna
(define buscar-ligadura-para-modificar
  (lambda (amb simbolo)
    (cases ambiente amb
      (ambiente-vacio ()
          (eopl:error 'buscar-ligadura-para-modificar "No esta ligada para ~s" simbolo))
      (ambiente-extendido (tipos simbolos valores es-const amb)
          (let ((posicion (rib-find-position simbolo simbolos)))
            (if (number? posicion)
                (list (a-ref posicion valores) (vector-ref tipos posicion) (vector-ref es-const posicion))
                (buscar-ligadura-para-modificar amb simbolo)))))))

(define aplicar-asignacion
  (lambda (id amb valor-para-cambiar)
    (let* ((lista-ref-tipo-const (buscar-ligadura-para-modificar amb id)) ;sacamos cada elemento de la lista
           (referencia (car lista-ref-tipo-const))
           (tipo-variable (cadr lista-ref-tipo-const))
           (es-constante (caddr lista-ref-tipo-const))
           )
             (if es-constante ;cuerpo del let*
               (eopl:error 'aplicar-asignacion "No se puede cambiar la constante ~s" id)
               (if (validar-tipo tipo-variable valor-para-cambiar)
                   (begin
                     (setref! referencia valor-para-cambiar)
                     (deref referencia)) ;devuelve el nuevo valor que guardamos en la referencia :)
                   (eopl:error 'aplicar-asignacion "La variable ~s es de tipo ~s y no se puede cambiar a ~s" id tipo-variable valor-para-cambiar )      
              )
          )
      )
    )
  )

;; ============================================================================
;; PROCEDIMIENTOS
;; ============================================================================
;; Un procedimiento es un valor (clausura): guarda los tipos y nombres de sus
;; parametros, su cuerpo y el ambiente donde se creo (alcance estatico).
;; Basado en procval / closure / apply-procval del interpretador del curso.

(define-datatype procedimiento procedimiento?
  (clausura (tipos (list-of tipo?))          ; tipo declarado de cada parametro
            (parametros (list-of symbol?))   ; nombres de los parametros
            (cuerpo expresion?)              ; expresion despues de return
            (amb ambiente?)))                ; ambiente donde se creo

; aplicar-procedimiento:
; Proposito:
; Proc x L(V) x Sym -> V : Invoca un procedimiento con los argumentos ya
; evaluados. Verifica la cantidad de argumentos y el tipo de cada uno, crea un
; eslabon nuevo con los parametros (mutables) sobre el ambiente de la clausura
; y evalua el cuerpo ahi. El simbolo es el nombre del procedimiento, solo se usa
; en los mensajes de error. Paso por valor: el vector del eslabon es nuevo, asi
; que asignar un parametro no cambia la variable de quien invoca.
(define aplicar-procedimiento
  (lambda (proc argumentos nombre)
    (cases procedimiento proc
      (clausura (tipos parametros cuerpo amb)
        (if (not (= (length parametros) (length argumentos)))
            (eopl:error 'aplicar-procedimiento
                        "El procedimiento ~a espera ~a argumento(s) y recibio ~a"
                        nombre (length parametros) (length argumentos))
            (begin
              (validar-argumentos tipos parametros argumentos nombre)
              (evaluar-expresion cuerpo
                                 (ambiente-extendido
                                  (list->vector tipos)
                                  parametros
                                  (list->vector argumentos)
                                  (make-vector (length parametros) #f)
                                  amb))))))))

; validar-argumentos:
; Proposito:
; L(T) x L(Sym) x L(V) x Sym -> B : Recorre en paralelo los tipos, los nombres
; de los parametros y los argumentos. Si algun argumento no coincide con el tipo
; de su parametro lanza un error; si todos coinciden retorna #t.
(define validar-argumentos
  (lambda (tipos parametros argumentos nombre)
    (cond
      ((null? tipos) #t)
      ((validar-tipo (car tipos) (car argumentos))
       (validar-argumentos (cdr tipos) (cdr parametros) (cdr argumentos) nombre))
      (else
       (eopl:error 'aplicar-procedimiento
                   "El parametro ~a del procedimiento ~a es de tipo ~s y recibio ~a"
                   (car parametros) nombre (car tipos) (mostrar-valor (car argumentos)))))))

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

; rib-find-position:
; Proposito:
; Sym x L -> N o #f : retorna la posición del símbolo dentro de la "costilla"
; (rib) de un ambiente extendido, es decir, en su lista de símbolos. Esa
; posición es la misma que ocupa su valor en el vector de valores, y por eso
; aplicar-ambiente-referencia puede construir (a-ref posicion valores).
(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

; quitar-comillas:
; Proposito:
; S -> S : El scanner de SLLGEN guarda el lexema completo del token texto,
; incluyendo las comillas ("hola" llega como "\"hola\"", de longitud 6). Esta
; funcion retorna la cadena sin la primera ni la ultima comilla, para que
; longitud y concat trabajen sobre el contenido real.
(define quitar-comillas
  (lambda (cadena)
    (substring cadena 1 (- (string-length cadena) 1))))

; valor-verdad?:
; Proposito:
; V -> B : Determina si un valor se interpreta como verdadero o falso en una
; condicion (true-value?). Un bool se usa tal cual; un numero es falso si es 0 
; y verdadero en otro caso (regla del enunciado). Cualquier otro valor 
; lanza un error.
(define valor-verdad?
  (lambda (valor)
    (cond
      ((boolean? valor) valor)
      ((number? valor) (not (zero? valor)))
      (else (eopl:error 'valor-verdad?
                        "Una condicion debe ser bool o numero, se recibio: ~s"
                        valor)))))

;; valores-iguales?:
; Proposito:
; V x V -> B : Retorna #t si los dos valores son iguales. Compara numeros por
; valor, cadenas con string=? y bool con eq? (eqv?). Valores de categorias distintas
; (por ejemplo un int y una cadena) no son iguales entonces retorna #f.
(define valores-iguales?
  (lambda (valor1 valor2)
    (cond
      ((and (number? valor1) (number? valor2)) (= valor1 valor2))
      ((and (string? valor1) (string? valor2)) (string=? valor1 valor2))
      ((and (boolean? valor1) (boolean? valor2)) (eq? valor1 valor2))
      (else #f))))

; evaluar-switch:
; Proposito:
; V x L(E) x L(E) x E x Amb -> V : Recorre los case en orden. Evalua el valor
; de cada case y en el primero que sea igual al valor dado, evalua y retorna
; su cuerpo. Si la lista de case se acaba, evalua y retorna el cuerpo de default.
(define evaluar-switch
  (lambda (valor casos cuerpos cuerpo-default amb)
    (cond
      ((null? casos) (evaluar-expresion cuerpo-default amb))
      ((valores-iguales? valor (evaluar-expresion (car casos) amb))
       (evaluar-expresion (car cuerpos) amb))
      (else (evaluar-switch valor (cdr casos) (cdr cuerpos) cuerpo-default amb)))))

;; ============================================================================
;; NOTA: desde que existe main, el valor de un programa es lo que retorna main.
;; Los ejemplos de abajo que no definen main muestran el parseo de cada
;; produccion; el valor anotado es el de la expresion de la ultima definicion.
;;
;; NOTA sobre las comillas: dentro de (scan&parse "...") el programa es un
;; string de Racket, por eso las cadenas de LogicFusion se escriben \"hola\".
;; Para probar un ejemplo en el REPL (-->) se copia sin el \ :  sq("hola")
;; Si se pega con \" el parser falla (... can't begin with end-marker #f).
;; ============================================================================

;; ============================================================================
;; EJEMPLOS DE scan&parse PARA PROCEDIMIENTOS, INVOCACION Y main
;; ============================================================================
;; operando-lambda + main:
;; (scan&parse "GLOBALS { int a = 4; proc square = lambda (int x) { return x * x; }; } PROGRAM { proc main = lambda () { return square(a); }; }") ; 16
;; Varios parametros:
;; (scan&parse "GLOBALS { proc suma = lambda (int x, float y) { return x + y; }; } PROGRAM { proc main = lambda () { return suma(1, 2.5); }; }") ; 3.5
;; Invocacion dentro de una operacion:
;; (scan&parse "GLOBALS { proc square = lambda (int x) { return x * x; }; } PROGRAM { proc main = lambda () { return square(2) + square(3); }; }") ; 13
;; Recursion:
;; (scan&parse "GLOBALS { proc factorial = lambda (int n) { return if (n <= 1) { 1 } else { n * factorial(n ~ 1) }; }; } PROGRAM { proc main = lambda () { return factorial(5); }; }") ; 120
;; Recursion mutua (esPar llama a esImpar, definido despues, y viceversa):
;; (scan&parse "GLOBALS { proc esPar = lambda (int n) { return if (n == 0) { True } else { esImpar(n ~ 1) }; }; proc esImpar = lambda (int n) { return if (n == 0) { False } else { esPar(n ~ 1) }; }; } PROGRAM { proc main = lambda () { return esPar(4); }; }") ; True
;; Procedimiento que retorna un procedimiento (la clausura conserva n = 5):
;; (scan&parse "GLOBALS { proc sumador = lambda (int n) { return lambda (int x) { return x + n; }; }; proc mas5 = sumador(5); } PROGRAM { proc main = lambda () { return mas5(10); }; }") ; 15
;; Orden superior (parametro de tipo proc):
;; (scan&parse "GLOBALS { proc doble = lambda (int x) { return x * 2; }; proc aplicar = lambda (proc g, int x) { return g(x); }; } PROGRAM { proc main = lambda () { return aplicar(doble, 7); }; }") ; 14
;; LOCALS dentro de un procedimiento (triangle de la guia):
;; (scan&parse "GLOBALS { float base = 10.5; proc triangle = lambda (float base, float height) { return LOCALS { float area = 0; } { set area -> base * height / 2; area; }; }; } PROGRAM { proc main = lambda () { return triangle(base, 6.4); }; }") ; 33.6
;; Paso por valor (x global sigue en 5):
;; (scan&parse "GLOBALS { int x = 5; proc cambiar = lambda (int x) { return BLOCK { set x -> 99; x; }; }; } PROGRAM { proc main = lambda () { return BLOCK { cambiar(x); x; }; }; }") ; 5
;; Errores:
;; (scan&parse "GLOBALS { } PROGRAM { }")                                                   ; error: no hay main
;; (scan&parse "GLOBALS { int f = 2; } PROGRAM { proc main = lambda () { return f(2, 1); }; }") ; error: f no es un procedimiento
;; (scan&parse "GLOBALS { proc sq = lambda (int x) { return x * x; }; } PROGRAM { proc main = lambda () { return sq(1, 2); }; }") ; error: cantidad de argumentos
;; (scan&parse "GLOBALS { proc sq = lambda (int x) { return x * x; }; } PROGRAM { proc main = lambda () { return sq(\"hola\"); }; }") ; error: tipo del argumento
;; (scan&parse "GLOBALS { proc p = 5; } PROGRAM { }")                                       ; error: tipo proc

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

;; Secuencias y asignación (ejemplos de cada nueva producción):
;; (scan&parse "GLOBALS { int x = 1; } PROGRAM { int r = BLOCK { set x -> 4; x; }; }")
;; (scan&parse "GLOBALS { int x = 1; } PROGRAM { int r = LOCALS { int x = 2; } { set x -> 3; x; }; }")
;; (scan&parse "GLOBALS {} PROGRAM { list<int> x = (1,); }")
;; (scan&parse "GLOBALS {} PROGRAM { int x = (1 + 2) * 3; }")

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
;; EJEMPLOS DE scan&parse PARA if/else
;; ============================================================================

;; (scan&parse "GLOBALS { int edad = 20; } PROGRAM { string r = if (edad >= 18) { \"Mayor de edad\" } else { \"Menor de edad\" }; }") ; "Mayor de edad"
;; (scan&parse "GLOBALS { int edad = 15; } PROGRAM { string r = if (edad < 13) { \"Nino\" } else { if (edad < 18) { \"Adolescente\" } else { \"Adulto\" } }; }") ; "Adolescente" (anidado)
;; (scan&parse "GLOBALS { } PROGRAM { int r = if (0) { 1 } else { 2 }; }") ; 2 (0 es falso)
;; (scan&parse "GLOBALS { } PROGRAM { int r = 10 + if (True) { 1 } else { 2 } * 3; }") ; 13 (if como operando)

;; ============================================================================
;; EJEMPLOS DE scan&parse PARA switch
;; ============================================================================

;; (scan&parse "GLOBALS { string color = \"rojo\"; } PROGRAM { string r = switch (color) { case \"rojo\": { \"Para\" } case \"verde\": { \"Sigue\" } default: { \"Color invalido\" } }; }") ; "Para"
;; (scan&parse "GLOBALS { string color = \"azul\"; } PROGRAM { string r = switch (color) { case \"rojo\": { \"Para\" } default: { \"Color invalido\" } }; }") ; "Color invalido" (default)
;; (scan&parse "GLOBALS { int n = 2; } PROGRAM { int r = switch (n) { case 1: { 10 } case 1 + 1: { 20 } default: { 0 } }; }") ; 20
;; (scan&parse "GLOBALS { int n = 5; } PROGRAM { int r = 1 + switch (n) { default: { 7 } }; }") ; 8
;; ============================================================================
;Esta funcion debe conocer a todo lo demas del interpretador, por eso va al final.
(define logicfusion-interpretador
  (sllgen:make-rep-loop  "-->" evaluar-programa
                         (sllgen:make-stream-parser
                          scanner-spec-logicfusion
                          gramatica-logicfusion)))

(logicfusion-interpretador)
