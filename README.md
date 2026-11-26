# README.md — LogicFusion (estructura ajustada)

A continuación te dejo el `README.md` reorganizado y consolidado, alineado con el código real del intérprete (`#lang eopl`) y con la guía que ya tenías. Se corrigieron incoherencias (p. ej. el carácter de comentario es `$`, no `%`), se añadieron las producciones que el código soporta y que el README omitía (`switch`, `%`, `es-multiplicacion-division?`, etc.), y se conservó la numeración de secciones y el estilo de tablas.

---

```markdown
# LogicFusion — Intérprete

## Tabla de contenidos

1. Descripción general
2. Requisitos
3. Cómo ejecutar
4. Especificación léxica
5. Gramática BNF
6. Sistema de tipos
7. Estructura de un programa
8. Expresiones y operandos
9. Primitivas
10. Operaciones sobre listas
11. Estructuras de control
12. Ambientes y referencias
13. Definiciones formales de procedimientos
14. Ejemplos de uso
15. Pendientes

## 1. Descripción general

**LogicFusion** es un lenguaje de programación imperativo con tipado estático, implementado como un intérprete en Racket usando la biblioteca `eopl` (Essentials of Programming Languages). El intérprete procesa un programa completo, evalúa sus definiciones en orden y retorna el valor de la última definición del bloque `PROGRAM`.

El intérprete está construido sobre tres pilares:

- **Especificación léxica** (`scanner-spec-logicfusion`): define cómo tokenizar el texto fuente.
- **Especificación sintáctica** (`gramatica-logicfusion`): define la gramática del lenguaje con SLLGEN.
- **Evaluador**: recorre el árbol de sintaxis abstracta (ASA) producido por SLLGEN y computa valores.

## 2. Requisitos

- [Racket](https://racket-lang.org/) con el paquete `eopl` instalado.
- El archivo fuente debe comenzar con `#lang eopl`.

## 3. Cómo ejecutar

Al cargar el archivo en DrRacket o ejecutarlo desde la terminal, el intérprete entra en un **REPL interactivo** indicado por el prompt `-->`. Se pueden ingresar programas completos directamente:

```
--> GLOBALS { int x = 10; } PROGRAM { int r = x * 2; }
20
```

También se puede usar `scan&parse` para ver el ASA sin evaluar:

```racket
(scan&parse "GLOBALS { int a = 5; } PROGRAM { int b = a + 1; }")
```

Y `just-scan` para ver únicamente los tokens generados por el scanner:

```racket
(just-scan "GLOBALS { int a = 5; }")
```

## 4. Especificación léxica

Definida en `scanner-spec-logicfusion`. Cada regla tiene la forma `(nombre (expresión-regular) acción)`.

| Token              | Expresión regular                                        | Acción            |
| ------------------ | -------------------------------------------------------- | ----------------- |
| `espacio-blanco`   | `whitespace`                                             | `skip` — ignorado |
| `comentario`       | `$` seguido de cualquier carácter hasta fin de línea     | `skip` — ignorado |
| `decimal-positivo` | Dígitos, punto, dígitos                                  | `number`          |
| `decimal-negativo` | `-` seguido de dígitos, punto, dígitos                   | `number`          |
| `entero-positivo`  | Uno o más dígitos                                        | `number`          |
| `entero-negativo`  | `-` seguido de uno o más dígitos                         | `number`          |
| `identificador`    | Letra, seguida de letras, dígitos, `?` o `-letra/dígito` | `symbol`          |
| `texto`            | Caracteres entre comillas dobles                         | `string`          |

**Notas importantes:**

- Los identificadores permiten guiones medios en el interior siempre que vayan seguidos de letra o dígito (ej. `mi-variable`, `es-par?`). Un guion al inicio sería reconocido como número negativo.
- El carácter `$` inicia un comentario de línea; todo lo que sigue hasta el salto de línea es ignorado.
- El scanner conserva las comillas en el lexema del token `texto`. La función auxiliar `quitar-comillas` las elimina en tiempo de evaluación.

## 5. Gramática BNF

```bnf
programa     ::= "GLOBALS" "{" { definicion } "}"
                 "PROGRAM" "{" { definicion } "}"

definicion   ::= tipo identificador "=" expresion ";"
               | "const" tipo identificador "=" expresion ";"

tipo           ::= tipo-primitivo | tipo-compuesto
tipo-primitivo ::= "int" | "float" | "string" | "bool" | "void"
tipo-compuesto ::= "sat" | "proc"
                 | "list" "<" tipo-primitivo ">"
                 | "vector" "<" tipo-primitivo ">"
                 | "dict" "<" tipo-primitivo "," tipo-primitivo ">"

expresion    ::= operando { primitiva-binaria operando }
               | "set" identificador "->" expresion

operando     ::= dato-primitivo
               | "if" "(" expresion ")" "{" expresion "}"
                 "else" "{" expresion "}"
               | "switch" "(" expresion ")" "{"
                   { "case" expresion ":" "{" expresion "}" }
                   "default" ":" "{" expresion "}" "}"
               | primitiva-unaria "(" expresion ")"
               | "(" contenido-parentesis ")"
               | "empty" | "empty?" "(" expresion ")"
               | "make-list" "(" expresion "," expresion ")"
               | "list?" "(" expresion ")"
               | "head" "(" expresion ")" | "tail" "(" expresion ")"
               | "append" "(" expresion "," expresion ")"
               | "ref-list" "(" expresion "," expresion ")"
               | "length" "(" expresion ")"
               | "BLOCK" "{" expresion ";" { expresion ";" } "}"
               | "LOCALS" "{" definicion { definicion } "}"
                 "{" expresion ";" { expresion ";" } "}"

contenido-parentesis ::= <vacío> | expresion resto-parentesis
resto-parentesis     ::= <vacío> | "," { expresion "," }

dato-primitivo ::= entero | decimal | texto | "True" | "False" | identificador

primitiva-binaria ::= "+" | "~" | "/" | "*" | "%" | "concat"
                    | ">" | "<" | ">=" | "<=" | "!=" | "=="

primitiva-unaria  ::= "longitud" | "add1" | "sub1" | "neg"
```

> Nota: el código declara además `switch` como operando (ver §11) y `%` como primitiva binaria (ver §9).

### Semántica de los paréntesis

| Sintaxis        | Significado                          |
| --------------- | ------------------------------------ |
| `()`            | Lista vacía                          |
| `(e)`           | Agrupación — retorna el valor de `e` |
| `(e,)`          | Lista de un elemento                 |
| `(e1, e2, ...)` | Lista de varios elementos            |

### Precedencia de operadores binarios

De mayor a menor prioridad (todos con asociatividad a izquierda):

1. `*` `/` `%`
2. `+` `~`
3. `concat`
4. `>` `<` `>=` `<=` `!=` `==`

**Importante:** las comparaciones no se encadenan. `1 < 2 < 3` se evalúa como `(1 < 2) < 3`, es decir `True < 3`, lo que produce un error de tipos. `set` tiene la precedencia más baja; para usarlo dentro de una operación aritmética debe ir entre paréntesis: `1 + (set x -> 5)`.

`if` es un operando, por lo que puede usarse dentro de operaciones: `10 + if (True) { 1 } else { 2 } * 3` → `13`.

## 6. Sistema de tipos

### Tipos primitivos

| Tipo LogicFusion | Tipo Racket                           | Literales       |
| ---------------- | ------------------------------------- | --------------- |
| `int`            | entero exacto (`integer?` y `exact?`) | `0`, `42`, `-7` |
| `float`          | número real (`real?`)                 | `3.14`, `-0.5`  |
| `string`         | cadena (`string?`)                    | `"hola"`, `""`  |
| `bool`           | booleano (`boolean?`)                 | `True`, `False` |
| `void`           | cualquier valor                       | —               |

### Tipos compuestos

| Tipo LogicFusion | Tipo Racket        | Ejemplo                     |
| ---------------- | ------------------ | --------------------------- |
| `list<T>`        | lista (`list?`)    | `list<int>`, `list<string>` |
| `vector<T>`      | vector (`vector?`) | `vector<float>`             |
| `dict<K,V>`      | — (reservado)      | `dict<string, int>`         |
| `sat`            | — (reservado)      | —                           |
| `proc`           | — (reservado)      | —                           |

La validación de tipos compuestos verifica solo la estructura (si es lista o vector), no el tipo interno de los elementos. Las constantes declaradas con `const` no pueden ser reasignadas; intentar hacerlo con `set` produce un error en tiempo de ejecución.

**Coerción (`coercionar-tipo`):**

- `float` ← cualquier número: `exact->inexact` (convierte `int` a `float`).
- `int` ← decimal: `inexact->exact` + `truncate` (trunca la parte decimal).
- `string`, `bool`, `void`, listas y vectores: sin cambio.

## 7. Estructura de un programa

Todo programa LogicFusion tiene exactamente dos bloques:

```
GLOBALS {
    $ Definiciones accesibles en todo el programa
    int base = 10;
    const float pi = 3.14;
}
PROGRAM {
    $ Definiciones del bloque principal
    float area = pi * base * base;
}
```

- El bloque `GLOBALS` se evalúa primero y construye el ambiente global.
- El bloque `PROGRAM` se evalúa en el ambiente extendido por `GLOBALS`.
- El intérprete **retorna el valor de la última definición** del bloque `PROGRAM` (`obtener-ultimo-valor` sobre el ambiente final).

## 8. Expresiones y operandos

### Definición de variable

```
tipo identificador = expresion;
```

```
int x = 5 * 2;
string saludo = "hola" concat " mundo";
bool activo = True;
```

### Definición de constante

```
const tipo identificador = expresion;
```

```
const int MAX = 100;
const float PI = 3.14159;
```

### Asignación (`set`)

Modifica el valor de una variable ya definida. No funciona sobre constantes. El nuevo valor debe coincidir con el tipo declarado.

```
set identificador -> expresion
```

Dentro de un bloque:

```
GLOBALS { int x = 0; }
PROGRAM {
    int r = BLOCK {
        set x -> 42;
        x;
    };
}
```

Dentro de una operación (requiere paréntesis):

```
int r = 1 + (set x -> 5);
```

### BLOCK

Evalúa una secuencia de expresiones y retorna el valor de la última. Requiere al menos una expresión.

```
BLOCK {
    expresion1;
    expresion2;
    expresionN;
}
```

### LOCALS

Define variables locales visibles solo dentro del cuerpo del bloque. Requiere al menos una definición y al menos una expresión en el cuerpo.

```
LOCALS {
    tipo id = expresion;
} {
    expresion1;
    expresionN;
}
```

```
int r = LOCALS {
    int temp = 5;
    int doble = temp * 2;
} {
    doble + 1;
};
```

## 9. Primitivas

### Primitivas binarias

| Operador          | Nombre               | Tipos válidos          | Resultado       |
| ----------------- | -------------------- | ---------------------- | --------------- |
| `+`               | suma                 | `int`, `float`         | `int` o `float` |
| `~`               | resta                | `int`, `float`         | `int` o `float` |
| `*`               | multiplicación       | `int`, `float`         | `int` o `float` |
| `/`               | división             | `int`, `float`         | `int`o `float`  |
| `%`               | módulo (resto)       | `int`                  | `int`           |
| `concat`          | concatenación        | `string`               | `string`        |
| `>` `<` `>=` `<=` | comparación de orden | `int`, `float`, `bool` | `bool`          |
| `==` `!=`         | igualdad             | `int`, `float`, `bool` | `bool`          |

**División (`dividir`):** convierte ambos operandos a `inexact` y usa `/`, por lo que el resultado es siempre `float` (p. ej. `6 / 3` → `2.0`). Divisor `0` → error. Si se necesita división entera, el resultado puede pasarse a `int` por coerción.

**Módulo (`modular`):** exige ambos operandos enteros exactos (`entero?`); divisor `0` → error. Usa `remainder`.

**Comparaciones (`condicional?`):** dos números se comparan por valor (`1 == 1.0` es `#t`); dos booleanos se convierten a `0`/`1` (`False < True`); un bool contra un número lanza un error explicativo (encadenamiento de comparaciones); tipos de categorías distintas → error.

### Primitivas unarias

| Operador      | Tipos válidos         | Resultado                    | Ejemplo                        |
| ------------- | --------------------- | ---------------------------- | ------------------------------ |
| `longitud(e)` | `string`              | `int` — número de caracteres | `longitud("hola")` → `4`       |
| `add1(e)`     | `int`, `float`        | mismo tipo + 1               | `add1(7)` → `8`                |
| `sub1(e)`     | `int`, `float`        | mismo tipo − 1               | `sub1(3.0)` → `2.0`            |
| `neg(e)`      | `bool`, `int`/`float` | `bool` — negación lógica     | `neg(True)` → `False`, `neg(0)` → `True` |

`neg` sobre números sigue la convención del lenguaje: `0` es falso (`neg(0)` es `True`), cualquier otro número es verdadero (`neg(5)` es `False`).

## 10. Operaciones sobre listas

Las listas son **listas enlazadas** construidas recursivamente con `make-list` y `empty`.

| Operación   | Sintaxis               | Descripción                             |
| ----------- | ---------------------- | --------------------------------------- |
| Lista vacía | `empty`                | Retorna la lista vacía `()`             |
| ¿vacía?     | `empty?(lst)`          | `True` si `lst` es vacía                |
| Construir   | `make-list(elem, lst)` | Agrega `elem` al frente de `lst`        |
| ¿es lista?  | `list?(e)`             | `True` si `e` es una lista              |
| Cabeza      | `head(lst)`            | Primer elemento de `lst`                |
| Cola        | `tail(lst)`            | `lst` sin el primer elemento            |
| Unir        | `append(lst1, lst2)`   | Concatena dos listas                    |
| Acceso      | `ref-list(lst, pos)`   | Elemento en la posición `pos` (desde 0) |
| Longitud    | `length(lst)`          | Número de elementos                     |

```
GLOBALS { }
PROGRAM {
    list<int> nums = make-list(1, make-list(2, make-list(3, empty)));
    int primero   = head(nums);
    list<int> resto = tail(nums);
    int n         = length(nums);
    int segundo   = ref-list(nums, 1);
}
```

Lista por sintaxis de paréntesis:

```
list<int> x     = (1, 2, 3,);
list<int> uno   = (42,);
list<int> vacia = ();
```

## 11. Estructuras de control

### Condicional `if / else`

`if` es un **operando**, por lo que puede usarse dentro de operaciones aritméticas. La condición puede ser `bool` o número (`0` es falso, cualquier otro número es verdadero).

```
if (expresion-condicion) { expresion-verdadera } else { expresion-falsa }
```

```
GLOBALS { int edad = 20; }
PROGRAM {
    string r = if (edad >= 18) { "Mayor" } else { "Menor" };
}
```

`if` anidado:

```
string r = if (edad < 13) {
    "Niño"
} else {
    if (edad < 18) { "Adolescente" } else { "Adulto" }
};
```

Como operando en una expresión aritmética:

```
int r = 10 + if (True) { 1 } else { 2 } * 3;
$ Resultado: 10 + (1 * 3) = 13
```

### `switch`

Compara el valor de una expresión con cada `case` en orden y evalúa el primer cuerpo que coincida. Si ninguno coincide evalúa `default`. El `default` es obligatorio.

```
switch (expresion) {
    case valor1: { expresion1 }
    case valor2: { expresion2 }
    default:     { expresion-default }
}
```

```
GLOBALS { string color = "rojo"; }
PROGRAM {
    string r = switch (color) {
        case "rojo":  { "Para" }
        case "verde": { "Sigue" }
        default:      { "Color inválido" }
    };
}
```

Los valores de los `case` son expresiones arbitrarias evaluadas en orden. La comparación usa `valores-iguales?`: `=` entre números, `string=?` entre cadenas, `eq?` entre booleanos.

## 12. Ambientes y referencias

El intérprete implementa un modelo de **paso por valor con mutación explícita** mediante referencias.

### Estructura del ambiente

Cada marco (frame) del ambiente (`ambiente-extendido`) almacena cuatro vectores paralelos más el ambiente anterior:

| Campo      | Tipo     | Contenido                          |
| ---------- | -------- | ---------------------------------- |
| `tipos`    | `vector` | Tipo declarado de cada variable    |
| `simbolos` | `list`   | Nombre de cada variable            |
| `valores`  | `vector` | Referencia mutable al valor actual |
| `es-const` | `vector` | `#t` si la variable es constante   |
| `amb`      | `Amb`    | Ambiente anterior (cadena)         |

Los marcos se encadenan: cada `ambiente-extendido` apunta al anterior, formando una cadena que termina en `ambiente-vacio`.

### Referencias

La mutación se implementa con el tipo `referencia`:

```racket
(define-datatype referencia referencia?
  (a-ref (posicion integer?)
         (vector vector?)))
```

Una referencia guarda la **posición** dentro del vector de valores y el **vector** mismo. Leer una referencia se hace con `deref`; mutarla con `setref!`.

### Búsqueda y asignación

- `aplicar-ambiente` → valor actual (usa `deref`).
- `aplicar-ambiente-referencia` → la referencia (permite leer y mutar).
- `buscar-ligadura-para-modificar` → `(referencia, tipo, es-const)`; lo usa `aplicar-asignacion`.
- `aplicar-asignacion` valida que no sea constante y que el nuevo valor respete el tipo antes de `setref!`.

## 13. Definiciones formales de procedimientos

Notación: `Dominio -> Codominio`. Abreviaturas: `V` = valor arbitrario, `N` = número, `B` = booleano, `S` = cadena, `L(X)` = lista de X, `E` = expresión (nodo del ASA), `Amb` = ambiente, `PB` = primitiva binaria, `PU` = primitiva unaria, `Pred` = predicado `V -> B`, `Sym` = símbolo, `Proc` = procedimiento Racket, `Ref` = referencia.

### `evaluar-programa`

**Propósito:** `Programa -> V`

Punto de entrada del intérprete. Evalúa primero las definiciones de `GLOBALS` en el ambiente vacío, luego las de `PROGRAM` en el ambiente resultante, y retorna el valor de la última definición de `PROGRAM`.

### `obtener-ultimo-valor`

**Propósito:** `Amb -> V`

Dado un ambiente extendido, retorna el valor de la variable más recientemente definida (posición 0 del vector de valores del marco más externo). Si el ambiente está vacío lanza un error.

### `recorrer-lista-definicion`

**Propósito:** `L(Def) × Amb -> Amb`

Procesa una lista de definiciones en orden, extendiendo el ambiente con cada una. Es recursiva sobre la cola: cada definición ve las anteriores.

### `evaluar-definicion`

**Propósito:** `Def × Amb -> Amb`

Evalúa una definición (variable o constante): (1) evalúa la expresión inicial, (2) aplica `coercionar-tipo` según el tipo declarado, (3) valida con `validar-tipo`, (4) crea un nuevo marco `ambiente-extendido` con el valor en un vector mutable. Para constantes, `es-const` se guarda como `#t`. Si la validación falla, lanza un error descriptivo.

### `validar-tipo`

**Propósito:** `Tipo × V -> B`

Verifica que un valor corresponda al tipo declarado. Para tipos primitivos usa los predicados de Racket (`entero?`, `real?`, `string?`, `boolean?`); `void` siempre es válido. Para tipos compuestos verifica la estructura (`list?`, `vector?`).

### `coercionar-tipo`

**Propósito:** `Tipo × V -> V`

Ajusta el valor al tipo declarado antes de validarlo: `float` ← `exact->inexact`; `int` ← `inexact->exact` + `truncate`; `string`, `bool`, `void`, listas y vectores sin cambio.

### `evaluar-expresion`

**Propósito:** `E × Amb -> V`

Despacha sobre los dos tipos de expresión: `asignar-expresion` delega en `aplicar-asignacion`; `expresion-operaciones` evalúa todos los operandos, recolecta los operadores y delega en `evaluar-secuencia` para respetar la precedencia.

### `evaluar-operando`

**Propósito:** `Operando × Amb -> V`

Evalúa un nodo operando según su variante:

| Variante                     | Comportamiento                                              |
| ---------------------------- | ----------------------------------------------------------- |
| `operando-primitivo`         | Delega en `evaluar-dato-primitivo`                          |
| `operando-parentesis`        | `()` → lista vacía; `(e)` → valor de `e`; `(e,...)` → lista |
| `operando-condicional`       | Evalúa condición; elige rama con `valor-verdad?`            |
| `operando-switch`            | Delega en `evaluar-switch`                                  |
| `bloque-expresion`           | Evalúa secuencia; retorna último valor                      |
| `locales-expresion`          | Extiende ambiente con definiciones locales, evalúa cuerpo   |
| `primitiva-unaria-expresion` | Evalúa argumento; delega en `evaluar-primitiva-unaria`      |
| `lista-vacia`                | Retorna `'()`                                               |
| `vacio-predicado`            | `(null? ...)`                                               |
| `crear-lista`                | `(cons cabeza cola)`                                        |
| `lista-predicado`            | `(list? ...)`                                               |
| `cabeza-lista`               | `(car ...)` con guarda de lista vacía                       |
| `cola-lista`                 | `(cdr ...)` con guarda de lista vacía                       |
| `concatenar-listas`          | `(append l1 l2)` con validación                             |
| `referencia-lista`           | `(list-ref l pos)` con validación de rango                  |
| `longitud-lista`             | `(length l)` con validación                                 |

### `evaluar-dato-primitivo`

**Propósito:** `DatoPrimitivo × Amb -> V`

Evalúa literales e identificadores. Números: retorna el valor directamente. Cadenas: aplica `quitar-comillas`. `True`/`False`: retorna `#t`/`#f`. Identificador: busca el valor en el ambiente con `aplicar-ambiente`.

### `evaluar-cuerpo-secuencia`

**Propósito:** `E × L(E) × Amb -> V`

Evalúa en orden una secuencia de expresiones y retorna el valor de la última. Es la semántica de `BLOCK` y `LOCALS`.

### `evaluar-secuencia`

**Propósito:** `L(V) × L(PB) -> V`

Aplica la precedencia de operadores a una lista plana de valores y operadores alternados. Usa cuatro pasadas con `reducir-nivel`: (1) `*`/`/`/`%`, (2) `+`/`~`, (3) `concat`, (4) comparaciones. Si al final queda más de un valor sin operador, lanza un error.

### `reducir-nivel`

**Propósito:** `L(V) × L(PB) × Pred -> (L(V) × L(PB))`

Recorre de izquierda a derecha la lista de valores y operadores. Cuando el operador cumple el predicado, lo aplica inmediatamente y acumula el resultado. Cuando no lo cumple, conserva el operador y el valor para la siguiente pasada. Retorna `(nuevos-valores nuevos-operadores)`.

### `es-multiplicacion-division?` / `es-suma-resta?` / `es-concat?` / `es-comparacion?`

**Propósito:** `PB -> B`

Predicados de nivel de precedencia usados por `evaluar-secuencia`. `es-multiplicacion-division?` incluye `%`.

### `evaluar-primitiva-binaria`

**Propósito:** `PB × V × V -> V`

Despacha sobre la primitiva: aritméticas (`+`, `~`, `*`) delegan en `operar-numeros`; división (`/`) en `dividir`; módulo (`%`) en `modular`; `concat` en `concatenar`; comparaciones en `condicional?`.

### `evaluar-primitiva-unaria`

**Propósito:** `PU × V -> V`

`longitud` → `longitud-de-cadena`; `add1` → `operar-numero` con `(+ n 1)`; `sub1` → `operar-numero` con `(- n 1)`; `neg` → `negar-valor`.

### `operar-numeros`

**Propósito:** `Sym × Proc × V × V -> N`

Guarda de tipo para operaciones aritméticas binarias. Verifica que ambos valores sean numéricos; si no, lanza un error que incluye el nombre de la primitiva y los valores recibidos.

### `operar-numero`

**Propósito:** `Sym × Proc × V -> N`

Versión unaria de `operar-numeros`. Usada por `add1` y `sub1`.

### `dividir`

**Propósito:** `V × V -> N`

Convierte ambos operandos a `inexact` y usa `/`. Divisor `0` → error. Se evita `/` directo sobre exactos porque `(/ 7 2)` produce la fracción `7/2`, que rompe la validación de tipos.

### `modular`

**Propósito:** `V × V -> N`

Exige dos enteros exactos y usa `remainder`. Divisor `0` → error.

### `entero?`

**Propósito:** `V -> B`

Retorna `#t` si el valor es un entero exacto de Racket (`integer?` y `exact?`). Distingue `4` (int) de `4.0` (float).

### `concatenar`

**Propósito:** `V × V -> S`

Une dos cadenas con `string-append`. Si alguno no es `string`, lanza un error.

### `longitud-de-cadena`

**Propósito:** `V -> N`

Retorna `(string-length valor)`. Si el valor no es cadena, lanza un error.

### `negar-valor`

**Propósito:** `V -> B`

Negación lógica: `bool` → invierte con `not`; número → retorna `(zero? n)` (solo `0` es falso); otro tipo → error.

### `condicional?`

**Propósito:** `Sym × V × V -> B`

Aplica un operador de comparación a dos valores. Dos números → `comparar-numeros`. Dos booleanos → se convierten a `0`/`1` y se comparan numéricamente. Bool vs número → error explicativo (encadenamiento de comparaciones). Otros tipos distintos → error.

### `comparar-numeros`

**Propósito:** `Sym × N × N -> B`

Aplica el operador de comparación (`>`, `<`, `>=`, `<=`, `==`, `!=`) a dos números mediante `cond` sobre el símbolo.

### `valor-verdad?`

**Propósito:** `V -> B`

Interpreta un valor como condición booleana: `bool` se usa directamente; número → `0` es falso, cualquier otro número es verdadero; otro tipo → error.

### `valores-iguales?`

**Propósito:** `V × V -> B`

Comparación de igualdad: números → `=`; cadenas → `string=?`; booleanos → `eq?`; tipos distintos → `#f`. Usada por `evaluar-switch`.

### `evaluar-switch`

**Propósito:** `V × L(E) × L(E) × E × Amb -> V`

Recorre los `case` en orden. Evalúa el valor de cada caso y, cuando coincide (usando `valores-iguales?`), evalúa y retorna el cuerpo correspondiente. Si se agota la lista, evalúa y retorna el cuerpo de `default`.

### `aplicar-ambiente`

**Propósito:** `Amb × Sym -> V`

Busca el símbolo en la cadena de marcos y retorna su valor actual (leyendo la referencia con `deref`). Si no existe, lanza un error.

### `aplicar-ambiente-referencia`

**Propósito:** `Amb × Sym -> Ref`

Busca el símbolo y retorna la **referencia** (no el valor). Permite tanto leer como mutar la variable.

### `buscar-ligadura-para-modificar`

**Propósito:** `Amb × Sym -> (Ref × Tipo × B)`

Busca el símbolo y retorna una lista con: (1) la referencia, (2) el tipo declarado, (3) si es constante. Usada por `aplicar-asignacion`.

### `aplicar-asignacion`

**Propósito:** `Sym × Amb × V -> V`

Implementa `set id -> expr`: (1) obtiene referencia, tipo y bandera de constante; (2) si es constante → error; (3) si el tipo no coincide → error; (4) muta la referencia con `setref!` y retorna el nuevo valor.

> Nota: `aplicar-asignacion` valida el tipo con `validar-tipo` directamente (no aplica coerción); asignar `5.0` a un `int` falla aunque la definición inicial sí coerción.

### `deref` / `primitiva-deref`

**Propósito:** `Ref -> V`

Lee el valor actual almacenado en la referencia usando `vector-ref`.

### `setref!` / `primitiva-setref`

**Propósito:** `Ref × V -> void`

Muta el valor almacenado en la referencia usando `vector-set!`.

### `quitar-comillas`

**Propósito:** `S -> S`

El scanner conserva las comillas en el lexema del token `texto`. Esta función retorna el contenido sin la primera ni la última comilla usando `substring`.

### `nombre-tipo-valor`

**Propósito:** `V -> S`

Retorna el nombre del tipo LogicFusion de un valor (`"int"`, `"float"`, `"bool"`, `"string"`, `"list"`, `"vector"` o `"desconocido"`). Usado para mensajes de error legibles.

### `mostrar-valor`

**Propósito:** `V -> V`

Formatea un valor para mensajes de error con la sintaxis de LogicFusion: `#t` → `"True"`, `#f` → `"False"`, cadenas entre comillas.

### `list-index`

**Propósito:** `Pred × L -> N | #f`

Retorna la posición (desde 0) del primer elemento que cumple el predicado, o `#f` si ninguno lo cumple.

### `list-find-position`

**Propósito:** `Sym × L(Sym) -> N | #f`

Busca un símbolo en una lista y retorna su posición. Delega en `list-index` con el predicado `eqv?`.

### `rib-find-position`

**Propósito:** `Sym × L(Sym) -> N | #f`

Alias semántico de `list-find-position` para buscar la posición dentro del marco (_rib_) de un ambiente extendido. La posición indexa simultáneamente los vectores