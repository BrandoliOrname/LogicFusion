# ==🟢Pregunta No. 1== 

```
% Integrantes - codigo

Jorge Luis Junior Lasprilla Prada - 2420662

% Link del repositorio de GitHub
https://github.com/BrandoliOrname/LogicFusion
```
# ==🟡Pregunta No. 2==

## Muestre mediante **un solo ejemplo** que su **interpretador** es capaz de almacenar todos los siguientes valores denotados:

 - Enteros
 - Flotantes
 - empty -> para representar listas vacías
 - Cadenas de caracteres
 - Booleanos (true, false)
 - Procedimientos
 - Listas
 - Diccionarios
 - Vectores
 - Instancias SAT

Por ejemplo, puede escribir un programa que retorne una lista con los 10 elementos o también puede crear un programa con 10 variables donde se evidencien los valores denotados. 

**Evaluación**: Se asignará el 100% de los puntos si se entrega **1 solo programa** que retorne la lista o almacene los 10 valores en variables (el programa debe funcionar en el interpretador). Cualquier otro caso o si se entrega más de 1 programa, se asignarán máximo hasta 50% de los puntos con un valor de 5% por cada programa **que funcione y que demuestre** que se maneja un tipo de dato particular.

**Recuerde: Los ejercicios en este formulario que no cuenten con una correcta indentación o espaciado NO serán evaluados. Esto incluye aquellos escritos en una sola línea o sin estructura adecuada. Aunque el código pueda funcionar, este tipo de presentación es una mala práctica, dificulta su revisión y retrasa el proceso de calificación. Por favor, asegúrese de organizar el código de manera legible y estructurada.**

### Respuesta

``` logic-fusion
% Numero
GLOBALS {} PROGRAM {int numero = 10;}
10

% Flotante
GLOBALS {} PROGRAM {float decimal = 5.25;}
5.25

% Lista vacia
GLOBALS {} PROGRAM {list<int> listVacia = (empty);}
()

% Booelano
GLOBALS {} PROGRAM {bool activo = True;}
#t

% Procedimiento
Falta por implementar

% Listas
GLOBALS {} PROGRAM {list<int> numeros = (1,2,3);}
(1, 2, 3)

% Diccionarios
Falta por implementar

% Vectores 
Falta por implementar

% Instancias SAT
Falta por implementar
```

# ==🟢Pregunta No. 3==

Muestre mediante un solo ejemplo que su **interpretador** es capaz de manejar variables actualizables o mutables.  El ejemplo deberá contener una variable X con algún valor inicial, luego se debe evidenciar que la variable cambia de valor con alguna instrucción tipo **set o similar**. No es válido un ejemplo donde a través de ambientes se creen variables con el mismo nombre y con distintos valores, es necesario evidenciar la instrucción para modificación de la variable.  

**Evaluación**: Se asignará el 100% de los puntos si se entrega 1 solo programa que retorne una variable con un valor inicial y que es modificada utilizando una instrucción para dicho propósito. Cualquier otro caso o si se entrega más de 1 programa, se asignará 0% de los puntos.

**Nota:** dependiendo de la implementación en el interpretador, es posible que el programa retorne el valor de la variable o algún valor de éxito al ejecutar la instrucción de seteo. Ambas opciones son válidas.

### Respuesta

``` logic-fusion
GLOBALS { int x = 5; }
PROGRAM {
    int r = BLOCK {
        set x -> 10;
        x;
    };
}
```

# ==🟢Pregunta No. 4==

Muestre mediante dos ejemplos que su **interpretador** es capaz de crear variables de asignación única (constantes):

- Un programa debe permitir crear una constante con algún valor asignado y retornar su valor
- Un programa debe permitir crear una constante con algún valor asignado y retornar un error al tratar de modificar su valor a través de la instrucción para modificación de variables de su lenguaje

**Evaluación**: Se asignará el 100% de los puntos sólo si se entregan los dos programas especificados.  Cualquier otro caso, donde no se logre mostrar el soporte para constantes (i.e., variables inmutables), se asignarán 0 pts.

### Respuesta

```logic-fusion
% Crear constante e imprime el valor
GLOBALS {} 
PROGRAM {const int documento = 12345678;}
12345678

% Crear constante y cambia el valor
GLOBALS { const int salario = 1750000; }
PROGRAM {
    int nuevo-salario = BLOCK {
        set salario -> 1600000;
		salario;
    };
}   
aplicar-asignacion: No se puede cambiar el valor de la constante salario
```

# ==🟢Pregunta No. 5==

Muestre mediante un solo ejemplo que su **interpretador** tiene la capacidad de procesar las siguientes Primitivas aritméticas:  +, −, ∗, %, /, para:

- Enteros (aprox. 10% de los puntos por cada primitiva )
- Flotantes (aprox. 10%  de los puntos por cada primitiva)

Debe escribir un solo programa en su lenguaje de programación donde se evidencie la utilización de las **primitivas para enteros, flotantes.**

**Evaluación**: Se asignará el 100% de los puntos si se entrega **1 solo programa** que funcione en el interpretador y que evidencie el uso de las primitivas. Deben pensar como hacerlo en 1 solo programa .  Cualquier otro caso o si se entrega más de 1 programa, se asignarán máximo hasta 50% de los pts, es decir, la mitad del total de los puntos.

**Recuerde: Los ejercicios en este formulario que no cuenten con una correcta indentación o espaciado NO serán evaluados. Esto incluye aquellos escritos en una sola línea o sin estructura adecuada. Aunque el código pueda funcionar, este tipo de presentación es una mala práctica, dificulta su revisión y retrasa el proceso de calificación. Por favor, asegúrese de organizar el código de manera legible y estructurada.**

### Respuesta

Falta el caso de %

```logic-fusion
% Enteros
GLOBALS {
	int a = 10;
	int b = 20;
	int sumar = a + b;
	int restar = a ~ b;
	int multiplicar = a * b;
	int dividir = a / b;
	int modulo = a % b;
}
PROGRAM {list<void> resultado = (sumar, restar, multiplicar, dividir, modulo);}
(30 -10 200 0 10)

% Flotantes
GLOBALS {
	float a = 10.5;
	float b = 3.0;
	float sumar = a + b;
	float restar = a ~ b;
	float multiplicar = a * b;
	float dividir = a / b;
	float modulo = a % b;
}
PROGRAM {list<void> resultado = (sumar, restar, multiplicar, dividir, modulo);}
(13.5 7.5 31.5 3.5 1.5)
```

# ==🟡Pregunta No. 6==
Muestre mediante **un solo programa** que su **interpretador** tiene la capacidad de procesar las siguientes Primitivas booleanas:  (<, >, <=, >=, ='=, !=) para int, float y (and, or, not) para booleanos.

Debe escribir un solo programa en su lenguaje de programación donde se evidencie la utilización de las 9 primitivas (aproximadamente 11.11% del total de los puntos por cada una).

**Evaluación**: Se asignará el 100% de los puntos si se entrega **1 solo programa** que funcione en el interpretador y que evidencie el uso de las 9 primitivas, deben pensar como hacerlo en 1 solo programa (cada primitiva tiene un valor de 11.11% del total). Cualquier otro caso o si se entrega más de 1 programa, se asignarán máximo hasta el 50% de los puntos por cada programa que demuestre que cada primitiva funciona.

### Respuesta

Nos falta operadores como AND y OR y NOT

```logic-fusion
% Numeros
GLOBALS {
	int a = 10;
	int b = 15;
	int c = 15;
	bool mayor = a > b; % False
	bool menor = b < a; % False
	bool mayor-igual = b >= a; % True
	bool menor-igual = a <= b; % True
	bool igual = b == c; % True
	bool desigual = a != b; % True
}
PROGRAM {
	list<bool> resultado = 
	(mayor, menor, mayor-igual, menor-igual, igual, desigual);
}
(#f #f #t #t #t #t)

% Booleanos
GLOBALS {
	
}
PROGRAM {}
```

# ==🟢Pregunta No. 7==

Muestre mediante un solo ejemplo que su **interpretador** tiene la capacidad de procesar las Primitivas (longitud, concatenar) para cadenas.  

Debe escribir un solo programa en su lenguaje de programación donde se evidencie la utilización de las 2 primitivas.

**Evaluación**: Se asignará el 100% de los puntos si se entrega **1 solo programa** que funcione en el interpretador y que evidencie el uso de las 2 primitivas, deben pensar como hacerlo en 1 solo programa (cada primitiva tiene un valor del 50% de los pts). Cualquier otro caso o si se entrega más de 1 programa no se asignarán puntos.

### Respuesta

```logic-fusion
GLOBALS {
	string saludo = "Hola";
	string nombre = "Oswaldo";
}
PROGRAM {
	string saludar = saludo concat " " concat nombre;
	int caracteres = longitud(saludar);
	list<void> respuesta = (saludar, caracteres);
}
("Hola Oswaldo" 12)
```

# ==🔴Pregunta No. 8==

Demuestre que su **interpretador** es útil para programar. 

Elabore una función en su lenguaje de programación que reciba una lista de enteros L y retorne un registro (o diccionario) con dos claves: "valores" y "factoriales". La clave "valores" debe tener asociada una lista con los mismos valores de L y la clave "factoriales" debe tener asociada un vector con el factorial de cada valor de la lista L.  Las funciones auxiliares (si las requiere) se pueden desarrollar utilizando los ciclos de repetición del lenguaje o recursión.

A continuación un posible ejemplo:

```problema
registroFactorial( (1, 2,3,4,7,9) )

//retorna un registro en su representación interna. Por ejemplo

{
	"valores": (1,2,3,4,7,9), 
    "factoriales": [1,2,6,24,5040,362880]
}
```

//La sintaxis del ejemplo es simplemente explicativa y puede diferir de la suya.

Puede crear las funciones auxiliares que requiera

**Evaluación**: Se asignará el 100% de los puntos si el programa cumple con los requerimientos. De lo contrario:  
- Si sólo retorna la lista con los factoriales se asignará el 20% de los puntos  
- Si sólo retorna el registro con la lista de valores se asignaran 30% de los puntos  
- Si sólo retorna el registro con el vector de factoriales se asignaran 40% de los puntos

### Respuesta

Nos falta crear la estructura de dato vector, diccionario y procedimientos recursivos

``` logic-fusion
GLOBALS {}
PROGRAM {}
```

# ==🔴Pregunta No. 9==

Demuestre que su **interpretador** es útil para programar. 

Elabore la función "map" en su lenguaje de programación. La función "map" recibe una lista L y una función unaria F. "map" debe retornar una lista donde se le ha aplicado la función F a cada elemento de la lista L (utilice las funciones auxiliares necesarias). **La implementación debe hacerse a través de recursión.** 

**Evaluación**: Se asignará el 100% de los puntos si el programa cumple con los requerimientos. En cualquier otro caso se asignarán 0pts.

### Respuesta

``` logic-fusion
GLOBALS {}
PROGRAM {}
```

# ==🔴Pregunta No. 10==

Mediante 2 ejemplos muestre que su **interpretador** tiene la capacidad de utilizar ciclos de repetición. Para esto, escriba dos programas independientes en su lenguaje de programación (uno con **while** y otro con **for**) que:

a) contenga un ciclo de repetición **"for"** que itere sobre una **lista** con valores de 1 a 5 y construya una **lista** con los valores recíprocos de cada elemento de la lista origina (i.e., 1/x). En cada iteración se deberá mostrar el valor de la **lista** original y su recíproco calculado en la **lista** resultante. Finalizado el ciclo **for**, se deberá imprimir o retornar la lista que se ha construido.

b) contenga un ciclo de repetición **"while"** que itere de 1 a 5 e invoque al procedimiento "esPar?" con la variable del ciclo que utiliza (i.e., false,true,false,true,false). El programa podrá retornar ya sea una lista o imprimir los valores en pantalla.

**Evaluación**: Se asignará el 100% de los puntos por ambos programas si funcionan y cumplen con los requerimientos (i.e., 50% para el programa con for y 50% para el programa con while).

### Respuesta 

``` logic-fusion
GLOBALS {}
PROGRAM {}
```

# ==🔴Pregunta No. 11==

Muestre mediante programas que su **interpretador**  tiene la capacidad de manejar tipos, para esto deberá proveer varios programas:

**Programa 1 (20% pts):**  Muestre mediante un solo ejemplo que su interpretador tiene la capacidad de generar algún error de tipo **int!= bool** validado desde su interpretador y no por racket. En caso que su interpretador no incluya errores de este tipo, debe explicar las razones técnicas de dicha decisión.

**Programa 2 **(20% pts)**:**  Muestre mediante un solo ejemplo que su interpretador tiene la capacidad de generar algún error de tipo **string!= float** validado desde su interpretador y no por racket. En caso que su interpretador no incluya errores de este tipo, debe explicar las razones técnicas de dicha decisión.  

**Programa 3 **(20% pts)**:**  Muestre mediante un solo ejemplo que su interpretador tiene la capacidad de generar algún error de tipo **list != vector** o **list != dict** validado desde su interpretador y no por racket. En caso que su interpretador no incluya errores de este tipo, debe explicar las razones técnicas de dicha decisión.

**Programa 4 **(20% pts)**:**  Muestre mediante un solo ejemplo que su interpretador tiene la capacidad de generar algún otro error de tipo. Por ejemplo, la validación de los tipos de datos internos de una lista o de un vector o de un diccionario.

**Programa 5 (20% pts):**  Muestre mediante un solo ejemplo que su interpretador no genera errores de tipo para variables "void".

Usted podrá sumar el 100% de los puntos si incluye los cinco programas o si la justificación de la ausencia de la validación de alguno(s) de los tipos es válida.

### Respuesta 

Revisar la funcion **condicional?** para tratar estos casos, aun no hay verificación por cada elemento de un tipo compuesto

``` logic-fusion
% Programa 1
GLOBALS {
	int numero = 10;
	string estudiante = "True";
}
PROGRAM {
	void resultado = numero != estudiante; 
}
evaluar-primitiva-binaria: La comparacion != solo acepta int, float o bool del mismo tipo; se recibio un int (10) y un string ("True").

% Programa 2
...

```

# ==🔴Pregunta No. 12==

Demuestre que su **interpretador** es útil para programar.   

Aunque el interpretador NO implementa el paradigma orientado a objetos, mediante 1 programa muestre que su **interpretador** tiene la capacidad de **SIMULAR objetos a través de funciones con estado (utilice funciones de despacho como se mostró en clase y haga uso de un Switch).** Esto se logra a través del uso de los paradigmas funcional e imperativo como se estudió en clase. Para esto, debe implementar una representación de las clases que se muestran en la imagen bajo el paradigma funcional utilizando estados locales a las funciones:

![[POO.png.png]]

La evaluación se realizará asÍ:

- **100% de los puntos:** si entrega una representación que permita "crear objetos" o funciones con estado que representen una "moto", que a su vez tienen acceso a otra función con estado que representan un vehículo simulando así la herencia. **La implementación deberá evidenciar la creación de 3 motos con diferentes cilindradas, marcas y modelos. Debe implementar las funciones setter y getter para cada "atributo".**

- **50% de los puntos:** si no logra representar la herencia pero logra "crear objetos" o funciones con estado que representen una "moto" con los atributos del vehículo. **La implementación deberá evidenciar la creación de 3 motos con diferentes cilindradas, marcas y modelos. Debe implementar las funciones setter y getter para cada "atributo".**

- **Cualquier otra representación diferente a funciones con estado se le asignarán 0 pts.**
### Respuesta 

``` logic-fusion
GLOBALS {}
PROGRAM {}
```
# ==🔴Pregunta No. 13==


Mediante 1 programa muestre que su **interpretador** tiene la capacidad de dar soporte a instancias SAT. El programa debe contener dos variables con la representación de los siguientes problemas:

a) (x∨x∨x)∧(¬x∨¬x∨¬x)

b) (x ∨ y) ∧ (¬x ∨ ¬y)

El programa podrá retornar ya sea una lista o un vector o imprimir los valores de las dos soluciones en pantalla.

**Evaluación**: Se asignarán el 100% de los puntos por el programa si funciona y cumple con los requerimientos.
### Respuesta 

``` logic-fusion
GLOBALS {}
PROGRAM {}
```