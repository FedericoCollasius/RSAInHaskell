
{-ACLARACIÓN:
               la función esCarmichael_aux comentadad debajo de esCarmichael_aux2 funciona mucho más rápido que 
               esta última cuando la uso como función auxiliar para esCarmichael pero no la terminé usando por dos motivos; 
               primero porque no estaba seguro si era correcto usar la expresión "mod (m^(n) - m) n == 0" para 
               el PTM cuando nos habían dado otra. Esta es una versión del PTM que encontré investigando más 
               acerca del Teorema en Interent. A su vez, no logro entender como es que son equivalentes.
               Segundo, y fuertemente relacionado con el primer punto, no entiendo porque usando esa expresión
               no necesito pedir que m sea coprimo con n para que funcione esCarmichael. Asumo que eso está 
               implítico en la formula pero no entiendo cómo. 
               Me encantaría, en lo posible, si me pueden dar una explicación matemática para aclarar esta 
               duda. 
               Gracias.
-}


{-Función 1: sonCoprimos
	Dados dos números naturales decide si son coprimos. 
-}
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = sonCoprimos_aux a b 2 == True 

sonCoprimos_aux :: Integer -> Integer -> Integer -> Bool                       
sonCoprimos_aux a b n |n > a || n > b                = True                            
                      | mod a n == 0 && mod b n == 0 = False
                      | mod a n >= 1 || mod b n >= 1 = sonCoprimos_aux a b (n+1) 

{-Función 2: es2Pseudoprimo 
	Dado un número natural decide si es 2-pseudoprimo.
-}
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n | n > 1 && not (esPrimo n) && mod ((2^(n-1)) - 1) n == 0 = True
                 | otherwise                                              = False     

{-Función 3: cantidad3Pseudoprimos
	dado un número natural "m" calcula la cantidad de 3-pseudoprimos que hay entre 1 y m
	inclusive.
-}
cantidad3Pseudoprimos :: Integer -> Integer 
cantidad3Pseudoprimos m = cantidad3Pseudoprimos_aux 1 m 0 

cantidad3Pseudoprimos_aux :: Integer -> Integer -> Integer -> Integer
cantidad3Pseudoprimos_aux n m k |n > m                     = k     
                                |es3Pseudoprimo n == True  = cantidad3Pseudoprimos_aux (n+1) m (k+1)
                                |es3Pseudoprimo n == False = cantidad3Pseudoprimos_aux (n+1) m k                                                  

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo n | n > 1 && not (esPrimo n) && mod ((3^(n-1)) - 1) n == 0 = True
                 | otherwise                                              = False                    

{-Función 4: kesimo2y3Pseudoprimo
	Dado un número natural "k" calcula el k-ésimo número que es simultaneamente 2-pseudoprimo
	y 3-pseudoprimo.
-}
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k = kesimo2y3Pseudoprimo_aux k 1 0 

kesimo2y3Pseudoprimo_aux :: Integer -> Integer -> Integer -> Integer
kesimo2y3Pseudoprimo_aux k n c |k == c                               = n - 1 
                               |es3Pseudoprimo n && es2Pseudoprimo n = kesimo2y3Pseudoprimo_aux k (n+1) (c+1)
                               |otherwise                            = kesimo2y3Pseudoprimo_aux k (n+1) (c)

{-Función 5 (opcional): esCarmichael
	Dado un número natural decide si es un número de Carmichael. 
-}
esCarmichael :: Integer -> Bool  
esCarmichael n = esCarmichael_aux2 n 1

esCarmichael_aux2 :: Integer -> Integer -> Bool
esCarmichael_aux2 n m | esPrimo n || n == 1                         = False
                      | n == m                                      = True 
                      | not (sonCoprimos n m) || esAPseudoprimo n m = esCarmichael_aux2 n (m + 1)
                      | otherwise                                   = False 

esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo n a | n > 1 && not (esPrimo n) && mod ((a^(n-1)) - 1) n == 0 = True
                   | otherwise                                              = False

{-
esCarmichael' :: Integer -> Bool
esCarmichael' n = esCarmichael_aux n 1

esCarmichael_aux :: Integer -> Integer -> Bool               
esCarmichael_aux n m | esPrimo n || n == 1    = False 
                     | n == m                 = True 
                     | mod (m^(n) - m) n == 0 = esCarmichael_aux n (m+1) 
                     | otherwise              = False 
-}

{-Funciones Auxiliares Programadas en Clase 
-}
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n 

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2 

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise    = menorDivisorDesde n (k+1)

{-Explicación de Funciones Auxiliares
	. sonCoprimos_aux: toma 3 valores "a", "b" y "n". "a" y "b" van a ser los números que se 
	  están evaluando si son coprimos y "n" sería el número por el que se divide a "a" y "b". 
	  Si el resto de la división entre a con "n" o "b" con "n" da un número mayor o igual a 1 
	  ("n" no es divisor de "a" o "b"), entonces la función se vuelve a llamar pero con "n+1". 
	  Pero si el resto de la división entre "a" con "n" y "b" con "n" es igual a 0 ("n" es divisor
	  de "a" y "b") entonces la función devuelve "False" ya que comparten un divisor (además del 1) 
	  por lo tanto 1 no va a ser el único divisor positivo que tienen en común. La función 
	  devuelve "True" cuando "n" es mayor a "a" o "n" es mayor a "b" porque esto significa 
	  que "n" no dividió a "a" y a "b" cuando era menor que ellos y un n mayor a "a" y "b" no 
	  puede dividir a ningun "a" y "b" de manera entera por lo tanto no puede ser un divisor.

	. es2Pseudoprimo: toma un solo valor, "n". "n" es evaluado en 3 instancias: si es mayor a 1,
	  si no es primo y si satisface que es divisor del número 2^(n-1)-1. Si falla en cualquiera 
	  de estas tres instancias es porque no cumpla las propiedades para ser 2-pseudoprimo y 
	  devuelve "False". Si cumple todas devuelve "True".

	. cantidad3Pseudoprimo_aux: toma 3 valores "n", "m" y "k". "n" sería la variable donde arranca,
	  "m" seria donde finaliza y "k" vendría a jugar el rol de un "contador". Si "n" es 
	  3-PseudoPrimo , entonces la función se vuelve a llamar pero con un "n + 1" y "k + 1" ya 
	  que la función encontró un 3-PseudoPrimo así que el contador pasa de 0 a 1. Si el 
	  siguiente número "n + 1" no es 3-PseudoPrimo, entonces la función se vuelve a llamar con 
	  "(n + 1) + 1" pero sin sumarle un 1 al "k". 
      Cuando "n" es mayor que "m", significa que la función evaluó todos los valores hasta 
      "m" inclusive y entonces devuelve el "k" que sumó hasta ese punto. 
    . es3Pseudoprimo: funciona igual que es2Pseudoprimo solo que en vez de 2^(n-1)-1 es 
      3^(n-1)-1.

  .  kesimo2y3Pseudoprimo_aux: toma 3 valores; "k" que representa la posición del número que es 
     2-PseudoPrimo y 3-PseudoPrimo. Así que por ejemplo "k = 1" debería devolver el 1105 ya que 
     es el primer número que cumple ser 2-PseudoPrimo y 3-PseudoPrimo. Por otro lado, "n" va a ser 
     el número que se va a estar evaluando a ver si es 2-PseudoPrimo y 3-Pseudoprimo. Finalmente "c" 
     va a contar la cantidad de 2-PseudoPrimos y 3-PseudoPrimos que la función se vaya encontrando en 
     el camino. Es por eso que cuando "n" cumple ser 2-Pseudoprimo y 3-Pseudoprimo, la función se 
     vuelve a llamar pero con "n + 1" y "c + 1". Si el "n" no cumple con esta condición entonces la 
     función vuelve a llamarse con "n + 1" pero sin sumarle un 1 al "c". La recursión finaliza cuando el 
     "k" que dimos es igual a "c" ya que entonces "c" habría llegado al kesimo número que es 2 y 3 
     PseudoPrimo. Por lo tanto la función devuelve el "n" que tiene menos 1. Esto está definidio así 
     porque por ejemplo si uno quiere encontrar el primer número que cumpla estas condiciones, el 1105, 
     entonces la función lo que hará en un momento es evaluar si 1105 es 3PseudoPrimo y 2PseudoPrimo. Como es 
     cierto, se le va a sumar 1 al "n" que ahora sería igual al 1106 y 1 al "c" que ahora sería igual a 1. 
     Como "k" es igual a "c" porque estabamos buscando el primer 2 y 3 PseudoPrimo, la función devolvería 1106. 
     Pero ese número no es el que buscamos, por lo que le restamos 1 para que la función devuelva un resultado 
     verídico.       
     
  .  esCarmichael_aux2: toma 2 valores; "n" que sería el número que es evaluado para ver si es un número de Carmichael 
     y "m" que serían los números menores a "n" que voy a usar como mi "a" en el PTM. Para que un número sea de Carmichael 
     debe cumplir con ciertas condiciones: ser un natural compuesto y si "a" es coprimo con "n", entonces se debe cumplir que 
     son a-PseudoPrimos para todo número natural entre "1" y "n - 1". La primera condición es evaluada cuando pido 
     que la función devuelva "False" si n es primo o 1. La segunda condición es evaluada en la tercer guada cuando pido que 
     si "n" y "m" son Coprimos, entonces que se le sume "1" a "m" ya que los casos donde "n" y "m" son coprimos no van a cumplir 
     el PTF. Si eso no se cumple entonces "n" y "m" son Coprimos y por lo tanto evalua a "n" y "m" si son n-PseudoPrimos. 
     Si esto se cumple, vuelve a llamar a la función pero con "n + 1". Si no se cumple devuelve "False" ya que si un "m" no 
     es Coprimo con "n" ni cumple que es n-PseudoPrimo, es porque no es un número de Carmichael. La función devuelve 
     "True" cuando "n" es igual a "m" ya que esto signfica que probo todos los casos menores a "m" y no fueron "False". 
    . aPseudoPrimo: es una generalización de las funciones 2PseudoPrimo y 3PseudoPrimo programadas anteriormente.    
-}