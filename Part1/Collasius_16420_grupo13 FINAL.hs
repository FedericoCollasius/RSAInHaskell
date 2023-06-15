
{-Función 1: sonCoprimos
	Dados dos números naturales decide si son coprimos.
-}
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = noExisteDivisorComunDesde a b 2 

noExisteDivisorComunDesde :: Integer -> Integer -> Integer -> Bool
noExisteDivisorComunDesde a b n |n > a || n > b = True
                                | otherwise     = not (mod a n == 0 && mod b n == 0) && noExisteDivisorComunDesde a b (n+1)

{-Función 2: es2Pseudoprimo
	Dado un número natural decide si es 2-pseudoprimo.
-}
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = n > 1 && not (esPrimo n) && mod ((2^(n-1)) - 1) n == 0                                           

{-Función 3: cantidad3Pseudoprimos
	dado un número natural "m" calcula la cantidad de 3-pseudoprimos que hay entre 1 y m
	inclusive.
-}
cantidad3PseudoPrimos :: Integer -> Integer
cantidad3PseudoPrimos 1 = 0
cantidad3PseudoPrimos m | es3Pseudoprimo m = 1 + cantidad3PseudoPrimos (m - 1) 
                        | otherwise = cantidad3PseudoPrimos (m - 1)

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo n = n > 1 && not (esPrimo n) && mod ((3^(n-1)) - 1) n == 0

{-Función 4: kesimo2y3Pseudoprimo
	Dado un número natural "k" calcula el k-ésimo número que es simultaneamente 2-pseudoprimo
	y 3-pseudoprimo.
-}
kesimo2y3PseudoPrimo :: Integer -> Integer
kesimo2y3PseudoPrimo 0 = 1 
kesimo2y3PseudoPrimo k = primer2y3PseudoprimoDesde (kesimo2y3PseudoPrimo (k-1))

primer2y3PseudoprimoDesde :: Integer -> Integer  
primer2y3PseudoprimoDesde 1 = primer2y3PseudoprimoMayor 1 
primer2y3PseudoprimoDesde k | es3Pseudoprimo k && es2Pseudoprimo k = primer2y3PseudoprimoMayor k 
                            | otherwise = primer2y3PseudoprimoDesde (k + 1)

primer2y3PseudoprimoMayor :: Integer -> Integer 
primer2y3PseudoprimoMayor k | es3Pseudoprimo (k + 1) && es2Pseudoprimo (k + 1) = k + 1
                            | otherwise = primer2y3PseudoprimoMayor (k + 1)


{-Función 5 (opcional): esCarmichael
	Dado un número natural decide si es un número de Carmichael.
-}
esCarmichael :: Integer -> Bool
esCarmichael n = n /= 1 && not (esPrimo n) && esAPseudoprimoSiCoprimoDesde n 1

esAPseudoprimoSiCoprimoDesde :: Integer -> Integer -> Bool
esAPseudoprimoSiCoprimoDesde n m = n == m || (not (sonCoprimos n m) || esAPseudoprimo n m) && esAPseudoprimoSiCoprimoDesde n (m + 1)

esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo n a = n > 1 && not (esPrimo n) && mod ((a^(n-1)) - 1) n == 0 


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
	. noExisteDivisorComunDesde : toma 3 valores "a", "b" y "n". "a" y "b" van a ser los números que se
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

	. cantidad3PseudoPrimos: la recursión se basa en evaluar a "m" a ver si es 3PseudoPrimo. En caso que 
    sea, la función se vuelve a evaluar pero con (m-1) sumandole 1 ya que m era 3PseudoPrimo. En caso que no
    lo sea, se vuelve a llamar con m-1 pero sin sumarle 1. El caso base es 1 y se evalua a 0 ya que no es
    3 pseudoprimo.

  .  primer2y3PseudoprimoMayor: devuelve el primer número mayor a k que es 2-pseudoprimo y 3-pseudoprimo. Si no usas esta 
     función, entonces se va a trabar en el primer 2 y 3 pseudoprimo. 
  .  primer2y3PseudoprimoDesde: devuelve el primer 2 y 3 pseudoprimo a partir de un numero. 

  .  esAPseudoprimoSiCoprimoDesde: la idea es evaluar todas las condiciones para que un numero sea de carmichael. La funcion
                                   se llama recursivamente al final ya que tiene que cumplir todas estas propiedades para todo
                                   numero menor a n para ver si es Carmichael.  

-}