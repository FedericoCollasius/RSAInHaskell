import Data.Char (ord, chr)

{-Generar Claves: 
	Dados dos números primos p y q, genera un par que contiene una clave pública y una
	clave privada en el formato ((n,d), (n,e)).
-}
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
generarClaves p q | esPrimo p && esPrimo q  && n > 127 = ((n, d), (n, e))
                  | otherwise = undefined
     where
        n = p * q
        e = exDescifrado 2 p q 
        d = solucionEc (e, 1, ((p-1)*(q-1))) 


-- exDescifrado devuelve el primer número coprimo con m que sea menor o igual a m - 2
-- para ser usado como el Exponente de Descifrado en la función generarClaves. 
exDescifrado :: Integer -> Integer -> Integer -> Integer
exDescifrado i p q | sonCoprimos i m && i <= m - 2 = i 
                   | otherwise = exDescifrado (i + 1) p q 
          where 
             m = (p-1)*(q-1)

{-Encriptar:
	Dada una clave pública y un mensaje, lo reemplaza por la lista de enteros que lo
	encripta.
-}
encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar (n, d) [] = []
encriptar (n, d) (a:as) = mod ((fromIntegral(ord a))^d) n : encriptar (n, d) as

{-Desencriptar:
	Dada una clave privada y una lista de enteros que encripta un mensaje, lo
	decripta. 
-}      
desencriptar :: (Integer, Integer) -> [Integer] -> String
desencriptar (n, e) [] = []
desencriptar (n, e) (b:bs) = chr (fromInteger(mod (b^e) n)) : desencriptar (n, e) bs   

{-Opcional:
	Romper  el  código  asociado  a  la  clave  pública  (100337,60953),  desencriptar la siguiente pregunta:
	[33706,38913,58255,99961,77756,23881,220,77756,1606,38913,77756,78982,18800,91658,91658,58255,77756,96593,58255,438,22839,28700,18800,1606,58255,48389]
	encriptar la respuesta, y ponerla como comentario en el T.P.
-}

-- factorizacionEnPrimos devuelve una lista de los primos que divide a un número natural n. Uso el teorema probado en la guia 
-- 4 de álgebra que dice que un número natural es compuesto si y solo si es divisible por algún primo positivo menor
-- o igual a su raíz cuadrada. Por lo tanto empiezo a probar divisiores de n a partir de ese número.       
factorizacionEnPrimos :: Integer -> [Integer]
factorizacionEnPrimos n = factorizacionEnPrimos' n cotaDivisorPrimoN
     where 
          cotaDivisorPrimoN = proximoPrimoMenorDesde (round (sqrt (fromIntegral n)))

-- la función auxiliar factorizacionEnPrimos' toma la cota calculada en factorizar y empieza a buscar primos desde ahí.
-- En caso que n sea primo va a devolver sólo n. Elegí como Caso Base a n = 1 ya que no hay primo que lo pueda
-- dividir.    
factorizacionEnPrimos' :: Integer -> Integer -> [Integer]
factorizacionEnPrimos' 1 p = []
factorizacionEnPrimos' n p | esPrimo n = [n]
                           | mod n p == 0 = p : factorizacionEnPrimos' (div n p) p
                           | otherwise = factorizacionEnPrimos'  n (proximoPrimoMenorDesde p)

-- proximoPrimoMenorDesde toma un valor n natural y devuelve el primer numero primo menor a ese n. Elegí a 2 como Caso
-- Base ya que no hay primos menores a 2. 
proximoPrimoMenorDesde :: Integer -> Integer 
proximoPrimoMenorDesde 2 = 2 
proximoPrimoMenorDesde n | esPrimo (n - 1) = n - 1
                         | otherwise = proximoPrimoMenorDesde (n - 1)

-- desencriptarMensaje toma una clave publica (n, d) y una lista de numeros (x:xs) usados para encriptar un mensaje
-- y devuelve el mensaje encriptado.  
desencriptarClavePublica :: (Integer, Integer) -> [Integer] -> String
desencriptarClavePublica (n, d) (x:xs) = desencriptar (n, e) (x:xs)
     where 
        e = solucionEc (d, 1, (head (factorizacionEnPrimos n) - 1) * (head (tail (factorizacionEnPrimos n)) - 1)) 

-- Respuesta: 
cp = (100337,60953)
m  = [22791,23881,77756,96593,38913,97660,58255,91658,91658,23881,1606,58255,77756,85626,23881,99961,77756,33706,38913,58255,28700,1606,18800,1606,22839,41020]

--Funciones Auxiliaresm

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = noExisteDivisorComunDesde a b 2 

noExisteDivisorComunDesde :: Integer -> Integer -> Integer -> Bool
noExisteDivisorComunDesde a b n |n > a || n > b = True
                                | otherwise     = not (mod a n == 0 && mod b n == 0) && noExisteDivisorComunDesde a b (n+1)

ecEquivalente :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
 where d = mcd a m 

solucionEcConPropAdic :: (Integer, Integer, Integer) -> Integer
solucionEcConPropAdic (a, b, m) = mod (s*b) m
 where (d, s, t) = emcd a m 

solucionEc :: (Integer, Integer, Integer) -> Integer
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

emcd :: Integer -> Integer -> (Integer , Integer , Integer)
emcd a 0 = (a, 1, 2)
emcd a b = (d, t, s - (div a b)*t)
    where 
         (d,s,t) = emcd b (mod a b)

mcd :: Integer -> Integer -> Integer 
mcd _ 1 = 1
mcd a b | a `mod` b == 0 = b 
        | otherwise = mcd b (a `mod` b)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise    = menorDivisorDesde n (k+1)
