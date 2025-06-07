--Ejercicio N°5: Definir una función que cuente los elementos pares tiene una lista de números.

cuentaPares :: (Integral a) => [a] -> Int
cuentaPares lista = length [x | x <- lista, even x]

--Ejercicio Nº 6: Definir una función que reciba una lista de listas y entregue la cantidad de elementos de la lista de mayor longitud.     

mayorLista :: [[a]] -> Int
mayorLista lista = maximum [length x | x <- lista]

--Ejercicio Nº 7: Definir una función que transforme una lista de números en otra lista que contenga el cubo de cada elemento.

cuboLista :: (Integral a) => [a] -> [a]
cuboLista lista = [x^3 | x <- lista]

--Definir una función que entregue rango de valores que toma una lista

rangoLista :: (Integral a) => [a] -> [a]
rangoLista lista = [maximum lista, minimum lista]

--Ejercicio Nº 8: Definir una función recursiva que permita eliminar los elementos repetidos de una lista de átomos.

-- Cuales serían los pasos?
--1)Tomar el primer elemento de la lista
--2)Verificar que no se repita,si se repite elimino las repeticiones
--3)Volver a llamar a la función con el resto de la lista

eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos [] = []
eliminarRepetidos(x:xs) = x : eliminarRepetidos (filter (/=x) xs)

--Ejercicio Nº 9: Implementar una función recursiva que pase un número decimal a binario.

{-Para convertir un número de decimal a binario, tengo que realizar divisiones sucesivas, el flujo sería el siguiente:
1)Tomo un número, por ejemplo el 15, de ahí empiezo a dividir por 2:
    Al dividir 15 en 2, me queda 7 de cociente y resto 1, ese 1 es el primer digito binario
    Luego divido 7 en 2, me queda 3 de cociente y resto 1, es el segundo dígito binario
    Luego divido 3 en 2, cociente 1 y resto 1, es el tercer digito binario
    1 no es divisible por dos, ese es el último dígito binario
    El resultado es 1111, que es 15 en binario
IMPORTANTE: El número binario se construye con los dígitos obtenidos, primero el último obtenido, de ahí para atrás-}

decimalBinario :: (Integral a)=> a-> [a]
decimalBinario 1 = [1]
decimalBinario 0 = [0]
decimalBinario x = decimalBinario (div x 2) ++ [mod x 2]

--Ejercicio Nº 10: Implementar una función recursiva que permita obtener la unión de dos listas dadas; los elementos repetidos solo deben aparecer una vez.

--Solución 1

union :: Eq a => [a] -> [a] -> [a]
union [] lista2 = lista2
union (x:xs) lista2 =  x : union xs (filter(/=x) lista2)

--Solución 2 

union2 ::(Integral a)=> [a]->[a]->[a]
union2 [] y = y
union2 (x:xs) y
 |x `elem` y = union2 xs y
 |otherwise= x : union xs y

--Ejercicio Nº 11: Construir un programa no recursivo que realice la suma de números complejos, los cuales se ingresan en sublistas 
--con pares de números donde el primer elemento es la componente real y el segundo la componente imaginaria.

--Los números vienen en tuplas dentro de una lista, de la forma (a,b) donde a es la parte real y b la parte imaginaria
--Hay que hacer la suma de los elementos de la lista, donde los elementos son tuplas, entonces tengo que sumar a con a y b con b
--Ejemplo: sumaComplejos [(1,2)(1,4)(5,3)] ----> [7,9] o (7,9) ??

--Solución 1

sumaComplejos:: [(Int,Int)] -> [Int]
sumaComplejos numeros = [ sum[x | (x,_) <- numeros], sum[y | (_,y) <- numeros]]

--Solución 2

complejos :: (Integral a) => [(a,a)]->[a]
complejos x= [sum (map fst x), sum (map snd x)]

--Ejercicio Nº 12: Dada una lista ordenada y un átomo escribir una función que inserte el átomo en el lugar correspondiente.

--Solución 1

insertarElem :: Ord a => a -> [a] -> [a]
insertarElem y [] = [y]
insertarElem y (x:xs)
    |y <= x = y : (x:xs)
    |otherwise = x: insertarElem y xs 

--Solución 2

inserta:: (Integral a)=> a->[a]->[a]
inserta x (y:ys)
 |x<y= x:(y:ys)
 |otherwise= y: inserta x ys

--Ejercicio Nº 13: Calcular la suma de dos matrices.

sumaMatrices :: (Integral a) => [[a]] -> [[a]] -> [[a]]
sumaMatrices [] [] = []
sumaMatrices (x:xs) (y:ys) = zipWith (+) x y : sumaMatrices xs ys
