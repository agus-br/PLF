module Ejercicios where
    -- EJERCICIO 1
    sumarLista :: [Int] -> Int
    sumarLista lista = sum [x | x <- lista]

    -- EJERCICIO 2
    calcularFactorial :: Int -> Int
    calcularFactorial 0 = 1
    calcularFactorial numero = numero * calcularFactorial (numero - 1)

    -- EJERCICIO 3
    numerosPares :: Int -> [Int]
    numerosPares numero = [2..numero]

     -- EJERCICIO 4
    longitudCadena :: String -> Int
    longitudCadena = length

    -- EJERCICIO 5
    reversoLista :: String -> String
    reversoLista = reverse

    -- EJERCICIO 6
    duplicarElementos :: [Int] -> [Int]
    duplicarElementos [] = []
    duplicarElementos lista = concatMap(replicate(2)) lista

    -- EJERCICIO 7 
    filtrarPares :: [Int] -> [Int]
    filtrarPares [] = []
    filtrarPares (x:xs)
        | x`mod` 2 == 0 = filtrarPares xs 
        | otherwise = x:filtrarPares xs 

    -- EJERCICIO 8
    fibonacci :: Int -> Int
    fibonacci 0 = 0
    fibonacci 1 = 1
    fibonacci numero = fibonacci(numero - 1) + fibonacci(numero - 2) 

    -- EJERCICIO 9
    divisores :: Int -> [Int]
    divisores 0 = []
    divisores numero = [n | n <- [1..numero], numero `mod` n == 0]

    -- EJERCICIO 10
    esPalindromo :: String -> Bool
    esPalindromo texto = texto == reverse texto
