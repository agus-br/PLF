-- Nombre del módulo
module FizzBuzz where 

    -- Recibe un número, lo evalúa para conocer sus divisores y retorna una cadena de carácteres si hay un resultado directo o retorna una función 
    fizzbuzz :: Int -> String
    fizzbuzz n 
     | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz" -- Si el número es divisible por 3 y 5 entonces retornamos "FizzBuzz"
     | n `mod` 3 == 0 = "Fizz" -- Si el número es divisible por 3 entonces retornamos "Fizz"
     | n `mod` 5 == 0 = "Buzz" -- Si el número es divisible por 5 entonces retornamos "Buzz"
     | 1 <= n && n < 20 = lessThan20 (n) -- Si el número está entre 1 y 19 entonces retornamos a la función lessThan20
     | n < 100 = tens(n `div` 10) ++ " " ++ lessThan20(n `mod` 10) --  Si el número no es divisible por 3 o por 5 y es mayor que 20 entonces retornamos la función tens concatenada con lessThan20 para encontrar su nombre

    -- Recibe un número entre 1 y 19 incluyéndolos que representa un índice y retorna su nombre en inglés en formato cadena de carácteres 
    lessThan20 :: Int -> String
    lessThan20 n 
        | n > 0 && n < 20 =
        let answers = words("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")
        in answers !! (n-1) -- Retorna el índice correspondiente con el nombre

    -- Recibe un entero que representa un índice, devuelve una cadena de carácteres
    tens :: Int -> String 
    tens n
        | n >= 2 && n<=9 =
            answers !! (n - 2) -- Se restan dos al índice por el hecho de que solamente recibirá un número entre 2 y 9 y el caso del 10 y 100 ya se contemplan pues ambos son divisibles por 5
            where 
                answers = words ("twenty thirty fourty fifty sixty seventy eighty ninety") -- Se modifica el arreglo de respuestas para que solo contenga los valores de números divisibles por 10 y mayores a 19
