import Data.Char (toUpper)

-- Ejercicio 1
-- Recibe el precio y el porcentaje de descuento de 0 a 100
-- Retorna el precio con su decuento aplicado
discount :: Float -> Float -> Float
discount price dis = price - (price * dis / 100)

-- Recibe el precio y el porcentaje de iva de 0 a 100
-- Retorna el precio con el iva aplicado
aplyIva :: Float -> Float -> Float
aplyIva price iva = price + (price * (iva / 100))

-- Recibe un diccionario de precios y porcentajes, y una función para aplicar descuentos o IVA
-- Devuelve el precio final de toda la cesta con el iva o el descuento aplicado
aplyToCesta :: [(Float, Float)] -> (Float -> Float -> Float) -> Float
aplyToCesta cesta funcion = sum [funcion price dis | (price, dis) <- cesta] -- Aplica una función a cada parte del diccionario y devuelve la suma de los resultados.


-- EJERCICIO 2 
-- Aplica una función a cada elemento de una lista
-- Devuelve una lista con los elementos alterados por la función
aplyFunction :: (a -> b) -> [a] -> [b]
aplyFunction _ [] = [] -- Lista vacía regresa lista vacia
-- Se hace uso de recursión para aplicar la función a todos los elementos
aplyFunction f (x : xs) = f x : aplyFunction f xs 

-- Suma 1 a un número ingresado
-- Regresa el numero + 1
addOne :: (Num a) => a -> a
addOne n = n + 1


-- EJECICIO 3: 
-- Recibe una frase y la divide en una lista
-- Devuelve una lista de tuplas con cada palabra y la cantidad de carácteres que la componen
sentenceLength :: String -> [(String, Int)]
-- por cada palabra se aplica la función length para obtener su longitud y se guarda en la tupla
sentenceLength sentence = [(word, length word) | word <- splitedSentence] 
  where
    splitedSentence = words sentence -- Considerando a word como un elemento de splitedSentence


-- EJERCICIO 4
-- Recibe una lista de tuplas con la materia y el puntaje o calificación
-- Retorna una lista de tuplas con la materia en mayúsculas y con su ponderación o calificación 
-- Ponderaciones: 
--      95 - 100: Excelente
--      85 - 94: Notable)
--      75 - 84: Bueno)
--      70 - 74: Suficiente)  
--      0 - 70: Desempeño insuficiente 
getPonderations :: [(String, Int)] -> [(String, String)]
-- Se aplica recursividad para aplicar la función a cada tupla
getPonderations ponderation = [(map toUpper subject, getNote score) | (subject, score) <- ponderation]

getNote :: Int -> String
getNote score
    | score >= 95 = "Excelente"
    | score >= 85 = "Notable"
    | score >= 75 = "Bueno"
    | score >= 70 = "Suficiente"
    | otherwise = "Desempeno insuficiente"

-- EJERCICIO 5
-- Recibe una lista de valores numéricos
-- Calcula y retorno el móduco del vector
vectorModule :: [Float] -> Float
-- El módulo del vector es la longitud del mismo, la formula consiste en 
-- la raíz cuadrada de la suma de todas las componentes del vector
vectorModule norma = sqrt (sum [x ^ 2 | x <- norma]) 

-- EJERCICIO 6 
-- Función que identifica valores atípicos en una muestra de números
-- Retorna una lista con los valores atípicos
atipicValues :: [Float] -> [Float]
atipicValues values =
    -- fromIntegral transforma cualquier numero en algo más general para evitar excepciones al trabajar con distintos tipos de numeros
    -- El hacer operaciones entre el valor devuelto por length y un float genera una excepción, con fromIntegral se soluciona
    let average = sum values / fromIntegral (length values) -- Calcula la media de la muestra
        -- Calcula la desviación estándar de la muestra
        deviation = sqrt (sum [(x - average) ^ 2 | x <- values] / fromIntegral (length values)) 
    -- Se calcula la desviación típica para cada valor y se filtra los que no cumplen con las condiciones
    in [x | x <- values, abs ((x - average) / deviation) > 3 || abs ((x - average) / deviation) < -3]
