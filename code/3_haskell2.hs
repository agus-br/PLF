import Data.Char (toUpper)
import System.IO 

-- Ejercicio 1
-- Aplica una función a cada elemento de una lista
-- Devuelve una lista de tuplas con los elementos alterados por la función
aplyFunction :: (Double -> Double) -> [Int] -> [(Int, Double)]
aplyFunction _ [] = [] -- Lista vacía regresa lista vacia
aplyFunction f values = [(x, f (fromIntegral x)) | x <- values]

-- Función que simula la calculadora
scientificCalculator :: IO ()
scientificCalculator = do
    putStrLn "Calculadora Cientifica"
    putStr "Ingrese un valor entero: "
    hFlush stdout
    entrada <- getLine
    let valor = read entrada :: Int
    putStr "Seleccione una funcion (sen, cos, tan, exp, log): "
    hFlush stdout
    func <- getLine
    let resultado = case func of
                        "sen" -> aplyFunction sin [1..valor]
                        "cos" -> aplyFunction cos [1..valor]
                        "tan" -> aplyFunction tan [1..valor]
                        "exp" -> aplyFunction exp [1..valor]
                        "log" -> aplyFunction log [1..valor]
                        _ -> []
    putStrLn "Resultados:"
    -- mapM recibe una función y una lista de tuplas que desempaqueta y usa la función para afectar sus componentes
    mapM_ (\(x, res) -> putStrLn(func ++ "(" ++ show x ++ ") = " ++ show res)) resultado

-- EJERCICIO 2
-- Toma una función que tome como parámetro algo y devuelva un booleano
-- Devuelve una lista del mismo tipo que la función y devuelve una lista
trueElements :: (a -> Bool) -> [a] -> [a]
trueElements _ [] = [] 
trueElements f (x:xs)
    | f x       = x : trueElements f xs 
    | otherwise = trueElements f xs 

-- Función que evalúa si un número es primo o no
-- Si es primo devuelve True, caso contrario devuelve False
esPrimo :: Int -> Bool
esPrimo n
    | n <= 1 = False -- Los números menores o iguales a 1 no son primos
    | otherwise = all (\x -> n `mod` x /= 0) [2..isqrt n]
    where
        -- la función isqrt es una fucnión compuesta, el orden en el que se lee y aplica es de derecha a izquierda
        isqrt = floor . sqrt . fromIntegral

-- EJERCICIO 3
-- Recibe una lista de calificaciones 
-- Retorna una lista de strings con la nota correspondiente a la calificación 
getNotes :: [Int] -> [String]
-- Se evalua cada calificación perteneciente a la lista aplicándole la función
getNotes notes = [getNote score | score <- notes]

-- Toma una calificación y regresa la nota correspondiente
getNote :: Int -> String
getNote score
    | score >= 95 = "Excelente"
    | score >= 85 = "Notable"
    | score >= 75 = "Bueno"
    | score >= 70 = "Suficiente"
    | otherwise = "Desempenio insuficiente"


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


-- EJERCICIO 5
-- Crear un tipo de dato imueble
data Inmueble = Inmueble { anio :: Int
                         , metros :: Int
                         , habitaciones :: Int
                         , garaje :: Bool
                         , zona :: Char
                         } deriving (Show)

-- Recibe un inmueble
-- Devuelve el cálculo del precio del inmueble
precioInmueble :: Inmueble -> Float
precioInmueble inmueble =
    let factorZona = if zona inmueble == 'A' then 1 else 1.5 -- Se define el multiplicador de la zona
        antiguedad = 2024 - anio inmueble  -- Calculamos la antigüedad con el año actual
        precioBase = fromIntegral (metros inmueble * 1000 + habitaciones inmueble * 5000 + if garaje inmueble then 15000 else 0)
        precioConDescuento =  precioBase * (1 - (fromIntegral antiguedad / 100))
    in precioConDescuento * factorZona

-- Recibe una lista de inmuebles y un presupuesto
-- Devuelve una lista de tuplas con los inmuebles filtrados y el precio del mismo si es que el precio es menor o igual al presupuesto
buscarInmueblesPorPresupuesto :: [Inmueble] -> Float -> [(Inmueble, Float)]
buscarInmueblesPorPresupuesto inmuebles presupuesto =
    -- Primero se obtienen los precios de los inmuebles
    let inmueblesConPrecio = map (\inmueble -> (inmueble, precioInmueble inmueble)) inmuebles
        -- Los inmuebles se almacenan si el precio calculado es menos al presupuesto
        inmueblesFiltrados = filter (\(_, precio) -> precio <= presupuesto) inmueblesConPrecio
    in inmueblesFiltrados

main :: IO ()
main = do
    -- Entrada de tipo lista de inmuebles
    let inmuebles = [ Inmueble { anio = 2000, metros = 100, habitaciones = 3, garaje = True, zona = 'A' }
                    , Inmueble { anio = 2012, metros = 60, habitaciones = 2, garaje = True, zona = 'B' }
                    , Inmueble { anio = 1980, metros = 120, habitaciones = 4, garaje = False, zona = 'A' }
                    , Inmueble { anio = 2005, metros = 75, habitaciones = 3, garaje = True, zona = 'B' }
                    , Inmueble { anio = 2015, metros = 90, habitaciones = 2, garaje = False, zona = 'A' }
                    ]
    putStrLn "Ingrese su presupuesto: "
    hFlush stdout
    entrada <- getLine
    let presupuesto = read entrada :: Float

        inmueblesFiltrados = buscarInmueblesPorPresupuesto inmuebles presupuesto
    putStrLn "Inmuebles dentro del presupuesto:"
    mapM_ (\(inmueble, precio) -> putStrLn $ show inmueble ++ ", Precio: " ++ show precio) inmueblesFiltrados
