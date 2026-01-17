-- Definimo el modulo conjunto ocn la siguiente funcionalidad
module Conjunto
  ( Conjunto
    , vacio                   -- Conjunto vacío
    , unitario                -- Conjunto unidad
    , desdeLista              -- Construir conjunto a partir de una lista
    , aLista                  -- Transformar conjunto a lista
    , pertenece               -- Comprobar si un elemento es parte del conjunto
    , insertar                -- Insertar un elemento en el conjunto
    , eliminar                -- Eliminar un elemento del conjunto
    , union                   -- Crear un nuevo conjunto como la unión de otros dos
    , interseccion            -- Crear un nuevo conjunto como la intersección de otros dos
    , diferencia              -- Diferencia de conjuntos
    , esSubconjunto           -- Comprueba si un conjunto es un subconjunto de otro
    , esIgual                 -- Comprueba si dos conjuntos son idénticos
    , cardinal                -- Devuelve la cardinalidad de un conjunto
    , conjuntoPotencia        -- Devuelve el conjunto de todos los subconjuntos
    , productoCartesiano      -- Producto cartesiano entre dos conjuntos
    , mapC                    -- Aplica una función a todos los elementos del conjunto
    , filterC                 -- Filtra los elementos según un predicado
    , foldC                   -- Hace la operación de plegado sobre un conjunto
  ) where

-- Declaramos la estructura interna de un conjunto como una lista de elementos de a
newtype Conjunto a = Conjunto [a]

-------------------
-- Funcionalidad --
-------------------

vacio :: Conjunto a
vacio = Conjunto []

unitario :: a -> Conjunto a
unitario x = Conjunto [x]

desdeLista :: (Eq a) => [a] -> Conjunto a
desdeLista = foldr insertar vacio

aLista :: Conjunto a -> [a]
aLista (Conjunto xs) = xs

pertenece :: (Eq a) => a -> Conjunto a -> Bool
pertenece x (Conjunto xs) = x `elem` xs

insertar :: (Eq a) => a -> Conjunto a -> Conjunto a
insertar x (Conjunto xs)
  | x `elem` xs = Conjunto xs
  | otherwise   = Conjunto (x:xs)

eliminar :: (Eq a) => a -> Conjunto a -> Conjunto a
eliminar x (Conjunto xs) = Conjunto (filter (/= x) xs)

union :: (Eq a) => Conjunto a -> Conjunto a -> Conjunto a
union (Conjunto xs) ys = foldr insertar ys xs

interseccion :: (Eq a) => Conjunto a -> Conjunto a -> Conjunto a
interseccion (Conjunto xs) ys =
  Conjunto [x | x <- xs, pertenece x ys]

diferencia :: (Eq a) => Conjunto a -> Conjunto a -> Conjunto a
diferencia (Conjunto xs) ys =
  Conjunto [x | x <- xs, not (pertenece x ys)]

esSubconjunto :: (Eq a) => Conjunto a -> Conjunto a -> Bool
esSubconjunto (Conjunto xs) ys = all (`pertenece` ys) xs

esIgual :: (Eq a) => Conjunto a -> Conjunto a -> Bool
esIgual = (==)

cardinal :: Conjunto a -> Int
cardinal (Conjunto xs) = length xs

conjuntoPotencia :: (Eq a) => Conjunto a -> Conjunto (Conjunto a)
conjuntoPotencia (Conjunto []) = unitario vacio
conjuntoPotencia (Conjunto (x:xs)) =
  let ps = conjuntoPotencia (Conjunto xs)
  in union ps (mapC (insertar x) ps)

productoCartesiano :: Conjunto a -> Conjunto b -> Conjunto (a,b)
productoCartesiano (Conjunto xs) (Conjunto ys) =
  Conjunto [(x,y) | x <- xs, y <- ys]

mapC :: (Eq b) => (a -> b) -> Conjunto a -> Conjunto b
mapC f (Conjunto xs) = desdeLista (map f xs)

filterC :: (a -> Bool) -> Conjunto a -> Conjunto a
filterC p (Conjunto xs) = Conjunto (filter p xs)

foldC :: (a -> b -> b) -> b -> Conjunto a -> b
foldC f z (Conjunto xs) = foldr f z xs

-- Añadimos además la siguiente definiciones para poder comparar y mostrar el tipo conjunto

-- Igualdad
instance (Eq a) => Eq (Conjunto a) where
  Conjunto xs == Conjunto ys =
    all (`elem` ys) xs && all (`elem` xs) ys

-- Salida estándar
instance (Show a) => Show (Conjunto a) where
  show (Conjunto xs) = "{" ++ mostrar xs ++ "}"
    where
      mostrar []     = ""
      mostrar [x]    = show x
      mostrar (x:xs) = show x ++ ", " ++ mostrar xs