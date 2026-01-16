--import Data.List (sort)

-- Comprobar si tres puntos conforman un triángulo mediante la propiedad de la desigualdad triangular
ternasEsTriangulo :: Double -> Double -> Double -> Bool
ternasEsTriangulo a b c =
  let [x, y, z] = sort [a, b, c] 
  in x > 0 && x + y > z


-- Calcular la raíz cuadrada usando el método de Newton–Raphson
raizNewtonRaphson :: Double -> Double
raizNewtonRaphson n = head $ dropWhile (\x -> abs (x*x - n) > epsilon) (iterate next x0)
  where
    x0 = n / 2            -- aproximación inicial
    epsilon = 1e-10       
    next x = (x + n / x) / 2


-- Cálculo del área de un triángulo usando la Fórmula de Herón
areaTrianguloHeron :: Double -> Double -> Double -> Maybe Double
areaTrianguloHeron a b c
    | ternasEsTriangulo a b c = Just (raizNewtonRaphson (s * (s - a) * (s - b) * (s - c)))
    | otherwise               = Nothing
    where
        s = (a + b + c) / 2
