module Vigenere.Analysis
  ( indiceCoincidencia
  , estimarLongitudClave
  , estimarClave
  ) where

import Vigenere.Utils (normalizarTexto, letraAIndice, indiceALetra)
import Vigenere.Cipher
import Data.Ord (comparing)
import Data.List (group, sort, minimumBy, maximumBy, sortOn, transpose)

-- Índice de coincidencia (IC)
indiceCoincidencia :: String -> Double
indiceCoincidencia texto =
    let letras = normalizarTexto texto
        n = fromIntegral (length letras)
        counts = map (\l -> fromIntegral (length (filter (== l) letras))) ['A'..'Z']
    in sum [ c*(c-1) | c <- counts ] / (n*(n-1))

-- Estimación simplificada de la longitud de la clave
-- Usa IC promedio dividiendo el texto en bloques
estimarLongitudClave :: String -> Int -> Int -> IO ()
estimarLongitudClave texto minL maxL = do
    let textoNorm = normalizarTexto texto
        bloques l = [ takeEvery l (drop i textoNorm) | i <- [0..l-1] ]
        promedioIC l = 
          let icsBloques = map indiceCoincidencia (bloques l)
          in sum icsBloques / fromIntegral (length icsBloques)
        ics = [(l, promedioIC l) | l <- [minL..maxL]]
        icEsp = 0.065
        sorted = sortOn (\(_,ic) -> abs (ic - icEsp)) ics
    putStrLn "Estimaciones de longitud de clave (más probable primero):"
    mapM_ (\(l,ic) -> putStrLn $ "Longitud: " ++ show l ++ ", IC: " ++ show ic) (take 5 sorted)

  
-- Función auxiliar para tomar cada n-ésimo elemento
takeEvery :: Int -> [a] -> [a]
takeEvery n xs = case xs of
    [] -> []
    (y:ys) -> y : takeEvery n (drop (n-1) ys)

-- Frecuencias español
frecuenciaEsp :: [Double]
frecuenciaEsp = 
  [0.125,0.014,0.046,0.058,0.136,0.006,0.010,0.007,0.062,0.005,
   0.004,0.049,0.031,0.067,0.086,0.025,0.008,0.068,0.079,0.046,
   0.033,0.004,0.002,0.002,0.010,0.004]  -- A-Z

-- Contar frecuencias del texto
frecuencia :: String -> [Double]
frecuencia txt =
    let letras = normalizarTexto txt
        n = fromIntegral (length letras)
        counts = map (\c -> fromIntegral (length (filter (== c) letras)) / n) ['A'..'Z']
    in counts

-- César por columnas
desplazamientoColumna :: String -> Int
desplazamientoColumna columna =
    let frecCol = frecuencia columna
        correlacion k = sum [ frecCol !! ((i + k) `mod` 26) * frecuenciaEsp !! i | i <- [0..25] ]
    in snd $ maximum [(correlacion k, k) | k <- [0..25]]

-- Estimar clave
estimarClave :: String -> Int -> String
estimarClave texto lenClave =
    let textoNorm = normalizarTexto texto
        columnas = [takeEvery lenClave (drop i textoNorm) | i <- [0..lenClave-1]]
        desplazamientos = map desplazamientoColumna columnas
    in map indiceALetra desplazamientos