module Vigenere.Analysis
  ( indiceCoincidencia
  , estimarLongitudClave
  ) where

import Vigenere.Utils (normalizarTexto)
import Data.List (group, sort)

-- Índice de coincidencia (IC)
indiceCoincidencia :: String -> Double
indiceCoincidencia texto =
    let letras = normalizarTexto texto
        n = fromIntegral (length letras)
        counts = map (\l -> fromIntegral (length (filter (== l) letras))) ['A'..'Z']
    in sum [ c*(c-1) | c <- counts ] / (n*(n-1))

-- Estimación simplificada de la longitud de la clave
-- Usa IC promedio dividiendo el texto en bloques
estimarLongitudClave :: String -> Int -> Int -> Int
estimarLongitudClave texto minL maxL =
    let ics = [(l, promedioIC l) | l <- [minL..maxL]]
    in fst $ maximumBy (\(_,ic1) (_,ic2) -> compare ic1 ic2) ics
  where
    textoNorm = normalizarTexto texto
    bloques l = transpose [ takeEvery l (drop i textoNorm) | i <- [0..l-1] ]
    promedioIC l = let bs = bloques l
                       icsBloques = map indiceCoincidencia bs
                   in sum icsBloques / fromIntegral (length icsBloques)

-- Función auxiliar para tomar cada n-ésimo elemento
takeEvery :: Int -> [a] -> [a]
takeEvery n xs = case drop (n-1) xs of
                    (y:ys) -> y : takeEvery n ys
                    [] -> []
