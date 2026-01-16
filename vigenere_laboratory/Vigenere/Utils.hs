module Vigenere.Utils
  ( letraAIndice
  , indiceALetra
  , normalizarTexto
  ) where

import Data.Char (chr, ord, toUpper, isAlpha)

-- Convertir letra a índice 0..25
letraAIndice :: Char -> Int
letraAIndice c = ord (toUpper c) - ord 'A'

-- Convertir índice 0..25 a letra
indiceALetra :: Int -> Char
indiceALetra i = chr (i + ord 'A')

-- Normalizar texto: eliminar no-alfabéticos y pasar a mayúsculas
normalizarTexto :: String -> String
normalizarTexto = map toUpper . filter isAlpha