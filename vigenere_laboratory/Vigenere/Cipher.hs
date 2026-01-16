module Vigenere.Cipher
  ( cifrarVigenere
  , descifrarVigenere
  , cifrarLetra
  , descifrarLetra
  ) where

import Vigenere.Utils (letraAIndice, indiceALetra, normalizarTexto)
import Data.Char (isAlpha)
import Data.List (cycle)

-- Cifrar una letra con otra letra de la clave
cifrarLetra :: Char -> Char -> Char
cifrarLetra p k
    | isAlpha p = indiceALetra $ (letraAIndice p + letraAIndice k) `mod` 26
    | otherwise = p

-- Descifrar una letra con otra letra de la clave
descifrarLetra :: Char -> Char -> Char
descifrarLetra c k
    | isAlpha c = indiceALetra $ (letraAIndice c - letraAIndice k + 26) `mod` 26
    | otherwise = c

-- Cifrar un texto completo con clave
cifrarVigenere :: String -> String -> String
cifrarVigenere mensaje clave =
    zipWith cifrarLetra (normalizarTexto mensaje) (cycle (normalizarTexto clave))

-- Descifrar un texto completo con clave
descifrarVigenere :: String -> String -> String
descifrarVigenere mensaje clave =
    zipWith descifrarLetra (normalizarTexto mensaje) (cycle (normalizarTexto clave))