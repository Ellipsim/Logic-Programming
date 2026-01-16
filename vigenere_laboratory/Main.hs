module Main where

import Vigenere.Cipher
import Vigenere.Analysis
import System.IO

main :: IO ()
main = do
    putStrLn "=== Laboratorio Vigenère ==="
    putStrLn "Ingrese mensaje a cifrar:"
    mensaje <- getLine
    putStrLn "Ingrese clave:"
    clave <- getLine
    let cifrado = cifrarVigenere mensaje clave
    putStrLn $ "Mensaje cifrado: " ++ cifrado
    let descifrado = descifrarVigenere cifrado clave
    putStrLn $ "Mensaje descifrado: " ++ descifrado
