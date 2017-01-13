module Main where

import           Caligo.Runtime

main :: IO ()
main = execRuntime >> putStrLn "Done."
