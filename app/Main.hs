module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Piano (pianoKeyboard)

main :: IO ()
main = do
  putStrLn "Generating piano keyboard with 3 octaves..."
  mainWith (pianoKeyboard 3 :: Diagram B)
  putStrLn "Done! Use: ./circle-of-fourths -o keyboard.svg -w 600"
