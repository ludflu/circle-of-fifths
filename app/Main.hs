module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import CircleOfFifths

main :: IO ()
main = do
  putStrLn "Generating Circle of Fifths diagram..."
  putStrLn "C is at 12 o'clock, F#/Gb is at 6 o'clock"
  mainWith (circleOfFifths :: Diagram B)
