module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Piano

main :: IO ()
main = do
  putStrLn "Generating Piano keyboard with 3 octaves..."
  putStrLn "Highlighting notes from the circle of fourths: C, F, Bb, Eb, Ab, Db, Gb..."

  -- Circle of fourths starting from C (all in octave 4, which is middle C)
  let circleOfFourths = [c 4, f 4, bf 4, ef 4, af 4, df 4, gf 4]

  mainWith (pianoKeyboardWithNotes 3 circleOfFourths :: Diagram B)
