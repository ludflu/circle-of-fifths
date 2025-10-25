module Piano (pianoKeyboard) where

import Diagrams.Prelude
import Diagrams.Backend.SVG

-- Key dimensions
whiteKeyWidth :: Double
whiteKeyWidth = 20

whiteKeyHeight :: Double
whiteKeyHeight = 100

blackKeyWidth :: Double
blackKeyWidth = 12

blackKeyHeight :: Double
blackKeyHeight = 60

-- Create a white key
whiteKey :: Diagram B
whiteKey = rect whiteKeyWidth whiteKeyHeight
         # fc white
         # lw 1
         # lc black

-- Create a black key
blackKey :: Diagram B
blackKey = rect blackKeyWidth blackKeyHeight
         # fc black
         # lw 1
         # lc black

-- Position of black keys relative to the start of an octave
-- Black keys appear between: C-D, D-E, F-G, G-A, A-B
-- Positions: 1, 2, 4, 5, 6 (in terms of white key spacing)
blackKeyPositions :: [Double]
blackKeyPositions = [0.7, 1.7, 3.7, 4.7, 5.7]

-- Create a single octave starting at a given x position
octave :: Double -> Diagram B
octave startX =   blackKeys <> whiteKeys
  where
    -- 7 white keys in an octave
    whiteKeys = mconcat
      [ whiteKey # translateX (startX + fromIntegral i * whiteKeyWidth)
      | i <- [0..6]
      ]

    -- 5 black keys positioned above white keys
    blackKeys = mconcat
      [ blackKey
        # translateX (startX + pos * whiteKeyWidth)
        # translateY (whiteKeyHeight / 2 - blackKeyHeight / 2)
      | pos <- blackKeyPositions
      ]

-- Create a piano keyboard with the specified number of octaves
pianoKeyboard :: Int -> Diagram B
pianoKeyboard numOctaves = mconcat
  [ octave (fromIntegral i * 7 * whiteKeyWidth)
  | i <- [0..numOctaves-1]
  ]
