{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module CircleOfFifths (circleOfFifths) where

import Diagrams.Prelude
import Diagrams.Backend.SVG

-- Note names for the circle of fifths (clockwise from C at top)
circleNotes :: [String]
circleNotes =
  [ "C"
  , "G"
  , "D"
  , "A"
  , "E"
  , "B"
  , "F♯/G♭"
  , "D♭"
  , "A♭"
  , "E♭"
  , "B♭"
  , "F"
  ]

-- Create a note badge (blue circle with note name inside) at a specific angle
-- Angle 0 is at the top (12 o'clock), increases clockwise
noteBadgeAt :: String -> Double -> Diagram B
noteBadgeAt label angleDeg =
  let radius = 100.0  -- Position on the main circle
      circleSize = 18.0  -- Size of the blue circle
      angleRad = angleDeg * pi / 180.0  -- Convert to radians
      pos = p2 (radius * sin angleRad, radius * cos angleRad)
      -- Create the badge: blue circle with white text on top
      -- Using `atop` to ensure text is on top of circle
      badge = (text label # fontSize (local 10) # bold # fc white)
           `atop` (circle circleSize # fc blue # lw 1 # lc white)
  in badge # moveTo pos

-- Create lines from center to each note position
radiusLines :: Diagram B
radiusLines = mconcat
  [ fromVertices [origin, p2 (innerRadius * sin angleRad, innerRadius * cos angleRad)]
    # lc lightgray
    # lw 0.5
  | i <- [0..11]
  , let angleRad = fromIntegral i * 30 * pi / 180.0
  ]
  where
    innerRadius = 70.0

-- The main circle
outerCircle :: Diagram B
outerCircle = circle 100
            # lw 2
            # lc black
            # fc white

innerCircle :: Diagram B
innerCircle = circle 70
            # lw 1
            # lc lightgray
            # fcA (lightblue `withOpacity` 0.1)

-- Create the complete circle of fifths diagram
circleOfFifths :: Diagram B
circleOfFifths =
  -- Layer badges on top using atop
  noteBadges `atop` (title `atop` (radiusLines `atop` (innerCircle `atop` outerCircle)))
  where
    -- Position note badges at 30-degree intervals (12 notes in 360 degrees)
    noteBadges = mconcat
      [ noteBadgeAt label (fromIntegral i * 30)
      | (i, label) <- zip [0..] circleNotes
      ]

    -- Add a title above the circle
    title = text "Circle of Fifths"
          # fontSize (local 18)
          # bold
          # fc black
          # translateY 130
