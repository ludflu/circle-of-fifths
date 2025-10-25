{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module CircleOfFifths (circleOfFifths) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Piano (compactPianoWithNotes, majorChord, majorChordSecondInversion, noteFromString, Note(..))

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
  let radius = 80.0  -- Position on the main circle (smaller)
      circleSize = 12.0  -- Size of the blue circle (smaller)
      angleRad = angleDeg * pi / 180.0  -- Convert to radians
      pos = p2 (radius * sin angleRad, radius * cos angleRad)
      -- Create the badge: blue circle with white text on top
      -- Using `atop` to ensure text is on top of circle
      badge = (text label # fontSize (local 7) # bold # fc white)
           `atop` (circle circleSize # fc blue # lw 1 # lc white)
  in badge # moveTo pos

-- Create a piano spoke radiating outward from a note position
-- Shows a compact piano with the major chord highlighted
-- Uses second inversion for indices 2, 4, 6, 7, 9, 11 (D, E, F#/Gb, Db, Eb, F)
pianoSpokeAt :: String -> Double -> Int -> Diagram B
pianoSpokeAt label angleDeg index =
  case noteFromString label of
    Nothing -> mempty
    Just (Note name acc _) ->
      let angleRad = angleDeg * pi / 180.0
          -- Position the piano keyboards in an elliptical pattern
          -- Use extra vertical spread for top (0°) and bottom (180°) to avoid overlap
          spokeRadiusX = 130.0  -- Horizontal spread
          spokeRadiusY = if angleDeg == 0 || angleDeg == 180
                         then 140.0  -- Extra vertical spread for C and F♯/G♭
                         else 120.0  -- Normal vertical spread for others
          pos = p2 (spokeRadiusX * sin angleRad, spokeRadiusY * cos angleRad)
          -- Use second inversion for specific indices (G, A, B, Db, Eb, F)
          useSecondInversion = index `elem` [1, 3, 5, 7, 9, 11]
          -- For A (3) and B (5) in second inversion, use octave 2 to keep all notes in range
          -- Otherwise use octave 3 so all chord notes fit in the 2-octave display (octaves 3-4)
          octaveToUse = if useSecondInversion && (index == 3 || index == 5)
                        then 2
                        else 3
          rootNote = Note name acc octaveToUse
          chord = if useSecondInversion
                  then majorChordSecondInversion rootNote
                  else majorChord rootNote
          -- Create the compact piano (no rotation, keep horizontal)
          -- Center it before positioning so it's centered on the spoke position
          piano = compactPianoWithNotes chord # centerXY
      in piano # moveTo pos

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
    innerRadius = 42.0  -- Scaled down

-- The main circle (smaller)
outerCircle :: Diagram B
outerCircle = circle 80
            # lw 2
            # lc black
            # fc white

innerCircle :: Diagram B
innerCircle = circle 42
            # lw 1
            # lc lightgray
            # fcA (lightblue `withOpacity` 0.1)

-- Create the complete circle of fifths diagram
circleOfFifths :: Diagram B
circleOfFifths =
  -- Layer elements with badges on top, then add 10% padding
  (noteBadges `atop` (title `atop` (pianoSpokes `atop` (radiusLines `atop` (innerCircle `atop` outerCircle)))))
  # pad 1.1  -- Add 10% whitespace around edges
  where
    -- Position note badges at 30-degree intervals (12 notes in 360 degrees)
    noteBadges = mconcat
      [ noteBadgeAt label (fromIntegral i * 30)
      | (i, label) <- zip [0..] circleNotes
      ]

    -- Position piano spokes at each note position
    -- Pass the index to determine root position vs second inversion
    pianoSpokes = mconcat
      [ pianoSpokeAt label (fromIntegral i * 30) i
      | (i, label) <- zip [0..] circleNotes
      ]

    -- Add a title above the circle (adjusted for smaller circle)
    title = text "Circle of Fifths Alternating 2nd Inversions"
          # fontSize (local 14)
          # bold
          # fc black
          # translateY 180
