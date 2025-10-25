module Piano
  ( pianoKeyboard
  , pianoKeyboardWithNotes
  , compactPianoWithNotes
  , Note(..)
  , Accidental(..)
  -- Note constructors
  , c, d, e, f, g, a, b
  , cs, ds, fs, gs, as
  , df, ef, gf, af, bf
  -- Chord functions
  , majorChord
  , noteFromString
  ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Set (Set)
import qualified Data.Set as Set

-- Simple pitch representation
data Accidental = Flat | Natural | Sharp
  deriving (Eq, Ord, Show)

data Note = Note
  { noteName :: Int      -- 0=C, 1=D, 2=E, 3=F, 4=G, 5=A, 6=B
  , accidental :: Accidental
  , octave :: Int        -- Octave number (4 is middle C octave)
  } deriving (Eq, Ord, Show)

-- Convenience constructors for notes
c, d, e, f, g, a, b :: Int -> Note
c oct = Note 0 Natural oct
d oct = Note 1 Natural oct
e oct = Note 2 Natural oct
f oct = Note 3 Natural oct
g oct = Note 4 Natural oct
a oct = Note 5 Natural oct
b oct = Note 6 Natural oct

cs, ds, fs, gs, as :: Int -> Note
cs oct = Note 0 Sharp oct
ds oct = Note 1 Sharp oct
fs oct = Note 3 Sharp oct
gs oct = Note 4 Sharp oct
as oct = Note 5 Sharp oct

df, ef, gf, af, bf :: Int -> Note
df oct = Note 1 Flat oct
ef oct = Note 2 Flat oct
gf oct = Note 4 Flat oct
af oct = Note 5 Flat oct
bf oct = Note 6 Flat oct

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

-- Create a highlighted white key
highlightedWhiteKey :: Diagram B
highlightedWhiteKey = rect whiteKeyWidth whiteKeyHeight
                    # fc lightblue
                    # lw 2
                    # lc blue

-- Create a black key
blackKey :: Diagram B
blackKey = rect blackKeyWidth blackKeyHeight
         # fc black
         # lw 1
         # lc black

-- Create a highlighted black key
highlightedBlackKey :: Diagram B
highlightedBlackKey = rect blackKeyWidth blackKeyHeight
                    # fc lightcoral
                    # lw 2
                    # lc red

-- Position of black keys relative to the start of an octave
-- Black keys appear between: C-D, D-E, F-G, G-A, A-B
blackKeyPositions :: [Double]
blackKeyPositions = [0.5, 1.5, 3.5, 4.5, 5.5]

-- Key position within an octave (octave, position, isBlackKey)
type KeyPosition = (Int, Double, Bool)

-- Map a note to a key position on the keyboard
-- Returns (octave_relative_to_C4, position_within_octave, is_black_key)
noteToKeyPosition :: Note -> Maybe KeyPosition
noteToKeyPosition (Note name acc oct) =
  let -- Adjust octave relative to C4 (middle C)
      relativeOctave = oct - 4

      -- Get base position for the note name
      basePos = fromIntegral name :: Double

      -- Adjust for accidentals
      result = case acc of
        Sharp -> Just (relativeOctave, basePos + 0.5, True)
        Flat  -> Just (relativeOctave, basePos - 0.5, True)
        Natural -> Just (relativeOctave, basePos, False)

  in result

-- Create a single octave starting at a given x position
-- with optional highlighted keys
octaveWithHighlights :: Double -> Int -> Set KeyPosition -> Diagram B
octaveWithHighlights startX octaveNum highlights = blackKeys <> whiteKeys
  where
    -- Check if a key should be highlighted
    isHighlighted pos isBlack = Set.member (octaveNum, pos, isBlack) highlights

    -- 7 white keys in an octave
    whiteKeys = mconcat
      [ (if isHighlighted (fromIntegral i) False
         then highlightedWhiteKey
         else whiteKey)
        # translateX (startX + fromIntegral i * whiteKeyWidth)
      | i <- [0..6]
      ]

    -- 5 black keys positioned above white keys
    blackKeys = mconcat
      [ (if isHighlighted pos True
         then highlightedBlackKey
         else blackKey)
        # translateX (startX + pos * whiteKeyWidth)
        # translateY (whiteKeyHeight / 2 - blackKeyHeight / 2)
      | pos <- blackKeyPositions
      ]

-- Create a piano keyboard with the specified number of octaves
pianoKeyboard :: Int -> Diagram B
pianoKeyboard numOctaves = pianoKeyboardWithNotes numOctaves []

-- Create a piano keyboard with highlighted notes
-- The keyboard is centered around octave 4 (middle C)
pianoKeyboardWithNotes :: Int -> [Note] -> Diagram B
pianoKeyboardWithNotes numOctaves notes = mconcat
  [ octaveWithHighlights (fromIntegral i * 7 * whiteKeyWidth) (i - offset) highlightSet
  | i <- [0..numOctaves-1]
  ]
  where
    -- Offset to center around middle C (octave 4)
    offset = numOctaves `div` 2

    -- Convert notes to key positions and store in a Set for fast lookup
    highlightSet = Set.fromList $
      [ pos | n <- notes, Just pos <- [noteToKeyPosition n] ]

-- Create a compact 1-octave piano keyboard (scaled down for use in spokes)
compactPianoWithNotes :: [Note] -> Diagram B
compactPianoWithNotes notes =
  pianoKeyboardWithNotes 1 notes # scale 0.4

-- Convert a semitone offset to a note
-- For major chords: root=0, major third=4, perfect fifth=7
addSemitones :: Note -> Int -> Note
addSemitones (Note name acc oct) semitones =
  let -- Convert note to chromatic position (0-11)
      baseChromatic = case name of
        0 -> 0  -- C
        1 -> 2  -- D
        2 -> 4  -- E
        3 -> 5  -- F
        4 -> 7  -- G
        5 -> 9  -- A
        6 -> 11 -- B
        _ -> 0

      accidentalOffset = case acc of
        Flat -> -1
        Natural -> 0
        Sharp -> 1

      chromatic = baseChromatic + accidentalOffset
      newChromatic = (chromatic + semitones) `mod` 12
      newOctave = oct + ((chromatic + semitones) `div` 12)

      -- Convert back to note
      (newName, newAcc) = case newChromatic of
        0 -> (0, Natural)  -- C
        1 -> (0, Sharp)    -- C#
        2 -> (1, Natural)  -- D
        3 -> (2, Flat)     -- Eb
        4 -> (2, Natural)  -- E
        5 -> (3, Natural)  -- F
        6 -> (3, Sharp)    -- F#
        7 -> (4, Natural)  -- G
        8 -> (4, Sharp)    -- G#
        9 -> (5, Natural)  -- A
        10 -> (6, Flat)    -- Bb
        11 -> (6, Natural) -- B
        _ -> (0, Natural)

  in Note newName newAcc newOctave

-- Construct a major chord from a root note
-- Major chord = root, major third (+4 semitones), perfect fifth (+7 semitones)
majorChord :: Note -> [Note]
majorChord root = [root, addSemitones root 4, addSemitones root 7]

-- Parse a note name string to a Note (octave 4)
noteFromString :: String -> Maybe Note
noteFromString s = case s of
  "C" -> Just (c 4)
  "G" -> Just (g 4)
  "D" -> Just (d 4)
  "A" -> Just (a 4)
  "E" -> Just (e 4)
  "B" -> Just (b 4)
  "F♯/G♭" -> Just (fs 4)  -- F# for major chord
  "D♭" -> Just (df 4)
  "A♭" -> Just (af 4)
  "E♭" -> Just (ef 4)
  "B♭" -> Just (bf 4)
  "F" -> Just (f 4)
  _ -> Nothing
