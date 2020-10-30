module Main where

import Data.Fixed                           (mod')
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort         (ViewPort)
import Graphics.Gloss.Interface.Environment (getScreenSize)

--------------------------------------------------------------------------------

toCartesian :: Point -> Point
toCartesian (ρ, φ) = (ρ * cos φ, ρ * sin φ)

createCircle :: Float -> Int -> [Point]
createCircle ρ n = map toCartesian [(ρ, φ) | φ <- [0, dφ .. 2 * pi - dφ]]
  where
    dφ = 2 * pi / fromIntegral n

data Line = Between
    { start
    , end   :: Point
    }

createLines :: Float -> [Point] -> [Line]
createLines factor points = map connect paths
  where
    count   = fromIntegral $ length points
    starts  = [0 .. count - 1]
    ends    = map (\start -> (start * factor) `mod'` count) starts
    paths   = zip starts ends
    connect = \(start, end) -> Between
        { start = points !! floor start
        , end   = points !! floor end
        }

--------------------------------------------------------------------------------

data Model = Model
    { base   :: Int
    , radius
    , factor
    , delta  :: Float
    }

model :: Model
model = Model
    { base   = 180
    , radius = 350
    , factor = 2
    , delta  = 0.01
    }

--------------------------------------------------------------------------------

class Renderable a where
    render :: a -> Picture

instance Renderable Line where
    render (Between start end) = Line [start, end]

instance Renderable Model where
    render (Model base radius factor _delta) =
        Color black $ Pictures [ring, lines]
      where
        ring  = Circle radius
        lines = Pictures
            . map render
            . createLines factor
            $ createCircle radius base

--------------------------------------------------------------------------------

update :: ViewPort -> Float -> Model -> Model
update _ _ model = model { factor = factor model + delta model }

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let padding = 10
        side    = round (radius model) * 2 + padding
        size    = (side, side)
    position <- centerScreen size
    let title      = "Multiplication circle"
        background = white
        mode       = InWindow title size position
        fps        = 30
    simulate mode background fps model render update

centerScreen :: (Int, Int) -> IO (Int, Int)
centerScreen (windowWidth, windowHeight) = do
    (screenWidth, screenHeight) <- getScreenSize
    let x = ( screenWidth  - windowWidth  ) `div` 2
        y = ( screenHeight - windowHeight ) `div` 2
    pure (x, y)