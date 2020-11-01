-- {-# LANGUAGE CPP #-}

-- -- On Windows console pops up alongside with window.
-- -- We need to tell to the linker, that we don't need console.
-- #ifdef mingw32_HOST_OS
-- {-# OPTIONS_GHC -optl-mwindows #-}
-- #endif

import Data.Fixed (mod')
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Text.Printf (printf)

-------------------------------------------------------------------------------

toCartesian :: Point -> Point
toCartesian (ρ, φ) = (ρ * cos φ, ρ * sin φ)

mkCircle :: Float -> Int -> [Point]
mkCircle ρ n = map toCartesian [(ρ, φ) | φ <- [0, dφ .. 2 * pi - dφ]]
  where
    dφ = 2 * pi / fromIntegral n

data Line = Between
    { start
    , end   :: Point
    }

mkLines :: Float -> [Point] -> [Line]
mkLines factor points = map (uncurry connect) paths
  where
    count   = fromIntegral $ length points
    starts  = [0 .. count - 1]
    ends    = map (\start -> (start * factor) `mod'` count) starts
    paths   = zip starts ends
    connect = \start end -> Between
        { -- TODO: Get rid of indexing operator.
          start = points !! floor start
        , end   = points !! floor end
        }

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

class Renderable a where
    render :: a -> Picture

instance Renderable Line where
    render (Between start end) = Line [start, end]

-- https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_RGB
hslToRgb :: Float -> Float -> Float -> Color
hslToRgb h s l = makeColor r g b 1
  where
    c  = (1 - abs (2 * l - 1)) * s
    h' = h / 60
    x  = c * (1 - abs (h' `mod'` 2 - 1))
    m  = l - c / 2

    (r1, g1, b1)
        | 0 <= h' && h' <= 1 = (c, x, 0)
        | 1 <  h' && h' <= 2 = (x, c, 0)
        | 2 <  h' && h' <= 3 = (0, c, x)
        | 3 <  h' && h' <= 4 = (0, x, c)
        | 4 <  h' && h' <= 5 = (x, 0, c)
        | 5 <  h' && h' <= 6 = (c, 0, x)
        | otherwise          = (0, 0, 0)

    (r, g, b) = (r1 + m, g1 + m, b1 + m)

instance Renderable Model where
    render Model { base = n, radius = r, factor = q, delta = d } =
        Color color $ Pictures [ring, info, lines]
      where
        ring       = Circle r
        lines      = foldMap render $ mkLines q $ mkCircle r n

        precision  = round $ logBase 10 $ recip d :: Int
        infoText   = printf "Factor: %.*f" precision q
        info       = Translate (-r) (r * 0.95) $ Scale 0.15 0.15 $ Text infoText

        hue        = 360 * (fromIntegral (ceiling q) - q)
        saturation = 0.95
        lightness  = 0.5
        color      = hslToRgb hue saturation lightness

-------------------------------------------------------------------------------

update :: Model -> Model
update model = model { factor = factor model + delta model }

-------------------------------------------------------------------------------

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
    simulate mode background fps model render (const $ const update)

centerScreen :: (Int, Int) -> IO (Int, Int)
centerScreen (windowWidth, windowHeight) = do
    (screenWidth, screenHeight) <- getScreenSize
    let x = ( screenWidth  - windowWidth  ) `div` 2
        y = ( screenHeight - windowHeight ) `div` 2
    pure (x, y)