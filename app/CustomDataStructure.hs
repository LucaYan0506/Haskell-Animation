module CustomDataStructure where

import qualified Data.Vector as V
import Graphics.Gloss ( Point, Color )

type Vector2D a = V.Vector [a]


type Velocity = (Float, Float)
type ID = Float

data GameState = GameState
  { particles :: Vector2D Particle,
    randomPoints :: [Point],
    pause :: Bool,
    generateParticles :: Bool,
    mousePos :: Point,
    showGrid :: Bool,
    gravity :: Float,
    isPullInteraction :: Bool,
    isPushInteraction :: Bool,
    addColor :: Bool
  }

data Particle = Particle
  { pos :: Point,
    predPos :: Point,
    vel :: Velocity,
    mass :: Float,
    radius :: Float,
    opacity :: Float,
    density :: Float,
    id :: ID,
    particleColor::Color
  }

instance Show Particle where
    show (Particle (x,y) _ _ _ _ _ _ _ _) = show (x,y)


--functions for 2D Vector
vector2DToList :: Vector2D a -> [a]
vector2DToList vec2D = helper (V.toList vec2D)
    where helper :: [[a]] -> [a]
          helper [] = []
          helper [x] = x
          helper (x:xs) = x ++ helper xs

list2DToVector2D :: [[a]] -> Vector2D a
list2DToVector2D = V.fromList

map2D :: (a -> b) -> Vector2D a -> Vector2D b
map2D f = V.map (map f)

length2D :: Vector2D a -> Int
length2D = V.sum . V.map length


addElementToRow :: Int -> a -> Vector2D a -> Vector2D a
addElementToRow i newElem grid
    | i >= V.length grid || i < 0 = V.update grid (V.singleton (0, newRow 0))
    | otherwise = V.update grid (V.singleton (i, newRow i))
    where
        newRow localI = newElem : grid V.! localI

getNList :: Int -> Vector2D a -> [a]
getNList i vec2D 
    | i < 0 || i >= V.length vec2D = []
    | otherwise = vec2D V.! i

emptyVector2DWithNRow :: Int ->  Vector2D a
emptyVector2DWithNRow n = V.replicate n []