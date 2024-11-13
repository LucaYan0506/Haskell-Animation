import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)
import Data.IORef
import Graphics.Gloss.Interface.IO.Game (playIO)

import System.IO.Unsafe (unsafePerformIO)

type Velocity = (Float, Float)

data Particle = Particle
  { pos :: Point
  , vel :: Velocity
  , mass :: Float
  , _radius :: Float
  , _opacity :: Float
  ,property :: Float
  }

data GameState = GameState
  { particles :: [Particle],
    smoothRadius :: Float

  }

width, height, offset :: Int
width = 600
height = 400
offset = 100

radius ,gravity, collisionDamping, targetDensity, pressureMultiplier :: Float
radius = 1
gravity = 0
collisionDamping = 0.2
targetDensity = 0
pressureMultiplier = 0



randomParticle :: IO Particle
randomParticle = do
  x <- randomRIO (-(fromIntegral width / 2 - radius), fromIntegral width / 2 - radius)
  y <- randomRIO (- (fromIntegral height / 2), fromIntegral height / 2)
  -- m <- randomRIO (1,50)
  -- vx <- randomRIO (-50, 50)  -- Random velocity
  -- vy <- randomRIO (-50, 50)  -- Random velocity
  return $ Particle (x, y) (0, 0) 1 radius 0 (exampleFunction (x,y))

randomParticles :: Int -> IO [Particle]
randomParticles n = mapM (const randomParticle) [1..n]

evenlySpacedParticles :: Float -> [Particle]
evenlySpacedParticles n = 
  let positions = [ (-300 + (i * 25), -200 + (j * 25))| i <- [1..n],j <- [1..n]]
  in map (\(x, y) -> Particle (x, y) (0, 0) 1 radius 0 (exampleFunction (x,y))) positions 

applyGravity :: Float -> Particle -> Particle
applyGravity seconds (Particle (x, y) (vx, vy) m r _opacity property) = Particle (x', y') (vx', vy') m r _opacity property
    where vx' = vx
          vy' = vy + gravity * seconds
          x' = x
          y' = y + vy' * seconds

applyBoardCollision :: Particle -> Particle
applyBoardCollision (Particle (x, y) (vx, vy) m r _opacity property) = Particle (x, y') (vx', vy') m r _opacity property
    where vy'| abs y > abs (fromIntegral height / 2) -radius  = vy * (-1) * collisionDamping
             | otherwise = vy
          vx'| abs x > abs (fromIntegral width / 2) - radius = vx * (-1) * collisionDamping
             | otherwise = vx
        --   x' | abs x > abs (fromIntegral width / 2) - radius = sign x * ((fromIntegral width / 2) - radius)
        --      | otherwise = x
          y' | abs y > abs ((fromIntegral height / 2) - radius) = sign y * ((fromIntegral height / 2) - radius)
             | otherwise = y

sign :: Float -> Float
sign x | x >= 0 = 1
       | otherwise = -1

applyDensity :: GameState -> Particle -> Particle
applyDensity state (Particle (x, y) (vx, vy) m r _opacity property) = Particle (x, y) (vx, vy) m r _opacity' property
  where _opacity' = calculateDensity (x,y) state * 10


calculateDensity :: Point -> GameState -> Float
calculateDensity (x1,y1) (GameState ps _smoothRadius) = sum [ density p _smoothRadius| p <- ps ]
    where density (Particle (x, y) (vx, vy) m r _opacity property) _smoothRadius = m * influence (Particle (x, y) (vx, vy) m r _opacity property) _smoothRadius
          influence (Particle (x, y) (vx, vy) m r _opacity property) = smoothingKernel (distance (Particle (x, y) (vx, vy) m r _opacity property))
          distance (Particle (x, y) (vx, vy) m r _opacity property) = sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1) )

smoothingKernel :: Float -> Float -> Float
smoothingKernel distance _smoothRadius = (value**3) / volume
    where value = max 0 (_smoothRadius * _smoothRadius - distance * distance)
          volume = pi * (_smoothRadius**8) / 4

drawParticle :: Particle -> Picture
drawParticle (Particle (x, y) _ _ r _opacity property) = pictures [
    translate x y $ color white $ circleSolid r,
    translate x y $ color (makeColor 0.0078 0.5882 1 opacity) $ circleSolid (r + 20)]
  where opacity = _opacity








initialState :: IO GameState
initialState = do
  p <- randomParticles 1000
  -- let p = evenlySpacedParticles 15
  return GameState { particles = p, smoothRadius = 250}

render :: GameState -> Picture
render state = Pictures $ map drawParticle (particles state)

update :: Float -> GameState -> GameState
update dt state = state { particles = map (applyDensity state . applyBoardCollision . applyGravity dt) (particles state) }

-- Main function
main :: IO ()
main = do
  initState <- initialState
  play
    (InWindow "Fluid simulation" (width, height) (offset, offset))
    black
    60
    initState
    render
    handleEvent
    update


handleEvent ::  Event -> GameState -> GameState
handleEvent (EventKey (Char 'w') Down _ _) (GameState particles smoothRadius)  = 
  let newSmoothRadius = smoothRadius + 1
  in (GameState particles smoothRadius) { smoothRadius = newSmoothRadius }
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) state = unsafePerformIO $ do
    print (calculatePropertyGradient mousePos state)
    let newParticle = Particle mousePos (0, 0) 0 (getSmoothRadius state) 0 (exampleFunction mousePos)
    return state { particles = newParticle : particles state }
handleEvent (EventKey (MouseButton LeftButton) Up _ _) state = unsafePerformIO $ do
  return state { particles = tail (particles state) }

handleEvent  _ state = state

getSmoothRadius :: GameState -> Float
getSmoothRadius (GameState _ smoothRadius) = smoothRadius

getFirstparticleOpacity :: GameState -> Float
getFirstparticleOpacity (GameState ps _) = getOpacity $ head ps

getOpacity :: Particle -> Float
getOpacity (Particle (x, y) (vx, vy) m r _opacity property) = _opacity

getPos :: Particle -> Point
getPos (Particle (x, y) _ _ _ _ _) = (x,y)



{-
interpolation equation
-}
exampleFunction :: Point -> Float
exampleFunction (x,y) = cos (y - 3 + sin x)


calculateProperty :: Point -> GameState -> Float
calculateProperty (x1,y1) (GameState ps _smoothRadius) = sum [ prop p _smoothRadius / calculateDensity (getPos p) (GameState ps _smoothRadius)| p <- ps ]
    where prop (Particle (x, y) (vx, vy) m r _opacity property) _smoothRadius = property * m * influence (Particle (x, y) (vx, vy) m r _opacity property) _smoothRadius
          influence (Particle (x, y) (vx, vy) m r _opacity property) = smoothingKernel (distance (Particle (x, y) (vx, vy) m r _opacity property))
          distance (Particle (x, y) (vx, vy) m r _opacity property) = sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1) )

calculatePropertyGradient :: Point -> GameState -> Point
calculatePropertyGradient (x,y) state = (deltaX / 0.001,deltaY / 0.001)
  where 
    deltaX = calculateProperty(x + x * 0.001,y) state - calculateProperty (x,y) state
    deltaY = calculateProperty(x,y + y * 0.001) state - calculateProperty (x,y) state


convertDensityToPressure :: Float -> Float
convertDensityToPressure density = pressure
  where pressure = densityError * pressureMultiplier
        densityError = density - targetDensity