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
  }

data GameState = GameState
  { particles :: [Particle],
    smoothRadius :: Float

  }

width, height, offset :: Int
width = 600
height = 400
offset = 100

radius ,gravity, collisionDamping :: Float
radius = 10
gravity = -98.1
collisionDamping = 0.5


randomParticle :: IO Particle
randomParticle = do
  x <- randomRIO (-(fromIntegral width / 2 - radius), fromIntegral width / 2 - radius)
  y <- randomRIO (- (fromIntegral height / 2), fromIntegral height / 2)
  -- m <- randomRIO (1,50)
  -- vx <- randomRIO (-50, 50)  -- Random velocity
  -- vy <- randomRIO (-50, 50)  -- Random velocity
  return $ Particle (x, y) (0, -1) 1 radius 0

randomParticles :: Int -> IO [Particle]
randomParticles n = mapM (const randomParticle) [1..n]

evenlySpacedParticles :: Float -> [Particle]
evenlySpacedParticles n = 
  let positions = [ (-300 + (i * 25), -200 + (j * 25))| i <- [1..n],j <- [1..n]]
  in map (\(x, y) -> Particle (x, y) (0, 0) 1 radius 0) positions 

applyGravity :: Float -> Particle -> Particle
applyGravity seconds (Particle (x, y) (vx, vy) m r _opacity) = Particle (x', y') (vx', vy') m r _opacity
    where vx' = vx
          vy' = vy + gravity * seconds
          x' = x
          y' = y + vy' * seconds

applyBoardCollision :: Particle -> Particle
applyBoardCollision (Particle (x, y) (vx, vy) m r _opacity) = Particle (x, y') (vx', vy') m r _opacity
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
applyDensity state (Particle (x, y) (vx, vy) m r _opacity) = Particle (x, y) (vx, vy) m r _opacity'
  where _opacity' = calculateDensity (x,y) state * 10


calculateDensity :: Point -> GameState -> Float
calculateDensity (x1,y1) (GameState ps _smoothRadius) = sum [ density p _smoothRadius| p <- ps ]
    where density (Particle (x, y) (vx, vy) m r _opacity) _smoothRadius = m * influence (Particle (x, y) (vx, vy) m r _opacity) _smoothRadius
          influence (Particle (x, y) (vx, vy) m r _opacity) = smoothingKernel (distance (Particle (x, y) (vx, vy) m r _opacity))
          distance (Particle (x, y) (vx, vy) m r _opacity) = sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1) )

smoothingKernel :: Float -> Float -> Float
smoothingKernel distance _smoothRadius = (value**3) / volume
    where value = max 0 (_smoothRadius * _smoothRadius - distance * distance)
          volume = pi * (_smoothRadius**8) / 4

drawParticle :: Particle -> Picture
drawParticle (Particle (x, y) _ _ r _opacity) = pictures [
    translate x y $ color (withAlpha 0.8 blue) $ circleSolid r,
    translate x y $ color (withAlpha opacity blue) $ circleSolid (r + 20)]
  where opacity = _opacity








initialState :: IO GameState
initialState = do
  p <- randomParticles 100
  -- let p = evenlySpacedParticles 15
  return GameState { particles = p, smoothRadius = 30}

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
    white
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
    print (calculateDensity mousePos state)
    print(getFirstparticleOpacity state)
    let newParticle = Particle mousePos (0, 0) 0 (getSmoothRadius state) 0
    return state { particles = newParticle : particles state }
handleEvent (EventKey (MouseButton LeftButton) Up _ _) state = unsafePerformIO $ do
  return state { particles = tail (particles state) }

handleEvent  _ state = state

getSmoothRadius :: GameState -> Float
getSmoothRadius (GameState _ smoothRadius) = smoothRadius

getFirstparticleOpacity :: GameState -> Float
getFirstparticleOpacity (GameState ps _) = getOpacity $ head ps

getOpacity :: Particle -> Float
getOpacity (Particle (x, y) (vx, vy) m r _opacity) = _opacity