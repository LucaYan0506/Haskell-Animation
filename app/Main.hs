import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)

import System.IO.Unsafe (unsafePerformIO)

type Position = (Float, Float)
type Velocity = (Float, Float)
type Mass = Float

data Particle = Particle
  { pos :: Position
  , vel :: Velocity --dm/s
  , mass :: Mass    --kg
  , timeSinceBounce :: Float -- Time accumulator (in seconds)
  }

initialParticle :: IO Particle
initialParticle = do
  x <- randomRIO (-(fromIntegral width / 2 - radius), fromIntegral width / 2 - radius)
  -- m <- randomRIO (1,50)
  --y <- randomRIO (-fromIntegral height / 2, fromIntegral height / 2)
  -- vx <- randomRIO (-50, 50)  -- Random velocity
  -- vy <- randomRIO (-50, 50)  -- Random velocity
  return $ Particle (x, 180) (0, 0) 50 0

numParticles :: Int
numParticles = 2

initialParticles :: IO [Particle]
initialParticles = mapM (const initialParticle) [1..numParticles]
-- initialParticles = do
  -- return [Particle (30, 190) (0,0) 10, Particle (30, 160) (0,0) 10]

width, height, offset :: Int
width = 600     --60m, every 10 pixel = 1m
height = 400    --40m
offset = 100


radius, gravity, airDensity :: Float
gravity = (-9.81) * 10 --dm/s
radius = 10 --dm
airDensity = 1.225 / 1000 --kg/dm^3

updateParticles :: Float -> [Particle] -> [Particle]
updateParticles seconds = map (applyGravityAirResistance seconds)
-- updateParticles seconds = applyCollision seconds . map (applyGravityAirResistance seconds)

applyGravityAirResistance :: Float -> Particle -> Particle
applyGravityAirResistance seconds (Particle (x, y) (vx, vy) m customSeconds) = Particle (x', y') (vx', vy') m customSeconds
  where
    x' = x + vx * seconds
    y' = y + 0.5 * newAcceleration * (seconds * seconds)
    
    -- Velocity after collision or update
    vx' = if abs x' > fromIntegral width / 2 - radius then -vx else vx
    vy' = if abs y > fromIntegral height / 2 - radius then -(vy * e) else vy + newAcceleration * seconds
    
    -- Physics constants and equations
    newAcceleration = gravity + f_airResistance / m
    f_airResistance = 0.5 * airDensity * (vy * vy) * crossSectionalArea * dragCoefficient
    crossSectionalArea = pi * radius * radius
    dragCoefficient = 0.47  -- Example for a sphere
    
    e = 0.7  -- Coefficient of restitution for sand


applyCollision :: Float -> [Particle] -> [Particle]
applyCollision seconds (x:xs)= undefined

drawParticles :: [Particle] -> Picture
drawParticles particles = pictures $ map drawParticle particles

drawParticle :: Particle -> Picture
drawParticle (Particle (x, y) _ _ _) = translate x y $ color (dark red) $ circleSolid radius

-- Main function
main :: IO ()
main = do
  particles <- initialParticles
  play
    (InWindow "Sand simulation" (width, height) (offset, offset))
    white
    60
    particles
    drawParticles
    handleEvent
    updateParticles


-- Event handler to print particle positions and velocities when 'p' is pressed
handleEvent :: Event -> [Particle] -> [Particle]
  -- Print positions and velocities of all particles when 'p' is pressed
handleEvent (EventKey (Char 'p') Down _ _) particles =  unsafePerformIO $ do
    printParticles particles
    return particles  
handleEvent _ particles = particles

-- Function to print particle information (positions and velocities)
printParticles :: [Particle] -> IO ()
printParticles = mapM_ (\(Particle (x, y) (vx, vy) _ _) ->
  print ("Position: (" ++ show x ++ ", " ++ show y ++ "), Velocity: (" ++ show vx ++ ", " ++ show vy ++ ")"))