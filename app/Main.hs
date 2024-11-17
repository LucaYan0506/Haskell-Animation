import Prelude hiding (id)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)
import Data.IORef
import Graphics.Gloss.Interface.IO.Game (playIO)

import System.IO.Unsafe (unsafePerformIO)

type Velocity = (Float, Float)
type ID = Float

data Particle = Particle
  { pos :: Point,
    vel :: Velocity,
    mass :: Float,
    radius :: Float,
    opacity :: Float,
    density :: Float,
    id :: ID,
    particleColor::Color
  }

data Density = Density{
  densityPos:: Point,
  value :: Float
}

data GameState = GameState
  { particles :: [Particle],
    densities :: [Density],
    randomPoints :: [Point],
    pause :: Bool,
    generateParticles :: Bool,
    mousePos :: Point
  }

width, height, offset :: Int
width = 300
height = 200
offset = 300

smoothRadius, squareSize, defaultRadius ,gravity, collisionDamping, targetDensity, pressureMultiplier :: Float
defaultRadius = 5
smoothRadius = 25
squareSize = 5
-- gravity = 0 
gravity = -981 
collisionDamping = 0.5
targetDensity = 2.651827e-3
pressureMultiplier = 10000000




randomParticle :: ID -> IO Particle
randomParticle id = do
  x <- randomRIO (-(fromIntegral width / 2 - smoothRadius ), fromIntegral width / 2 - smoothRadius )
  y <- randomRIO (- (fromIntegral height / 2 - smoothRadius), fromIntegral height / 2 - smoothRadius)
  -- m <- randomRIO (1,50)
  -- vx <- randomRIO (-50, 50)  -- Random velocity
  -- vy <- randomRIO (-50, 50)  -- Random velocity
  return $ Particle (x, y) (0, 0) 1 defaultRadius 0 0 id white

randomParticles :: Float -> IO [Particle]
randomParticles n = mapM randomParticle [1..n]

evenlySpacedParticles :: Float -> [Particle]
evenlySpacedParticles n = 
  let positions_index = [[ i , j , i * n + j]| i <- [0..n],j <- [0..n], (dirx,diry) <- []]
  in map (\[x, y, id] -> Particle (x, y) (0, 0) 1 defaultRadius 0 0 id (makeColor 0.0078 0.5882 1 1)) positions_index

updatePosition :: Float -> Particle -> Particle
updatePosition dt p = p { pos = (x + vx * dt, y + vy * dt) }
  where
    (x, y) = pos p
    (vx, vy) = vel p

applyGravity :: Float -> Particle -> Particle
applyGravity seconds p = p {vel = (vx',vy')}
    where (vx,vy) = vel p
          vx' = vx
          vy' = vy + gravity * seconds
          -- x' = x + vx * seconds
          -- y' = y + vy' * seconds

applyBoardCollision :: Particle -> Particle
applyBoardCollision p = p {pos = (x',y'), vel = (vx',vy')}
    where (vx,vy) = vel p
          (x,y) = pos p
          vy'| abs y >= (fromIntegral height / 2) - defaultRadius  = vy * (-1) * collisionDamping
             | otherwise = vy
          vx'| abs x >= (fromIntegral width / 2) - defaultRadius = vx * (-1) * collisionDamping
             | otherwise = vx
          x' | abs x >= (fromIntegral width / 2) - defaultRadius = sign x * ((fromIntegral width / 2) - defaultRadius)
             | otherwise = x
          y' | abs y >= ((fromIntegral height / 2) - defaultRadius) = sign y * ((fromIntegral height / 2) - defaultRadius)
             | otherwise = y
          sign :: Float -> Float
          sign x | x >= 0 = 1
                | otherwise = -1


calculateDensity :: Particle -> GameState -> Float
calculateDensity p state = sum [ _density otherP | otherP <- particles state, distance (pos otherP) < smoothRadius]
    where (x1,y1) = pos p
          _density localP = mass localP * influence localP
          influence localP = smoothingKernel (distance $ pos localP)
          distance (x,y) = sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1))

smoothingKernel :: Float -> Float
smoothingKernel distance 
  | distance >= smoothRadius = 0
  | otherwise = (smoothRadius - distance) * (smoothRadius - distance) / volume
    where volume = (pi * (smoothRadius**4)) / 6
-- smoothingKernel distance _smoothRadius = (value**3) / volume
--     where value = max 0 (_smoothRadius * _smoothRadius - distance * distance)
--           volume = pi * (_smoothRadius**8) / 4

smoothingKernelDerivative :: Float -> Float
smoothingKernelDerivative distance 
  | distance >= smoothRadius = 0
  | otherwise = (distance - smoothRadius) * constant
    where constant = 12 / ((smoothRadius**4) * pi)
-- smoothingKernelDerivative distance _smoothRadius 
--   | distance >= _smoothRadius = 0
--   | otherwise = constant * distance * f * f
--     where constant = (-24) / (pi * (_smoothRadius**8))
--           f = _smoothRadius * _smoothRadius - distance * distance

updateDensity :: GameState -> Particle -> Particle
updateDensity state p = p {density = density'}
  where density' = calculateDensity p state

applyPressure :: Float -> GameState -> Particle -> Particle
applyPressure dt state p = p {vel = newv}
  where newv = customMult pressureAcceleration dt
          -- | otherwise = customSum [vel p,(sign ax * min 200 (abs (ax * dt)),sign ay * min 200 (abs (ay * dt)))]
        pressureAcceleration = customMult (calculatePressureForce p state) (1 / density p)
        -- pressureAcceleration = customMult (calculatePressureForce p state) (1 / density p)
        sign 0 = 1
        sign n = abs n / n
        -- pressureAcceleration = customMult (calculatePressureForce p state) (1 / density p)

convertDensityToPressure :: Float -> Float
convertDensityToPressure _density = pressure
  where pressure = densityError * pressureMultiplier
        densityError = _density - targetDensity

calculatePressureForce :: Particle -> GameState -> Point
calculatePressureForce p state = pressureForce
  where (x1,y1) = pos p
        pressureForce = customSum [ dir (pos otherP) (id otherP) `customMult` (sharedPressure otherP * slope otherP * mass otherP / getDensity otherP) | otherP <- particles state,
                                                                   id otherP /= id p && distance (pos otherP) < smoothRadius]
        dir :: Point -> Float -> Point
        dir (x,y) _id
          -- | distance (x,y) < 1 = (1,1)
          | distance (x,y) == 0 = randomPoints state !! (fromEnum _id `mod` 4)
          -- | otherwise = (x - x1, y - y1) 
          | otherwise = customMult (x - x1, y - y1) ( 1/ distance (x,y))
        slope localP 
          | distance (pos localP) == 0 = smoothingKernelDerivative 1
          | otherwise = smoothingKernelDerivative (distance $ pos localP)
        distance (x,y) = sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1))
        getDensity localP
          | density localP < 0.1 = 1
          | otherwise = density localP
        sharedPressure localP = (convertDensityToPressure(density localP) + convertDensityToPressure (density p)) / 2



--Main Loop Components
drawParticle :: Particle -> Picture
drawParticle p = pictures [
    translate x y $ color (particleColor p) $ circleSolid (radius p)]
    -- translate x y $ color (makeColor 0.0078 0.5882 1 0.5) $ circleSolid smoothRadius]
  where (x,y) = pos p

drawDensity :: Density -> Picture
drawDensity (Density (x,y) k) = translate x y $ color (makeColor r g b a) $ rectangleSolid squareSize squareSize
  where (r,g,b,a) = customColor 
        customColor 
         | k < -0.0001 = (0.1266, 0.5330, 0.6886, 1)
         | k > 0.001 = (0.8301, 0.2914, 0.2075, 1)
         | otherwise = (1, 1, 1, 1)

initialState :: IO GameState
initialState = do
  p <- randomParticles 100
  -- let p = evenlySpacedParticles 4
  let pixx = fromIntegral width
  let pixy = fromIntegral height
  -- let d = [Density (x,y) 0 | x <- [-pixx,((-pixx) + squareSize)..pixx], y <- [-pixy,((-pixy) + squareSize)..pixy]]
  -- let d = [ Density (-300 + (i * 25), -200 + (j * 25)) 0| i <- [1..15],j <- [1..15]]
  let d = [ Density (pos x) 0| x <- p]
  
  return GameState { particles = p, densities = d, randomPoints = [(1,0.1),(1,-1),(-1,1),(-1,-1)], pause = False, generateParticles = False, mousePos = (0,0)}

render :: GameState -> Picture
-- render state = Pictures $ map drawDensity (densities state) ++ map drawParticle (particles state)
render state = Pictures $ map drawParticle (particles state)

update :: Float -> GameState -> GameState
update dt state = state { particles = finalParticles, randomPoints = newRandomPoints}
  where newParticles 
          | generateParticles state = Particle (mousePos state) (0, 0) 1 defaultRadius 0 0 ((fromIntegral $ length $ particles state) + 1.0) (makeColor 0.0078 0.5882 1 1) :particles state
          | otherwise = particles state
        particlesWithForces = map (applyGravity dt . applyPressure dt state . updateDensity state) (newParticles)
        -- finalParticles = particlesWithForces
        finalParticles = map (updatePosition dt . applyBoardCollision) particlesWithForces
        newRandomPoints = tail $ randomPoints state ++ [head $ randomPoints state]

--End main loop components

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

--Events

handleEvent ::  Event -> GameState -> GameState
-- handleEvent (EventKey (Char 'd') Down _ _) state  = 
  -- let newDensities = [Density (x,y) (calculateDensity (x,y) state - targetDensity)| Density (x,y) k <- densities state]
  -- in state { densities = newDensities }

-- handleEvent (EventKey (Char 'p') Down _ _) state  = 
--   let newDensities = [Density (x,y) (convertDensityToPressure $ calculateDensity (x,y) state)| Density (x,y) k <- densities state]
--   in state { densities = newDensities }

handleEvent (EventKey (Char 'a') Down _ _) state  = 
  let ps = map (applyPressure (1/60) state) (particles state)
  in state { particles = ps }

handleEvent (EventKey (Char 'f') Down _ mousePos) state = unsafePerformIO $ do
    -- print [convertDensityToPressure $ density p| p <- particles state]
    print [customMult (calculatePressureForce p state) (1 / density p)| p <- particles state]
    return state { particles = particles state}

handleEvent (EventKey (Char 'n') Down _ _mousePos) state = state { particles = Particle _mousePos (0, 0) 1 defaultRadius 0 0 ((fromIntegral $ length $ particles state) + 1.0) (makeColor 0.0078 0.5882 1 1) :particles state}

handleEvent (EventKey (MouseButton LeftButton) Down _ _mousePos) state = unsafePerformIO $ do 
    let newPs = changeColor _mousePos red (particles state)
    -- let distance (x,y) (x1,y1)= sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1))
    -- print $ (pos $ closestParticle mousePos (particles state)) 
    -- print $ smoothingKernel (distance (pos $ closestParticle mousePos (particles state)) (pos $ head $ particles state)) smoothRadius
    -- print $ density (closestParticle mousePos (particles state)) 
    -- print $ convertDensityToPressure $ density (closestParticle mousePos (particles state)) 
    -- print $ calculatePressureForce (closestParticle mousePos (particles state)) state
    -- print $ vel (closestParticle mousePos (particles state)) 
    -- let p = closestParticle mousePos (particles state)
    print $ smoothingKernelDerivative 0
    -- print $ applyPressure2 (1/60) state p

    print ""
    return state { particles = newPs}

handleEvent (EventKey (MouseButton LeftButton) up _ _mousePos) state = unsafePerformIO $ do
    let newPs = changeColor _mousePos black (particles state)
    return state { particles = newPs}

handleEvent (EventMotion _mousePos) state = state {mousePos = _mousePos}

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { generateParticles = True }
handleEvent (EventKey (SpecialKey KeySpace) Up _ _) state = state { generateParticles = False }



handleEvent  _ state = state

--End events

applyPressure2 :: Float -> GameState -> Particle -> Point
applyPressure2 dt state p = (density p, 0)
  where (vx,vy) = vel p
        (vx',vy') 
          | density p < 0.00000001 = (0,0)
          -- | otherwise = customSum [(vx,vy) ,customMult pressureAcceleration dt]
          | otherwise = (sign ax * min 100 (abs (ax * dt)),sign ay * min 100 (abs (ay * dt)))
        (ax,ay) = customMult (calculatePressureForce p state) (1 / density p)
        -- pressureAcceleration = customMult (calculatePressureForce p state) (1 / density p)
        sign n = abs n / n

changeColor :: Point -> Color -> [Particle] -> [Particle]
changeColor _ c [p] = [p {particleColor = c}]
changeColor (x1,y1) c (p:ps) 
  | x1 >= x - defaultRadius && x1 <= x + defaultRadius && y1 >= y - defaultRadius && y1 <= y + defaultRadius = (p {particleColor = c}):ps
  | otherwise = p:changeColor (x1,y1) c ps
  where (x,y) = pos p


closestParticle :: Point -> [Particle] -> Particle
closestParticle _ [p] = p
closestParticle (x1,y1) (p:ps) 
  | x1 >= x - smoothRadius  && x1 <= x + smoothRadius  && y1 >= y - smoothRadius  && y1 <= y + smoothRadius  = p
  | otherwise = closestParticle (x1,y1) ps
  where (x,y) = pos p

customMult :: Point -> Float -> Point
customMult (x,y) a = (a * x, a * y)

customSum :: [Point] -> Point
customSum [] = (0,0)
customSum [p] = p
customSum ((x1,y1):(x2,y2):ps) = customSum ((x1 + x2, y1 + y2):ps)

