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
    smoothRadius :: Float,
    densities :: [Density],
    randomPoints :: [Point]
  }

width, height, offset :: Int
width = 600
height = 400
offset = 300

squareSize, defaultRadius ,gravity, collisionDamping, targetDensity, pressureMultiplier :: Float
defaultRadius = 10
squareSize = 5
-- gravity = 0 
gravity = -981
collisionDamping = 0.2
targetDensity = 1.2
pressureMultiplier = 40000



randomParticle :: ID -> IO Particle
randomParticle id = do
  x <- randomRIO (-(fromIntegral width / 2 - defaultRadius), fromIntegral width / 2 - defaultRadius)
  y <- randomRIO (- (fromIntegral height / 2), fromIntegral height / 2)
  -- m <- randomRIO (1,50)
  -- vx <- randomRIO (-50, 50)  -- Random velocity
  -- vy <- randomRIO (-50, 50)  -- Random velocity
  return $ Particle (x, y) (0, 0) 1 defaultRadius 0 0 id black

randomParticles :: Float -> IO [Particle]
randomParticles n = mapM randomParticle [1..n]

evenlySpacedParticles :: Float -> [Particle]
evenlySpacedParticles n = 
  let positions_index = [ [-300 + (i * 25), -200 + (j * 25), i * n + j]| i <- [1..n],j <- [1..n]]
  in map (\[x, y, id] -> Particle (x, y) (0, 0) 1 defaultRadius 0 0 id black) positions_index

updatePosition :: Float -> Particle -> Particle
updatePosition dt p = p { pos = (x, y + vy * dt) }
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
applyBoardCollision p = p {vel = (vx',vy')}
    where (vx,vy) = vel p
          (x,y) = pos p
          vy'| abs y > abs (fromIntegral height / 2) - defaultRadius  = vy * (-1) * collisionDamping
             | otherwise = vy
          vx'| abs x > abs (fromIntegral width / 2) - defaultRadius = vx * (-1) * collisionDamping
             | otherwise = vx
          -- x' | abs x > abs (fromIntegral width / 2) - defaultRadius = sign x * ((fromIntegral width / 2) - defaultRadius)
          --    | otherwise = x
          -- y' | abs y > abs ((fromIntegral height / 2) - defaultRadius) = sign y * ((fromIntegral height / 2) - defaultRadius)
          --    | otherwise = y

-- sign :: Float -> Float
-- sign x | x >= 0 = 1
--        | otherwise = -1

applyDensity :: Particle -> Particle
applyDensity p = p {opacity = opacity'}
  where opacity' = density p 


calculateDensity :: Point -> GameState -> Float
calculateDensity (x1,y1) state = sum [ _density p | p <- particles state ]
    where _density p = mass p * influence p
          influence p = smoothingKernel (distance $ pos p) (smoothRadius state)
          distance (x,y) = sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1))

smoothingKernel :: Float -> Float -> Float
smoothingKernel distance _smoothRadius = (value**3) / volume
    where value = max 0 (_smoothRadius * _smoothRadius - distance * distance)
          volume = pi * (_smoothRadius**8) / 4

smoothingKernelDerivative :: Float -> Float -> Float
smoothingKernelDerivative distance _smoothRadius 
  | distance >= _smoothRadius = 0
  | otherwise = constant * distance * f *f
    where constant = (-24) / pi * (_smoothRadius**8)
          f = _smoothRadius * _smoothRadius - distance * distance

updateDensity :: GameState -> Particle -> Particle
updateDensity state p = p {density = density'}
  where (x,y) = pos p
        density' = calculateDensity (x,y) state

applyPressure :: Float -> GameState -> Particle -> Particle
applyPressure dt state p = p {vel = (vx',vy')}
  where (x,y) = pos p
        -- (vx',vy') = customSum [(x,y) ,customMult pressureAcceleration dt]
        (vx',vy') = customMult pressureAcceleration dt
        pressureAcceleration 
          | density p < 0.1 = (1/dt,1/dt)
          | otherwise = customMult (calculatePressureForce p state) (1 / density p)
        -- limit:: Point -> Point
        -- limit (x1,y1)
        --   | x1 > 100 && y1 > 100 = (100,100)
        --   | x1 < 100 && y1 > 100 = (x1,100)
        --   | x1 > 100 && y1 < 100 = (100,y1)
        --   | x1 < 100 && y1 < 100 = (x1,y1)

convertDensityToPressure :: Float -> Float
convertDensityToPressure _density = pressure
  where pressure = densityError * pressureMultiplier
        densityError = _density - targetDensity

calculatePressureForce :: Particle -> GameState -> Point
calculatePressureForce p state = pressureForce
  where (x1,y1) = pos p
        pressureForce = customSum [ dir (pos otherP) `customMult` (convertDensityToPressure(density otherP) * slope otherP * mass otherP / getDensity otherP) | otherP <- particles state, id otherP /= id p]
        dir (x,y) 
          -- | distance (x,y) < 1 = (1,1)
          | distance (x,y) < 1 = head $ randomPoints state
          | otherwise = (x - x1, y - y1) 
          -- | otherwise = customMult (x - x1, y - y1) ( 1/ distance (x,y))
        slope localP
          | distance (pos localP) < 1 = 1
          | otherwise = smoothingKernelDerivative (distance $ pos localP) (smoothRadius state) 
        distance (x,y) = sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1))
        getDensity localP
          | density localP < 0.1 = 1
          | otherwise = density localP



--Main Loop Components
drawParticle :: Particle -> Picture
drawParticle p = pictures [
    translate x y $ color (particleColor p) $ circleSolid (radius p),
    translate x y $ color (makeColor 0.0078 0.5882 1 (opacity p)) $ circleSolid (radius p + 10)]
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
  p <- randomParticles 1
  -- let p = evenlySpacedParticles 15
  let pixx = fromIntegral width
  let pixy = fromIntegral height
  -- let d = [Density (x,y) 0 | x <- [-pixx,((-pixx) + squareSize)..pixx], y <- [-pixy,((-pixy) + squareSize)..pixy]]
  -- let d = [ Density (-300 + (i * 25), -200 + (j * 25)) 0| i <- [1..15],j <- [1..15]]
  let d = [ Density (pos x) 0| x <- p]
  
  return GameState { particles = p, smoothRadius = 3, densities = d, randomPoints = [(1,1),(1,-1),(-1,1),(-1,-1)]}

render :: GameState -> Picture
render state = Pictures $ map drawDensity (densities state) ++ map drawParticle (particles state)
-- render state = Pictures $ map drawParticle (particles state)

update :: Float -> GameState -> GameState
update dt state = state { particles = finalParticles, randomPoints = newRandomPoints}
  -- where particlesWithForces = map (applyPressure dt state . updateDensity state) (particles state)
  where particlesWithForces = map (applyGravity dt . applyPressure dt state . updateDensity state) (particles state)
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
    white
    60
    initState
    render
    handleEvent
    update

--Events

handleEvent ::  Event -> GameState -> GameState
handleEvent (EventKey (Char 'd') Down _ _) state  = 
  let newDensities = [Density (x,y) (calculateDensity (x,y) state - targetDensity)| Density (x,y) k <- densities state]
  in state { densities = newDensities }

handleEvent (EventKey (Char 'p') Down _ _) state  = 
  let newDensities = [Density (x,y) (convertDensityToPressure $ calculateDensity (x,y) state)| Density (x,y) k <- densities state]
  in state { densities = newDensities }

handleEvent (EventKey (Char 'a') Down _ _) state  = 
  let ps = map (applyPressure (1/60) state) (particles state)
  in state { particles = ps }

handleEvent (EventKey (Char 'f') Down _ mousePos) state = unsafePerformIO $ do
    print [calculatePressureForce p state| p <- init $ particles state]
    print [density p| p <- init $ particles state]
    -- print [pos p| p <- particles state]
    return state { particles = particles state}

handleEvent (EventKey (Char 'n') Down _ mousePos) state = state { particles = Particle mousePos (0, 0) 1 defaultRadius 0 0 ((id $ last $ particles state) + 1.0) black :particles state}

handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) state = unsafePerformIO $ do 
    let newPs = changeColor mousePos red (particles state)
    
    print $ density (closestParticle mousePos (particles state)) 
    print $ calculatePressureForce (closestParticle mousePos (particles state)) state
    print $ vel (closestParticle mousePos (particles state)) 
    print ""
    return state { particles = newPs}

handleEvent (EventKey (MouseButton LeftButton) up _ mousePos) state = unsafePerformIO $ do
    let newPs = changeColor mousePos black (particles state)
    return state { particles = newPs}

handleEvent  _ state = state

--End events

changeColor :: Point -> Color -> [Particle] -> [Particle]
changeColor _ _ [p] = [p]
changeColor (x1,y1) c (p:ps) 
  | x1 >= x - defaultRadius && x1 <= x + defaultRadius && y1 >= y - defaultRadius && y1 <= y + defaultRadius = (p {particleColor = c}):ps
  | otherwise = p:changeColor (x1,y1) c ps
  where (x,y) = pos p


closestParticle :: Point -> [Particle] -> Particle
closestParticle _ [p] = p
closestParticle (x1,y1) (p:ps) 
  | x1 >= x - defaultRadius && x1 <= x + defaultRadius && y1 >= y - defaultRadius && y1 <= y + defaultRadius = p
  | otherwise = closestParticle (x1,y1) ps
  where (x,y) = pos p

customMult :: Point -> Float -> Point
customMult (x,y) a = (a * x, a * y)

customSum :: [Point] -> Point
customSum ((x1,y1):(x2,y2):ps) = customSum ((x1 + x2, y1 + y2):ps)
customSum [p] = p
customSum [] = (0,0)

