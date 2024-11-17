import Prelude hiding (id)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)
import Data.IORef
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V
import CustomDataStructure


data GameState = GameState
  { particles :: Vector2D Particle,
    densities :: [Density],
    randomPoints :: [Point],
    pause :: Bool,
    generateParticles :: Bool,
    mousePos :: Point,
    showGrid :: Bool,
    gravity :: Float
  }

width, height, offset, gridColumn :: Int
width = 600
height = 400
offset = 300
gridColumn = floor (fromIntegral width / squareSize)

smoothRadius, squareSize, defaultRadius , collisionDamping, targetDensity, pressureMultiplier :: Float
defaultRadius = 5
smoothRadius = 25
squareSize = smoothRadius
collisionDamping = 0.5
targetDensity = 2.651827e-3
pressureMultiplier = 50000000




randomParticle :: ID -> IO Particle
randomParticle id = do
  x <- randomRIO (-(fromIntegral width / 2 - smoothRadius ), fromIntegral width / 2 - smoothRadius )
  y <- randomRIO (- (fromIntegral height / 2 - smoothRadius), fromIntegral height / 2 - smoothRadius)
  -- m <- randomRIO (1,50)
  -- vx <- randomRIO (-50, 50)  -- Random velocity
  -- vy <- randomRIO (-50, 50)  -- Random velocity
  return $ Particle (x, y) (0, 0) 1 defaultRadius 0 0 id (makeColor 0.0078 0.5882 1 1)

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

applyGravity :: Float -> GameState -> Particle -> Particle
applyGravity seconds state p = p {vel = (vx',vy')}
    where (vx,vy) = vel p
          vx' = vx
          vy' = vy + (gravity state) * seconds
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
calculateDensity p state = sum [ _density otherP | otherP <- ps, distance (pos otherP) < smoothRadius]
    where ps = getNList (convertPositionToIndex (pos p) + gridColumn) (particles state) ++
               getNList (convertPositionToIndex (pos p) - gridColumn) (particles state) ++
               getNList (convertPositionToIndex (pos p) + 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) - 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) + gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) + gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) - gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) - gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (pos p)) (particles state) 
          (x1,y1) = pos p
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
        pressureForce = customSum [ dir (pos otherP) (id otherP) `customMult` (sharedPressure otherP * slope otherP * mass otherP / getDensity otherP) 
                                                                  | otherP <- ps
                                                                  , id otherP /= id p && distance (pos otherP) < smoothRadius]
        ps = getNList (convertPositionToIndex (pos p) + gridColumn) (particles state) ++
               getNList (convertPositionToIndex (pos p) - gridColumn) (particles state) ++
               getNList (convertPositionToIndex (pos p) + 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) - 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) + gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) + gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) - gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) - gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (pos p)) (particles state) 
        
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
        sharedPressure localP = (convertDensityToPressure (density localP) + convertDensityToPressure (density p)) / 2


putParticleToGrid :: [Particle] -> Vector2D Particle -> Vector2D Particle
putParticleToGrid [] grid = grid
putParticleToGrid (p:ps) grid = putParticleToGrid ps newGrid
  where newGrid = addElementToRow (convertPositionToIndex $ pos p) p grid

convertPositionToIndex :: Point -> Int
convertPositionToIndex (x,y) = floor ((x + (fromIntegral width / 2)) / squareSize) + (floor ((y + fromIntegral height / 2) / squareSize ) * gridColumn)

updateGrid ::  Vector2D Particle -> Vector2D Particle
updateGrid grid = putParticleToGrid (vector2DToList grid) newGrid
-- updateGrid grid = map2D  [(p, convertPositionToIndex $ pos p) | p <- vector2DToList grid]
    where newGrid = emptyVector2DWithNRow (round ((fromIntegral width / squareSize) * (fromIntegral height/ squareSize)))


--Main Loop Components
drawParticle :: Particle -> Picture
drawParticle p = pictures [
    translate x y $ color (particleColor p) $ circleSolid (radius p)]
    -- translate x y $ color (makeColor 0.0078 0.5882 1 0.5) $ circleSolid smoothRadius]
  where (x,y) = pos p

drawGrid :: Point -> Picture
drawGrid (x,y) = translate x y $ color white $ rectangleWire squareSize squareSize

initialState :: IO GameState
initialState = do
  p <- randomParticles 1
  -- let p = evenlySpacedParticles 4
  let pixx = fromIntegral width
  let pixy = fromIntegral height
  -- let d = [Density (x,y) 0 | x <- [-pixx,((-pixx) + squareSize)..pixx], y <- [-pixy,((-pixy) + squareSize)..pixy]]
  -- let d = [ Density (-300 + (i * 25), -200 + (j * 25)) 0| i <- [1..15],j <- [1..15]]
  let d = [ Density (pos x) 0| x <- p]

  return GameState { particles = list2DToVector2D [p], densities = d, randomPoints = [(1,0.1),(1,-1),(-1,1),(-1,-1)], pause = True, generateParticles = False, mousePos = (0,0), showGrid = False, gravity = 0}

render :: GameState -> Picture
-- render state = Pictures $ [drawGrid (x,y) (round (y * 8 + x)) | x <- [0], y <- [0]] 
render state
    | showGrid state = Pictures $ [drawGrid (x,y) | x <- [-n + cellSize,-n + 3 * cellSize..n - cellSize], y <- [-m + cellSize,-m + 3 * cellSize..m - cellSize]]
               ++ vector2DToList (map2D drawParticle (particles state))
    | otherwise = Pictures $ vector2DToList (map2D drawParticle (particles state))
  where n = fromIntegral width / 2
        m = fromIntegral height / 2
        cellSize = squareSize / 2

update :: Float -> GameState -> GameState
update dt state = state { particles = finalParticles, randomPoints = newRandomPoints}
  where newParticles
          | generateParticles state = addElementToRow 0 newParticle (particles state)
          | otherwise = particles state
        newParticle = Particle (mousePos state) (0, 0) 1 defaultRadius 0 0 ((fromIntegral $ length2D $ particles state) + 1.0) (makeColor 0.0078 0.5882 1 1)

        particlesWithGridUpdated = updateGrid newParticles
        particlesWithForces = map2D (applyGravity dt state . applyPressure dt state . updateDensity state) particlesWithGridUpdated

        finalParticles
          | pause state = particlesWithForces
          | otherwise = map2D (updatePosition dt . applyBoardCollision) particlesWithForces

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
--Debugging tools
handleEvent (EventKey (Char 'f') Down _ _) state = unsafePerformIO $ do
    -- print [convertDensityToPressure $ density p| p <- particles state]
    let p = closestParticle (mousePos state) (vector2DToList $ particles state)
    let ps = getNList (convertPositionToIndex (pos p) + gridColumn) (particles state) ++
               getNList (convertPositionToIndex (pos p) - gridColumn) (particles state) ++
               getNList (convertPositionToIndex (pos p) + 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) - 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) + gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) + gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) - gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (pos p) - gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (pos p)) (particles state) 
    print ps
    return state { particles = particles state}

handleEvent (EventKey (Char 'n') Down _ _mousePos) state = state { particles = addElementToRow 0 newParticle (particles state) }
  where newParticle = Particle (mousePos state) (0, 0) 1 defaultRadius 0 0 ((fromIntegral $ length2D $ particles state) + 1.0) (makeColor 0.0078 0.5882 1 1)


handleEvent (EventKey (MouseButton LeftButton) Down _ _mousePos) state = unsafePerformIO $ do
    let newPs = map2D (changeColor _mousePos red) (particles state)
    -- let distance (x,y) (x1,y1)= sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1))
    -- print $ (pos $ closestParticle mousePos (particles state)) 
    -- print $ smoothingKernel (distance (pos $ closestParticle mousePos (particles state)) (pos $ head $ particles state)) smoothRadius
    -- print $ density (closestParticle mousePos (particles state)) 
    -- print $ convertDensityToPressure $ density (closestParticle mousePos (particles state)) 
    -- print $ calculatePressureForce (closestParticle mousePos (particles state)) state
    -- print $ vel (closestParticle mousePos (particles state)) 
    -- let p = closestParticle mousePos (particles state)
    let convertPositionToIndex (x,y) = floor ((x + (fromIntegral width / 2)) / squareSize) + (floor ((y + fromIntegral height / 2) / squareSize ) * floor (fromIntegral width / squareSize))

    print $ convertPositionToIndex _mousePos

    -- print $ applyPressure2 (1/60) state p

    return state { particles = newPs}
handleEvent (EventKey (MouseButton LeftButton) up _ _mousePos) state = unsafePerformIO $ do
    let newPs = map2D (changeColor _mousePos (makeColor 0.0078 0.5882 1 1)) (particles state)
    return state { particles = newPs}

--End Debugging tools

handleEvent (EventKey (Char 'p') Down _ _) state  = state { pause = not $ pause state }


handleEvent (EventMotion _mousePos) state = state {mousePos = _mousePos}

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { generateParticles = True }
handleEvent (EventKey (SpecialKey KeySpace) Up _ _) state = state { generateParticles = False }

handleEvent (EventKey (Char 'G') Down _ _) state = state { showGrid = True }
handleEvent (EventKey (Char 'G') Up _ _) state = state { showGrid = False }

handleEvent (EventKey (Char 'g') Down _ _) state
  | gravity state == 0 = state { gravity = -(981 * 5) }
  | otherwise = state { gravity = 0 }

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

changeColor :: Point -> Color -> Particle ->  Particle
changeColor (x1,y1) c p
  | x1 >= x - defaultRadius && x1 <= x + defaultRadius && y1 >= y - defaultRadius && y1 <= y + defaultRadius = p {particleColor = c}
  | otherwise = p
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

