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


-- ============================================================
-- ================== Constant/default value ==================
-- ============================================================

width, height, offset, gridColumn :: Int
width = 300
height = 400
offset = 300
gridColumn = floor (fromIntegral width / squareSize)

viscosityStrength,interactionForceRadius,smoothRadius, squareSize, defaultRadius , collisionDamping, targetDensity, pressureMultiplier :: Float
defaultRadius = 2
smoothRadius = 25
interactionForceRadius = 60
squareSize = smoothRadius
collisionDamping = 0.5
targetDensity = 8.651827e-3
pressureMultiplier = 2000000
viscosityStrength = 0.1

defaultParticleColor :: Color
defaultParticleColor = makeColor 0.0078 0.5882 1 1


-- ============================================================
-- ================== Generate particles ======================
-- ============================================================

randomParticle :: ID -> IO Particle
randomParticle id = do
  x <- randomRIO (-(fromIntegral width / 2 - smoothRadius ), fromIntegral width / 2 - smoothRadius )
  y <- randomRIO (- (fromIntegral height / 2 - smoothRadius), fromIntegral height / 2 - smoothRadius)
  return $ Particle (x, y) (x,y) (0, 0) 1 defaultRadius 0 0 id defaultParticleColor

randomParticles :: Float -> IO [Particle]
randomParticles n = mapM randomParticle [1..n]



-- ============================================================
-- ====== Calculate force/density/pressure/viscosity ==========
-- ============================================================

pointMult :: Point -> Float -> Point
pointMult (x,y) a = (a * x, a * y)

pointSum :: [Point] -> Point
pointSum [] = (0,0)
pointSum [p] = p
pointSum ((x1,y1):(x2,y2):ps) = pointSum ((x1 + x2, y1 + y2):ps)

calculateDensity :: Particle -> GameState -> Float
calculateDensity p state = sum [ _density otherP | otherP <- ps, distance (predPos otherP) < smoothRadius]
    where ps = getNList (convertPositionToIndex (predPos p) + gridColumn) (particles state) ++
               getNList (convertPositionToIndex (predPos p) - gridColumn) (particles state) ++
               getNList (convertPositionToIndex (predPos p) + 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) - 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) + gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) + gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) - gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) - gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p)) (particles state) 
          (x1,y1) = predPos p
          _density localP = mass localP * influence localP
          influence localP = smoothingKernel (distance $ predPos localP)
          distance (x,y) = sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1))

smoothingKernel :: Float -> Float
smoothingKernel distance
  | distance >= smoothRadius = 0
  | otherwise = (smoothRadius - distance) * (smoothRadius - distance) / volume
    where volume = (pi * (smoothRadius**4)) / 6

smoothingKernelDerivative :: Float -> Float
smoothingKernelDerivative distance
  | distance >= smoothRadius = 0
  | otherwise = (distance - smoothRadius) * constant
    where constant = 12 / ((smoothRadius**4) * pi)


convertDensityToPressure :: Float -> Float
convertDensityToPressure _density = pressure
  where pressure = densityError * pressureMultiplier
        densityError = _density - targetDensity

calculatePressureForce :: Particle -> GameState -> Point
calculatePressureForce p state = pressureForce
  where (x1,y1) = predPos p
        pressureForce = pointSum [ dir (predPos otherP) `pointMult` (sharedPressure otherP * slope otherP * mass otherP / getDensity otherP) 
                                                                  | otherP <- ps
                                                                  , id otherP /= id p && distance (predPos otherP) < smoothRadius]
        ps = getNList (convertPositionToIndex (predPos p) + gridColumn) (particles state) ++
               getNList (convertPositionToIndex (predPos p) - gridColumn) (particles state) ++
               getNList (convertPositionToIndex (predPos p) + 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) - 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) + gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) + gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) - gridColumn + 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p) - gridColumn - 1) (particles state) ++
               getNList (convertPositionToIndex (predPos p)) (particles state) 
        
        dir :: Point -> Point
        dir (x,y)
          | distance (x,y) < 1 = pointMult (x - x1, y - y1) ( 1/ distance (x,y))
          | otherwise = pointMult (x - x1, y - y1) ( 1/ distance (x,y))
        slope localP
          | distance (predPos localP) == 0 = smoothingKernelDerivative 1
          | otherwise = smoothingKernelDerivative (distance $ predPos localP)
        distance (x,y) = sqrt ((x - x1) * (x - x1) + (y - y1) * (y - y1))
        getDensity localP
          | density localP < 0.1 = 1
          | otherwise = density localP
        sharedPressure localP = (convertDensityToPressure (density localP) + convertDensityToPressure (density p)) / 2

calculateInteractionForce :: GameState -> Particle -> Point
calculateInteractionForce state p = interactionForce
  where interactionForce 
          | sqrDst <= interactionForceRadius * interactionForceRadius = pointSum [pointSum [pointMult dir strength, pointMult (vel p) (-1 * centreT)]]
          | otherwise = (0,0)
        dir 
          | sqrDst < 0.00001 = (0,0)
          | otherwise =  pointMult (x,y) (1/(sqrt sqrDst))
        strength 
          | isPullInteraction state = -4
          | isPushInteraction state = 10
          | otherwise = 0
        centreT = 1 - sqrt sqrDst / interactionForceRadius
        sqrDst = x*x + y*y
        (x,y) = pointSum [mousePos state, pointMult (pos p) (-1)]

calculatePredictedPos :: Float -> Particle -> Particle
calculatePredictedPos dt p = p {predPos = pointSum [pos p, pointMult (vel p) (1/30)]}

calculateViscosity :: GameState -> Particle -> Point  
calculateViscosity state p = pointMult viscosityForce viscosityStrength
  where viscosityForce = pointSum [pointMult (pointSum [(0,0), pointMult (vel p) (-1)]) (influence otherP) | otherP <- ps
                                                                , id otherP /= id p && distance otherP < smoothRadius]
        ps = getNList (convertPositionToIndex (predPos p) + gridColumn) (particles state) ++
                    getNList (convertPositionToIndex (predPos p) - gridColumn) (particles state) ++
                    getNList (convertPositionToIndex (predPos p) + 1) (particles state) ++
                    getNList (convertPositionToIndex (predPos p) - 1) (particles state) ++
                    getNList (convertPositionToIndex (predPos p) + gridColumn + 1) (particles state) ++
                    getNList (convertPositionToIndex (predPos p) + gridColumn - 1) (particles state) ++
                    getNList (convertPositionToIndex (predPos p) - gridColumn + 1) (particles state) ++
                    getNList (convertPositionToIndex (predPos p) - gridColumn - 1) (particles state) ++
                    getNList (convertPositionToIndex (predPos p)) (particles state) 

        influence localP = viscositySmoothingKernel (distance localP) smoothRadius
        distance localP = magnitude $ pointSum [predPos p, pointMult (predPos localP) (-1)]
        magnitude (x,y) = sqrt (x*x + y*y)

viscositySmoothingKernel :: Float -> Float -> Float
viscositySmoothingKernel distance _smoothRadius = (value**3) / volume
    where value = max 0 (_smoothRadius * _smoothRadius - distance * distance)
          volume = pi * (_smoothRadius**8) / 4



-- ============================================================
-- =================== Update particles =======================
-- ============================================================

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

updateDensity :: GameState -> Particle -> Particle
updateDensity state p = p {density = density'}
  where density' = calculateDensity p state

applyPressure :: Float -> GameState -> Particle -> Particle
applyPressure dt state p = p {vel = newv}
  where newv = pointSum [vel p, pointMult pressureAcceleration dt]
        pressureAcceleration = pointMult (calculatePressureForce p state) (1 / getDensity p)
        getDensity localP
          | density localP < 0.00001 = 1
          | otherwise = density localP

applyViscosity :: GameState -> Float -> Particle -> Particle
applyViscosity state dt p = p {vel = newVelocity}
  where newVelocity = pointSum [vel p, pointMult viscosityAcceleration dt]
        viscosityAcceleration = pointMult (calculateViscosity state p) (1 / getDensity p)
        getDensity localP
          | density localP < 0.00001 = 1
          | otherwise = density localP
          
applyInteractionForce :: GameState -> Float -> Vector2D Particle -> Vector2D Particle
applyInteractionForce state dt =  map2D helper
  where helper :: Particle -> Particle
        helper p = p {vel = newV}
          where newV = pointSum [vel p, pointMult interactionAcceleration dt]
                interactionAcceleration = pointMult (calculateInteractionForce state p) (1 / getDensity p)
                getDensity localP
                  | density localP < 0.00001 = 1
                  | otherwise = density localP

changeColor :: Particle -> Particle
changeColor p = p { particleColor = colorGradient (magnitude (vel p)) }
  where
    magnitude (x, y) = x * x + y * y  
    colorGradient :: Float -> Color
    colorGradient value
      | t <= 0.2  = interpolate (t / 0.2) defaultParticleColor cyan
      | t <= 0.4  = interpolate ((t - 0.2) / 0.2) cyan green
      | t <= 0.6  = interpolate ((t - 0.4) / 0.2) green yellow
      | t <= 0.8  = interpolate ((t - 0.6) / 0.2) yellow orange
      | otherwise = interpolate ((t - 0.8) / 0.2) orange red
      where
        maxMagnitude = 8000  
        t = max 0 (min 1 (value / maxMagnitude))

    interpolate :: Float -> Color -> Color -> Color
    interpolate temp c1 c2 =
      let (r1, g1, b1, _) = rgbaOfColor c1 
          (r2, g2, b2, _) = rgbaOfColor c2  
      in makeColor (r1 + temp * (r2 - r1))
                   (g1 + temp * (g2 - g1))
                   (b1 + temp * (b2 - b1))
                   0.8  


-- ============================================================
-- ================= Functions for grid =======================
-- ============================================================

putParticleToGrid :: [Particle] -> Vector2D Particle -> Vector2D Particle
putParticleToGrid [] grid = grid
putParticleToGrid (p:ps) grid = putParticleToGrid ps newGrid
  where newGrid = addElementToRow (convertPositionToIndex $ pos p) p grid

convertPositionToIndex :: Point -> Int
convertPositionToIndex (x,y) = floor ((x + (fromIntegral width / 2)) / squareSize) + (floor ((y + fromIntegral height / 2) / squareSize ) * gridColumn)

updateGrid ::  Vector2D Particle -> Vector2D Particle
updateGrid grid = putParticleToGrid (vector2DToList grid) newGrid
    where newGrid = emptyVector2DWithNRow (round ((fromIntegral width / squareSize) * (fromIntegral height/ squareSize)))


-- ============================================================
-- ================== Main Loop Components ====================
-- ============================================================

drawParticle :: GameState -> Particle -> Picture
drawParticle state p 
  | showWater state = pictures [
    translate x y $ color (particleColor p) $ circleSolid (radius p),
    translate x y $ color (makeColor 0.0078 0.5882 1 0.5) $ circleSolid 10]
  | otherwise = pictures [
    translate x y $ color (particleColor p) $ circleSolid (radius p)]
  where (x,y) = pos p

drawGrid :: Point -> Picture
drawGrid (x,y) = translate x y $ color white $ rectangleWire squareSize squareSize

initialState :: IO GameState
initialState = do
  p <- randomParticles 100
  let pixx = fromIntegral width
  let pixy = fromIntegral height
  let x = 10
  return GameState { particles = list2DToVector2D [p], 
                     randomPoints = [(x,x),(x,-x),(-x,x),(-x,-x)], 
                     pause = False, 
                     generateParticles = False, 
                     mousePos = (0,0), 
                     showGrid = False, 
                     showWater = False, 
                     gravity = 0, 
                     isPullInteraction = False, 
                     isPushInteraction = False,
                     addColor = False
                     }

render :: GameState -> Picture
render state
    | showGrid state = Pictures $ [drawGrid (x,y) | x <- [-n + cellSize,-n + 3 * cellSize..n - cellSize], y <- [-m + cellSize,-m + 3 * cellSize..m - cellSize]]
                       ++ vector2DToList (map2D (drawParticle state) (particles state))
                       ++ [translate mousePosX mousePosY $ color red $ circle interactionForceRadius]
    | otherwise = Pictures $ vector2DToList (map2D (drawParticle state) (particles state))
  where n = fromIntegral width / 2
        m = fromIntegral height / 2
        cellSize = squareSize / 2
        (mousePosX,mousePosY) = mousePos state

update :: Float -> GameState -> GameState
update dt state = state { particles = finalParticles, randomPoints = newRandomPoints}
  where newParticles
          | generateParticles state = addElementToRow 0 newParticle (particles state)
          | otherwise = particles state
        newParticle = Particle (mousePos state) (mousePos state) (0, 0) 1 defaultRadius 0 0 ((fromIntegral $ length2D $ particles state) + 1.0) defaultParticleColor

        particlesWithGridUpdated = updateGrid newParticles
        
        particleWithPredictedPos = map2D (calculatePredictedPos dt) particlesWithGridUpdated
        
        particlesWithForces = map2D (applyGravity dt state . applyViscosity state dt . applyPressure dt state . updateDensity state) particleWithPredictedPos
        
        particlesWithInteractionForces 
          | isPullInteraction  state && isPushInteraction state = particlesWithForces
          | isPullInteraction  state || isPushInteraction state = applyInteractionForce state dt particlesWithForces
          | otherwise = particlesWithForces

        particleWithColor 
          | addColor state = map2D changeColor particlesWithInteractionForces
          | otherwise = particlesWithInteractionForces

        finalParticles
          | pause state = particleWithColor
          | otherwise = map2D (updatePosition dt . applyBoardCollision) particleWithColor

        newRandomPoints = tail $ randomPoints state ++ [head $ randomPoints state]



-- ============================================================
-- ====================== Main function =======================
-- ============================================================

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


-- ============================================================
-- ========================= Events ===========================
-- ============================================================
  
handleEvent ::  Event -> GameState -> GameState

handleEvent (EventKey (MouseButton LeftButton) Down _ _mousePos) state = state {isPullInteraction = True}
handleEvent (EventKey (MouseButton LeftButton) Up _ _mousePos) state = state {isPullInteraction = False}

handleEvent (EventKey (MouseButton RightButton) Down _ _mousePos) state = state {isPushInteraction = True}
handleEvent (EventKey (MouseButton RightButton) Up _ _mousePos) state = state {isPushInteraction = False}

handleEvent (EventKey (Char 'p') Down _ _) state  = state { pause = not $ pause state }

handleEvent (EventMotion _mousePos) state = state {mousePos = _mousePos}

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = state { generateParticles = True }
handleEvent (EventKey (SpecialKey KeySpace) Up _ _) state = state { generateParticles = False }

handleEvent (EventKey (SpecialKey KeyDelete) Down _ _) state = state { particles = newGrid }
  where newGrid = emptyVector2DWithNRow (round ((fromIntegral width / squareSize) * (fromIntegral height/ squareSize)))

handleEvent (EventKey (Char 'n') Down _ _mousePos) state = state { particles = addElementToRow 0 newParticle (particles state)}
  where newParticle = Particle (mousePos state) (mousePos state) (0, 0) 1 defaultRadius 0 0 ((fromIntegral $ length2D $ particles state) + 1.0) defaultParticleColor


handleEvent (EventKey (Char 'G') Down _ _) state = state { showGrid = not $ showGrid state}

handleEvent (EventKey (Char 'w') Down _ _) state = state { showWater = not $ showWater state }

handleEvent (EventKey (Char 'c') Down _ _) state 
  | addColor state = state { addColor = False, particles = newPs }
  | otherwise = state { addColor = True }
  where newPs = map2D helper (particles state)
        helper p = p {particleColor = defaultParticleColor}

handleEvent (EventKey (Char 'g') Down _ _) state
  | gravity state == 0 = state { gravity = -(98.1 * 2) }
  | otherwise = state { gravity = 0 }

handleEvent  _ state = state
