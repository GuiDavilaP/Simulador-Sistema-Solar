{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Simulator
    ( Simulator(..)
    , initializeSolarSystem
    , getSystemState
    , setTimeScale
    , addBody, removeBody, addBodies, removeBodies
    , updateSimulator
    , resetSimulator
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, Value, object, (.=))
import Data.List (tails)
import CelestialBody

data Simulator = Simulator
    { bodies         :: [CelestialBody]
    , timeScale      :: Double
    , numCollisions  :: Integer
    } deriving (Show, Generic)

initializeSolarSystem :: Simulator
initializeSolarSystem = 
    let timeScale = 24 * 3600 * 10 -- 10 days in seconds
        sun      = CelestialBody "Sol"      1.989e30 (0, 0)        (0, 0)        40 (255, 220, 0)
        mercury  = CelestialBody "Mercúrio" 3.285e23 (5.791e10, 0) (0, 47.87e3)  6  (169, 169, 169)
        venus    = CelestialBody "Vênus"    4.867e24 (1.082e11, 0) (0, 35.02e3)  9  (255, 198, 73)
        earth    = CelestialBody "Terra"    5.972e24 (1.496e11, 0) (0, 29.78e3)  10 (100, 149, 237)
        mars     = CelestialBody "Marte"    6.39e23  (2.279e11, 0) (0, 24.077e3) 8  (205, 92, 92)
        jupiter  = CelestialBody "Júpiter"  1.898e27 (7.785e11, 0) (0, 13.07e3)  25 (255, 165, 0)
        saturn   = CelestialBody "Saturno"  5.683e26 (1.429e12, 0) (0, 9.69e3)   22 (238, 232, 205)
        uranus   = CelestialBody "Urano"    8.681e25 (2.871e12, 0) (0, 6.81e3)   16 (173, 216, 230)
        neptune  = CelestialBody "Netuno"   1.024e26 (4.495e12, 0) (0, 5.43e3)   15 (0, 0, 128)
    in Simulator [sun, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune] timeScale 0

getSystemState :: Simulator -> Value
getSystemState sim = object
    [ "bodies" .= map toFrontEnd (bodies sim)
    -- , "stats"  .= object
    --     [ "total_bodies" .= length (bodies sim)
    --     , "collisions"   .= numCollisions sim
    --     ]
    ]

setTimeScale :: Simulator -> Double -> Simulator
setTimeScale sim newTimeScale = sim { timeScale = newTimeScale }

addBody :: Simulator -> CelestialBody -> Simulator
addBody sim body = sim { bodies = body : bodies sim }

removeBody :: Simulator -> String -> Simulator
removeBody sim nameToRemove = sim { bodies = filter ((/= nameToRemove) . name) (bodies sim) }

addBodies :: Simulator -> [CelestialBody] -> Simulator
addBodies sim newBodies = sim { bodies = newBodies ++ bodies sim }

removeBodies :: Simulator -> [String] -> Simulator
removeBodies sim namesToRemove = sim { bodies = filter ((`notElem` namesToRemove) . name) (bodies sim) }

updateSimulator :: Simulator -> Double -> Simulator
updateSimulator sim dt =
    let dtScaled = dt * timeScale sim
        sim' = updatePhysics sim dtScaled
        collisions = manageCollisions sim'
        numNew = fromIntegral (length collisions)
    in sim'
        { bodies = filter ((`notElem` collisions) . name) (bodies sim')
        , numCollisions = numCollisions sim' + numNew
        }

updatePhysics :: Simulator -> Double -> Simulator
updatePhysics sim dt = sim { bodies = updatedBodies }
  where
    accelerations = computeAccelerations (bodies sim)
    updatedVelocities = [updateVelocity body acc dt | (body, acc) <- zip (bodies sim) accelerations]
    updatedBodies     = map (`updatePosition` dt) updatedVelocities

manageCollisions :: Simulator -> [String]
manageCollisions sim = map (name . weakest) collisions
  where
    pairs = [ (b1, b2) | (b1:rest) <- tails (bodies sim), b2 <- rest ]
    collisions = filter (uncurry checkCollision) pairs
    weakest (a, b) = if mass a <= mass b then a else b

resetSimulator :: Simulator -> Simulator
resetSimulator sim = sim { bodies = [] }
