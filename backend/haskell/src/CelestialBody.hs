{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CelestialBody 
    ( CelestialBody(..)
    , setName, setMass, setPosition, setVelocity, setRadius, setColor
    , updatePosition, distance, checkCollision
    , computeGravitationalForce, computeGravitationalForces
    , computeAccelerations, updateVelocity
    , fromFrontEnd, toFrontEnd
    ) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson (ToJSON, Value, object, (.=))
import Data.Aeson.Types (FromJSON, parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Maybe (fromMaybe)

data CelestialBody = CelestialBody
    { name     :: String
    , mass     :: Double
    , position :: (Double, Double)
    , velocity :: (Double, Double)
    , radius   :: Double
    , color    :: (Int, Int, Int)
    } deriving (Show, Eq, Generic)

instance ToJSON CelestialBody
instance FromJSON CelestialBody

-- Setters
setName       body new = body { name     = new }
setMass       body new = body { mass     = new }
setPosition   body new = body { position = new }
setVelocity   body new = body { velocity = new }
setRadius     body new = body { radius   = new }
setColor      body new = body { color    = new }

-- Update position based on velocity
updatePosition :: CelestialBody -> Double -> CelestialBody
updatePosition body dt = body { position = (x + vx * dt, y + vy * dt) }
  where
    (vx, vy) = velocity body
    (x,  y ) = position body

-- Distance between two bodies
distance :: CelestialBody -> CelestialBody -> Double
distance b1 b2 = sqrt $ dx^2 + dy^2
  where
    (x1, y1) = position b1
    (x2, y2) = position b2
    dx = x2 - x1
    dy = y2 - y1

-- Collision check
checkCollision :: CelestialBody -> CelestialBody -> Bool
checkCollision b1 b2 = distance b1 b2 <= (radius b1 + radius b2)

-- Gravitational force from b2 on b1
computeGravitationalForce :: CelestialBody -> CelestialBody -> (Double, Double)
computeGravitationalForce b1 b2 = (fx, fy)
  where
    g = 6.67430e-11
    (x1, y1) = position b1
    (x2, y2) = position b2
    dx = x2 - x1
    dy = y2 - y1
    dist = sqrt (dx*dx + dy*dy)
    force = g * mass b1 * mass b2 / (dist * dist)
    fx = force * dx / dist
    fy = force * dy / dist

-- Compute net gravitational forces on each body
computeGravitationalForces :: [CelestialBody] -> [(Double, Double)]
computeGravitationalForces bodies = 
    [ sumForces body | body <- bodies ]
  where
    sumForces b1 = foldl addForce (0, 0)
                   [ computeGravitationalForce b1 b2 | b2 <- bodies, b1 /= b2 ]
    addForce (fx1, fy1) (fx2, fy2) = (fx1 + fx2, fy1 + fy2)

-- Convert force to acceleration
computeAccelerations :: [CelestialBody] -> [(Double, Double)]
computeAccelerations bodies = 
    zipWith (\(fx, fy) m -> (fx / m, fy / m)) forces masses
  where
    forces = computeGravitationalForces bodies
    masses = map mass bodies

-- Update velocity using acceleration and time
updateVelocity :: CelestialBody -> (Double, Double) -> Double -> CelestialBody
updateVelocity body (ax, ay) dt = body { velocity = (vx + ax * dt, vy + ay * dt) }
  where
    (vx, vy) = velocity body

-- JSON interface
fromFrontEnd :: Value -> Maybe CelestialBody
fromFrontEnd (Object obj) = do
    name     <- KM.lookup "name" obj >>= parseMaybe parseJSON
    mass     <- KM.lookup "mass" obj >>= parseMaybe parseJSON
    position <- KM.lookup "position" obj >>= parseMaybe parseJSON
    velocity <- KM.lookup "velocity" obj >>= parseMaybe parseJSON
    radius   <- KM.lookup "radius" obj >>= parseMaybe parseJSON
    color    <- KM.lookup "color" obj >>= parseMaybe parseJSON
    return $ CelestialBody name mass position velocity radius color
fromFrontEnd _ = Nothing

toFrontEnd :: CelestialBody -> Value
toFrontEnd body = object
    [ "name"     .= name body
    , "mass"     .= mass body
    , "position" .= position body
    , "velocity" .= velocity body
    , "radius"   .= radius body
    , "color"    .= color body
    ]