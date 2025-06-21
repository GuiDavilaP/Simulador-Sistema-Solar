{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.IO (hSetBuffering, BufferMode(LineBuffering), stdin, stdout)
import Data.Aeson
import Data.Aeson.Types (Value(..), Object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Simulator
import CelestialBody (fromFrontEnd, name)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering
    let sim = initializeSolarSystem
    BL.putStrLn (encode $ getSystemState sim)
    commandLoop sim

commandLoop :: Simulator -> IO ()
commandLoop sim = do
    input <- BS.getLine
    case decode (BL.fromStrict input) of
        Nothing -> do
            sendError "Invalid JSON"
            commandLoop sim
        Just cmd -> do
            (newSim, response) <- handleCommand sim cmd
            BL.putStrLn (encode response)
            commandLoop newSim

handleCommand :: Simulator -> Value -> IO (Simulator, Value)
handleCommand sim (Object obj) =
    case KM.lookup "command" obj of
        Just (String "update")        -> handleUpdateCommand sim obj
        Just (String "set_time_scale")-> handleTimeScaleCommand sim obj
        Just (String "add_body")      -> handleAddBodyCommand sim obj
        Just (String "reset")         -> return (initializeSolarSystem, getSystemState initializeSolarSystem)
        Just (String "get_state")     -> return (sim, getSystemState sim)
        Just (String unknownCmd)      -> return (sim, errorMsg $ "Unknown command: " ++ show unknownCmd)
        _                             -> return (sim, errorMsg "Missing or invalid 'command'")
handleCommand sim _ = return (sim, errorMsg "Invalid command format")

-- Sub-handlers

handleUpdateCommand :: Simulator -> Object -> IO (Simulator, Value)
handleUpdateCommand sim obj =
    let paused = case KM.lookup "paused" obj of
                    Just (Bool b) -> b
                    _             -> False
        dt = case KM.lookup "dt" obj of
                Just (Number n) -> fromRational $ toRational n
                _               -> 0.016
    in if paused
       then return (sim, getSystemState sim)
       else let newSim = updateSimulator sim dt
             in return (newSim, getSystemState newSim)

handleTimeScaleCommand :: Simulator -> Object -> IO (Simulator, Value)
handleTimeScaleCommand sim obj =
    case KM.lookup "multiplier" obj of
        Just (Number n) ->
            let val = fromRational $ toRational n
            in if val <= 0
               then return (sim, errorMsg "Time scale must be positive")
               else return (setTimeScale sim (val * 24 * 3600 * 10), statusOk)
        _ -> return (sim, errorMsg "'multiplier' is required and must be a number")

handleAddBodyCommand :: Simulator -> Object -> IO (Simulator, Value)
handleAddBodyCommand sim obj =
    case KM.lookup "body" obj of
        Just (Object bodyObj) ->
            let newBody = fromFrontEnd (Object bodyObj)
            in case newBody of
                Just body -> return (addBody sim body, statusOk)
                Nothing   -> return (sim, errorMsg "Invalid body format")
        Just _ -> return (sim, errorMsg "'body' must be an object")
        _ -> return (sim, errorMsg "'body' must be an object")

-- Helpers

sendError :: String -> IO ()
sendError msg = BL.putStrLn $ encode $ errorMsg msg

errorMsg :: String -> Value
errorMsg msg = object ["error" .= msg]

statusOk :: Value
statusOk = object ["status" .= ("ok" :: String)]
