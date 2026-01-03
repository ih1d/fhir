module Main (main) where

import Network.Wai.Handler.Warp (run)

import FHIR.Validation.Api (app)

main :: IO ()
main = do
    putStrLn "Starting FHIR Validation server on http://localhost:8080"
    run 8080 app
