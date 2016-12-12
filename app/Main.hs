module Main where

import System.Environment (getArgs)
import RunServer (startApp)

main :: IO ()
main = getArgs >>= startApp
