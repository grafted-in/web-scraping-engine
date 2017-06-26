module Main (main, mainTest) where

import ScrapeEngine.Main (Options, mainWithArgs, parseArgs)

import           Control.Monad
import qualified System.Environment       as Env
import qualified System.Remote.Monitoring as Monitor

import qualified ScrapingRules as SR


main' :: Options -> IO ()
main' opts = mainWithArgs opts SR.startScraper (void . Monitor.forkServer "localhost")

main :: IO ()
main = main' =<< parseArgs =<< Env.getArgs

mainTest :: String -> IO ()
mainTest argStr = main' =<< parseArgs (words argStr)
