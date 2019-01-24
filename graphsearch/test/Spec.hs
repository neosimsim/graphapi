module Main
  ( main
  ) where

import           Control.Concurrent.Classy
import           Control.Monad
import           Graphsearch
import           System.Exit
import qualified Test.DejaFu               as DejaFu
import qualified Test.QuickCheck           as QuickCheck

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  results <- test
  unless (all QuickCheck.isSuccess results) exitFailure
  DejaFu.dejafu
    "patches and queries shall not deadlock"
    DejaFu.deadlocksNever
    testListenAndRespond >>=
    (`unless` exitFailure)

test :: IO [QuickCheck.Result]
test = sequence []

testListenAndRespond :: MonadConc m => m [Result]
testListenAndRespond = do
  graph <- newIORef newGraph
  patchChan <- newChan
  queryChan <- newChan
  _ <-
    forkN "patchWriter" $ do
      writeChan patchChan . EdgePatch $ Created "patch 1"
      writeChan patchChan . EdgePatch $ Created "patch 2"
  _ <-
    forkN "patchWriter" $ do
      writeChan patchChan . EdgePatch $ Created "patch 3"
      writeChan patchChan . NodePatch $ Created "patch 4"
  _ <-
    forkN "queryWriter" $ do
      writeChan queryChan $ Query "1" "query 1"
      writeChan queryChan $ Query "2" "query 2"
  _ <-
    forkN "queryWriter" $ do
      writeChan queryChan $ Query "3" "query 3"
      writeChan queryChan $ Query "4" "query 4"
  (responseChan, _) <- listenAndRespond graph queryChan
  replicateM 4 $ readChan responseChan
