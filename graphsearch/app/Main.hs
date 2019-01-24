module Main
  ( main
  ) where

import           Control.Concurrent.Classy
import           Control.Monad
import           Data.Semigroup            ((<>))
import           Graphsearch
import           Options.Applicative
import           System.FSNotify

newtype Options = Options
  { repoDir :: String
  }

main :: IO ()
main = do
  o <- execParser opts
  withManager $ \mgr -> do
    patches <- patchesFromDir mgr $ repoDir o
    g <- loadGraph $ repoDir o
    (graph, _) <- listenForGraphPatches g patches
    queries <- queriesFromDir mgr $ repoDir o
    (results, _) <- listenAndRespond graph queries
    forever $ readChan results >>= writeResult
  where
    opts =
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "graph search deamon" <> header "graphsearch")

optionsParser :: Parser Options
optionsParser =
  Options <$>
  strOption
    (short 'r' <> metavar "REPO_DIR" <> help "the JSON graph repository")
