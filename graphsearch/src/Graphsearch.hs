{-# LANGUAGE OverloadedStrings #-}

module Graphsearch
  ( Graph
  , newGraph
  , Query(..)
  , Patch(..)
  , Result(..)
  , listenAndRespond
  , listenForGraphPatches
  , patchesFromDir
  , queriesFromDir
  , writeResult
  , loadGraph
  , GraphPatch(..)
  ) where

import           Control.Concurrent.Classy
import           Control.Monad
import           System.Directory
import           System.FilePath
import           System.FSNotify
import           System.IO
import qualified System.IO.Strict          as IOStrict
import           System.Posix

data Result = Result
  { resultId :: String
  , result   :: String
  } deriving (Eq, Show)

writeResult :: Result -> IO ()
writeResult r = do
  resultHandle <- openFile (joinPath [resultId r, "result"]) WriteMode
  hPrint resultHandle (result r)
  hClose resultHandle

data Query = Query
  { queryId :: String
  , query   :: String
  } deriving (Show)

data Graph = Graph
  { nodes :: [Node]
  , edges :: [Edge]
  } deriving (Show)

newGraph :: Graph
newGraph = Graph [] []

addNode :: Graph -> Node -> Graph
addNode g@Graph {nodes = ns} n = g {nodes = n : ns}

addEdge :: Graph -> Edge -> Graph
addEdge g@Graph {edges = ns} e = g {edges = e : ns}

type Node = String

getContent :: FilePath -> IO Node
getContent path = do
  fd <- openFile path ReadMode
  content <- IOStrict.hGetContents fd
  hClose fd
  return content

type Edge = String

data GraphPatch
  = NodePatch (Patch Node)
  | EdgePatch (Patch Edge)

newtype Patch a =
  Created a

isAddedEvent :: Event -> Bool
isAddedEvent Added {} = True
isAddedEvent _        = False

patchGraph :: Graph -> GraphPatch -> Graph
patchGraph g (EdgePatch (Created s)) = addEdge g s
patchGraph g (NodePatch (Created s)) = addNode g s

-- | wait until a the file is written.
-- TODO reproduce the deadlock fixed by commit 2d2c52,
-- in a test (dejafu)
waitForFile :: WatchManager -> FilePath -> IO ()
waitForFile mgr path = do
  lock <- newEmptyMVar
  putStrLn $ "Wait for " ++ path
  stop <-
    watchDir
      mgr
      (takeDirectory path)
      (isModified path)
      (\_ -> putMVar lock True)
  e <- doesFileExist path
  if e
    then putStrLn "File allready there"
    else do
      _ <- takeMVar lock
      putStrLn "File arrived"
  stop
  return ()
  where
    isModified p e@Modified {} = eventPath e == p
    isModified _ _             = False

-- queriesFromDir :: Chan IO Event -> FilePath -> IO (Chan IO Query)
queriesFromDir :: WatchManager -> FilePath -> IO (Chan IO Query)
queriesFromDir mgr path = do
  createDirectoryIfMissing True searchDir
  queries <- newChan
  _ <-
    watchDir mgr searchDir isAddedEvent $ \e -> do
      lockFileExists <-
        doesFileExist $ joinPath [searchDir, eventPath e, "lock"]
      unless lockFileExists $ do
        resultHandle <-
          openFile (joinPath [searchDir, eventPath e, "lock"]) WriteMode
        getProcessID >>= hPrint resultHandle
        hClose resultHandle
        let queryFile = joinPath [searchDir, eventPath e, "query"]
        waitForFile mgr queryFile
        handle <- openFile queryFile ReadMode
        IOStrict.hGetContents handle >>= writeChan queries . Query (eventPath e)
        hClose handle
  return queries
  where
    searchDir = joinPath [path, "queries"]

-- | watches in 'elements' and 'links' subdirs.
patchesFromDir :: WatchManager -> FilePath -> IO (Chan IO GraphPatch)
patchesFromDir mgr path = do
  patches <- newChan
  createDirectoryIfMissing True elementsPath
  _ <-
    watchDir
      mgr
      elementsPath
      isAddedEvent
      (writeChan patches . NodePatch . Created . takeBaseName . eventPath)
  createDirectoryIfMissing True linksPath
  _ <-
    watchDir
      mgr
      linksPath
      isAddedEvent
      (writeChan patches . EdgePatch . Created . takeBaseName . eventPath)
  return patches
  where
    elementsPath = joinPath [path, "elements"]
    linksPath = joinPath [path, "links"]

respond :: Graph -> Query -> Result
respond g q = Result (queryId q) (show g)

-- | Create an IORef to a Graph which is updated each time a new patch
-- is received.
listenForGraphPatches ::
     MonadConc m => Graph -> Chan m GraphPatch -> m (IORef m Graph, ThreadId m)
listenForGraphPatches graph patches = do
  graphRef <- newIORef graph
  threadId <-
    forkN "patchReader" . forever $ do
      p <- readChan patches
      g <- readIORef graphRef
      writeIORef graphRef $ patchGraph g p
  return (graphRef, threadId)

listenAndRespond ::
     MonadConc m
  => IORef m Graph
  -> Chan m Query
  -> m (Chan m Result, ThreadId m)
listenAndRespond graph queries = do
  results <- newChan
  threadId <-
    forkN "queryReader" . forever $ do
      q <- readChan queries
      g <- readIORef graph
      writeChan results $ respond g q
  return (results, threadId)

-- | load nodes from 'elements' and edges from 'links' directory
loadGraph :: FilePath -> IO Graph
loadGraph path = do
  elementFileNames <- listDirectory elementsRepoPath
  let elementFilePaths =
        map (\e -> joinPath [elementsRepoPath, e]) elementFileNames
  elements <- mapM getContent elementFilePaths
  let elementPatches = map (NodePatch . Created) elements
  linkFileNames <- listDirectory linksRepoPath
  let linkFilePaths = map (\e -> joinPath [linksRepoPath, e]) linkFileNames
  links <- mapM getContent linkFilePaths
  let linkPatches = map (EdgePatch . Created) links
  return $
    foldl patchGraph (foldl patchGraph (Graph [] []) elementPatches) linkPatches
  where
    elementsRepoPath = joinPath [path, "elements"]
    linksRepoPath = joinPath [path, "links"]
