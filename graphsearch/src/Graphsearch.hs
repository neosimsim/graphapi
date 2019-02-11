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
  exists <- doesFileExist filePath
  unless exists $ do
    writeFile filePath $ result r
    putStrLn "result written"
  where
    filePath = joinPath [resultId r, "result"]

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

isModifiedEvent :: Event -> Bool
isModifiedEvent Modified {} = True
isModifiedEvent _        = False

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
  pid <- getProcessID
  _ <-
    watchDir mgr searchDir isAddedEvent $ \e -> do
      lockFileExists <-
        doesFileExist $ joinPath [searchDir, eventPath e, "lock"]
      unless lockFileExists $ do
        resultHandle <-
          openFile (joinPath [searchDir, eventPath e, "lock"]) WriteMode
        hPrint resultHandle pid
        hClose resultHandle
        let queryFile = joinPath [searchDir, eventPath e, "query"]
        waitForFile mgr queryFile
        queryString <- getContent queryFile
        putStrLn $ "got query " ++ queryString
        writeChan queries . Query (eventPath e) $ queryString
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
      isModifiedEvent $ \e -> do
        element <- getContent $ eventPath e
        putStrLn $ "read element " ++ element
        writeChan patches (NodePatch $ Created element)
  createDirectoryIfMissing True linksPath
  _ <-
    watchDir
      mgr
      linksPath
      isModifiedEvent $ \e -> do
        link <- getContent $ eventPath e
        putStrLn $ "read link " ++ link
        writeChan patches . EdgePatch $ Created link
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
