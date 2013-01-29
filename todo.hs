{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time
import Data.Text hiding (zip)
import System.IO
import System.Environment

import Control.Monad
import Data.List
import Prelude hiding (zip)
import Control.Exception

import System.IO.Error (isDoesNotExistErrorType, ioeGetErrorType)
import System.Directory
import System.FilePath

data TItem = TItem { createdAt :: Data.Time.UTCTime
                   , deadline  :: Data.Time.UTCTime
                   , priority  :: Int
                   , done      :: Bool
                   , contents  :: String }
     deriving (Show, Read)

filename_ = ".mytodo"

showHelp = putStrLn $ "usage: todo [OPTS]\nwhere OPTS are:\n"
        ++ "  help        -- shows this message\n"
        ++ "  add TEXT    -- adds task\n"
        ++ "  done IDX    -- marks task as done\n"
        ++ "  delete IDX  -- permanently deletes task\n"

handleJustAndRerun :: Exception e => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJustAndRerun p h f = handleJust p (\ b -> h b >> f) f

main = do
  args <- getArgs
  dir_ <- getHomeDirectory
  let filename = joinPath [dir_, filename_]
  handleJustAndRerun (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
    (\_ -> print "creating"  >> openFile filename WriteMode >>= hClose >> return ()) $
    case args of
      ["help"]        -> showHelp
      ["add", text]   -> addTItem text filename
      ["done", idx]   -> setDone (read idx :: Int) filename
      ["delete", idx] -> removeTItem (read idx :: Int) filename
      []              -> displayTItems filename
      _               -> showHelp

  return ()

getItems file = do
  s <- hFileSize file
  if s == 0 then return []
  else do
    c <- hGetContents file
    return $! read c :: IO [TItem]


addTItem text filename = do
  d <- withFile filename ReadMode getItems
  t <- getCurrentTime
  let newdata = TItem t t 7 False text : d
  withFile filename WriteMode $ \h -> hPutStrLn h $ show newdata

displayTItems filename = do
  d <- withFile filename ReadMode getItems
  let sorted = sortBy (\x y -> compare (deadline x) (deadline y)) d
  forM_ (zip [0..] sorted) $
    \(idx, d) -> putStrLn $ show idx ++ ": "
                         ++ show (deadline d) ++ "\t"
                         ++ show (done d) ++ "\n\t"
                         ++ (contents d) ++ "\n"

setDone idx filename = do
  d <- withFile filename ReadMode getItems
  let sorted  = sortBy (\x y -> compare (deadline x) (deadline y)) d
  let newdata = (Prelude.take idx sorted)
             ++ [(sorted!!idx) {done = True}]
             ++ (Prelude.drop (idx + 1) sorted)
  withFile filename WriteMode $ \h -> hPutStrLn h $ show newdata

removeTItem idx filename = do
  d <- withFile filename ReadMode getItems
  let sorted = sortBy (\x y -> compare (deadline x) (deadline y)) d
  let newdata = (Prelude.take idx sorted) ++ (Prelude.drop (idx + 1) sorted)
  withFile filename WriteMode $ \h -> hPutStrLn h $ show newdata
