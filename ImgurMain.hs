module Main where

import Control.Concurrent
import Control.Monad

import Network.HTTP

import System.Exit
import System.Environment

import System.IO.Unsafe

import Data.Char

main = do
  args <- getArgs
  let [chars] = args
  let tests = perms chars
  results <- forM tests $ \test -> nonBlocking $ do
    let url = "http://imgur.com/" ++ test
    Right resp <- simpleHTTP (getRequest url)
    case (rspCode resp) of
      (2, 0, 0) -> return (True, url)
      _ -> return (False, "")
  let all = map snd . filter fst $ results
  forM_ all putStrLn

perms "" = [""]
perms (x:xs) = case isAlpha x of
  False -> map (x:) (perms xs)
  True -> do
    y <- [toLower x, toUpper x]
    ys <- perms xs
    return (y:ys)

nonBlocking act = do
  var <- newEmptyMVar
  forkIO $ do
    ret <- act
    putMVar var ret
    return ()
  unsafeInterleaveIO (takeMVar var)

