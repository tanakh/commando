{-# LANGUAGE GADTs, StandaloneDeriving, RankNTypes #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.Console.CmdLine

disp :: (a -> IO ()) -> (a -> CmdLine ())
disp m x = dispatch $ m x

main :: IO ()
main = runCmdLine $ do
  join $ disp foo <$> opt "arg" (Just 'a') (Just 123) (Just "hoge")

foo :: Int -> IO ()
foo x = putStrLn $ "*** " ++ show x ++ " ***"
