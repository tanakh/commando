{-# LANGUAGE RankNTypes, ImpredicativeTypes, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module System.Console.CmdLine (
  CmdLine,
  runCmdLine,

  opt, dispatch, sub,
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Control.Monad.State
import Data.IORef
import Data.Lens
import Data.Lens.Template
import Data.List
import Data.Maybe
import Data.Typeable
import System.Environment
import System.Exit
import System.IO.Unsafe

data Option where
  Option :: (Typeable a, Show a) =>
    { optFullName  :: Maybe String
    , optShortName :: Maybe Char
    , optDefault   :: Maybe a
    , optValue     :: IORef (Maybe a)
    , optReader    :: String -> Maybe a
    , optHelp      :: Maybe String
    } -> Option

-- deriving instance Show Option
instance Show Option where
  show _ = "<option>"

data Command
  = Command
    { cmdName        :: String
    , cmdDescription :: Maybe String
    , cmdOptions     :: [Option]
    , cmdDispatch    :: IO ()
    , cmdSubs        :: [Command]
    }
  | DispatchExternal
nameMakeLens ''Command (Just . (++"L"))

instance Show Command where
  show Command {..} =
    "{" ++ show cmdOptions ++ ", " ++ show cmdDescription ++
    unwords (map show cmdSubs) ++ "}"

printUsage :: Command -> [String] -> IO ()
printUsage Command {..} [] = do
  putStrLn $ "Usage: " ++ cmdName ++ " [OPTIONS]... [FILE]..."
  when (isJust cmdDescription) $ do
    putStrLn . fromJust $ cmdDescription
  putStrLn ""

  tbl <- forM cmdOptions $ \Option {..} -> do
    let flag = case (optShortName, optFullName) of
          (Just sn, Just fn) -> "-" ++ [sn] ++ ", --" ++ fn
          (Just sn, Nothing) -> "-" ++ [sn]
          (Nothing, Just fn) -> "    --" ++ fn
          _ -> assert False undefined
    return [flag, fromMaybe "" optHelp]

  renderTable tbl

printUsage _ _ = assert False undefined

renderTable :: [[String]] -> IO ()
renderTable rows = do
  forM_ rows $ \row -> do
    forM_ (zip rowWidth row) $ \(width, col) -> do
      putStr $ "  " ++ take width (col ++ repeat ' ')
    putStrLn ""
  where
    rowWidth = map (maximum . map length) $ transpose rows

data Arg
  = Arg
    { argName  :: String
    , argValue :: Maybe String
    }
  deriving (Show)

data Args
  = Args     [String] [Arg] [String]
  | External [String] [String]
  deriving (Show)

substitute :: (MonadError String m, MonadIO m)
              => Command -> Args -> m ()
substitute Command {..} (Args subNames args rest) = do
  forM_ args $ \Arg {..} -> do
    case find ((== Just argName) . optFullName) cmdOptions of
      Nothing ->
        throwError $ "unrecognized option '--" ++ argName ++ "'"
      Just (Option {..}) -> do
        case argValue of
          Just v -> do
            case optReader v of
              Just x ->
                liftIO $ writeIORef optValue $ Just x
              Nothing ->
                throwError $ "invalid --" ++ argName ++ " argument '"
                             ++ v ++ "'"
          Nothing -> do
            assert False undefined

substitute _ _ = assert False undefined

runCommand :: Command -> Args -> IO ()
runCommand Command {..} (Args [] _ _) = cmdDispatch
runCommand cmd (Args (s:ss) x y) =
  case find ((== s) . cmdName) $ cmdSubs cmd of
    Just scmd -> runCommand scmd $ Args ss x y
    Nothing -> assert False undefined

data CmdLineState
  = CmdLineState
    { clHeader  :: String
    , clCommand :: Command
    }
  deriving (Show)
nameMakeLens ''CmdLineState (Just . (++ "L"))

initCmdLineState :: IO CmdLineState
initCmdLineState = do
  progName <- getProgName
  return $ CmdLineState
    { clHeader = ""
    , clCommand = Command
      { cmdName = progName
      , cmdDescription = Nothing
      , cmdOptions = []
      , cmdDispatch = assert False undefined
      , cmdSubs = []
      }
    }

newtype CmdLine a
  = CmdLine { unCmdLine :: StateT CmdLineState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CmdLineState)

runCmdLine :: CmdLine a -> IO a
runCmdLine cmdline = do
  (ret, cmd@CmdLineState {..}) <- do
    ini <- initCmdLineState
    (`runStateT` ini) $ unCmdLine $ do
      opt "help" (Just '?') (Just False)
        (Just "give this help list")
      opt "version" (Just 'V') (Just False)
        (Just "print program version")
      cmdline

  print cmd
  -- TODO: Sanity Check of cmd

  args <- getArgs
  print args
  earg <- parse clCommand <$> return args
  print earg

  rarg <- case earg of
    Left errorMsg -> do
      putStrLn errorMsg
      printUsage clCommand []
      exitFailure
    Right arg -> do
      when (hasHelp arg) $ do
        printUsage clCommand []
        exitSuccess
      return arg

  e <- runErrorT $ substitute clCommand rarg
  case e of
    Left err -> do
      putStrLn err
      exitFailure
    Right () ->
      return ()
  -- TODO: sanity check

  runCommand clCommand rarg

  return ret

  where
    hasHelp (Args _ aa _) = "help" `elem` map argName aa
    hasHelp _ = False

sub :: String -> CmdLine a -> CmdLine a
sub name m = assert False undefined

opt :: (Typeable a, Read a, Show a)
       => String -> Maybe Char -> Maybe a -> Maybe String -> CmdLine a
opt name sname defv help = do
  ior <- liftIO $ newIORef defv
  CmdLine $ focus clCommandL $ cmdOptionsL %=
    (Option (Just name) sname defv ior rdr help :)
  return $ fromJust $ unsafePerformIO $ readIORef ior
  where
    rdr s = case find (null . snd) $ reads s of
      Just (r, _) -> Just r
      _ -> Nothing

parse :: Command -> [String] -> Either String Args
parse = subp where
  subp _ [] =
    return $ Args [] [] []

  subp DispatchExternal xs =
    return $ External [] xs

  subp c (x:xs) =
    case find ((== x) . cmdName) (cmdSubs c) of
      Just subCmd -> do
        e <- subp subCmd xs
        case e of
          Args subs args rest ->
            return $ Args (x:subs) args rest
          External subs args ->
            return $ External (x:subs) args
      Nothing ->
        uncurry (Args []) <$> cmd (cmdOptions c) (x:xs)

  cmd opts = go where
    go ls = case ls of
      [] ->
        return ([], [])
      (x:xs)
        | "--" `isPrefixOf` x ->
          case span (/='=') $ drop 2 x of
            (name, _) | not (isExist name) ->
              throwError $ "unrecognized option '--" ++ name ++ "'"
            (name, '=':val) ->
              addArgs (Arg name $ Just val) <$> go xs
            (name, _) | hasOption name -> case xs of
              y:ys ->
                addArgs (Arg name $ Just y) <$> go ys
              _ ->
                throwError $ "option '--" ++ name ++  "' requires an argument"
            (name, _) ->
              addArgs (Arg name Nothing) <$> go xs
        | "-" `isPrefixOf` x ->
          assert False undefined
        | otherwise ->
          addRest x <$> go xs

    addArgs x (args, rest) = (x:args, rest)
    addRest x (args, rest) = (args, x:rest)

    isExist name = any ((== Just name) . optFullName) opts
    hasOption name =
      case find ((== Just name) . optFullName) opts of
        Nothing -> False
        Just (Option {..}) ->
          isNothing (cast (fromJust optDefault) :: Maybe Bool)

dispatch :: IO () -> CmdLine ()
dispatch m =
  CmdLine $ void $ focus clCommandL $ cmdDispatchL ~= m
