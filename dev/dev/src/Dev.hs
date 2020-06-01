{-# language BlockArguments, ImplicitParams #-}
module Dev (module Dev, module Export, fromTxt, toTxt, i) where

import Pure.Data.Time as Export
import Pure.Data.Txt
import Pure.Data.Txt.Interpolate

import System.FSNotify hiding (Action)

import Data.Map.Strict as Map

import Control.Concurrent
import Control.Exception hiding (interruptible)
import Control.Monad
import Data.Char (isSpace)
import Data.Foldable
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath as Export
import System.Process
import System.IO
import System.IO.Unsafe
import Text.Printf

import System.FilePath.Glob as Export

import Prelude

import GHC.Stack
import Debug.Trace

type Name = (?name :: String)

name :: Name => String
name = ?name

type File = (?file :: String)

file :: File => String
file = ?file

dir :: File => String
dir = takeDirectory file

data Interrupt = NoInterrupt | Interrupt | InterruptWith ((File,Name) => IO ())

interruptible :: Interrupt -> Bool
interruptible NoInterrupt = False
interruptible _ = True

runInterrupt :: Interrupt -> ((File,Name) => IO ())
runInterrupt (InterruptWith f) = f
runInterrupt _ = pure ()

type Matcher = Name => Event -> Maybe (IO ())

data Action = Action Interrupt String Matcher

group :: String -> [Action] -> [Action]
group g = fmap (\(Action b nm f) -> Action b (g <> "." <> nm) f)

pattern Restartable :: String -> (Name => Event -> Maybe (IO ())) -> Action
pattern Restartable nm f = Action Interrupt nm f

pattern Interruptible :: String -> (Name => Event -> Maybe (IO ())) -> ((File,Name) => IO ()) -> Action
pattern Interruptible nm f g = Action (InterruptWith g) nm f

pattern Uninterruptible :: String -> (Name => Event -> Maybe (IO ())) -> Action
pattern Uninterruptible nm f = Action NoInterrupt nm f

first :: [a -> Maybe b] -> (a -> Maybe b)
first fs = \a -> listToMaybe $ catMaybes $ fs <*> pure a

emptyMatcher :: Matcher
emptyMatcher = first []

tracer :: (Name, File) => a -> a
tracer = traceShow (?name,?file)

infixr 0 |%
(|%) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|%) g f = let p = compile g in \ev ->
  case ev of
    Modified path _ _ | match p path -> Just (let ?file = path in f)
    _ -> Nothing

infixr 0 |+
(|+) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|+) g f = let p = compile g in \ev ->
  case ev of
    Added path _ _ | match p path -> Just (let ?file = path in f)
    _ -> Nothing

infixr 0 |-
(|-) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|-) g f = let p = compile g in \ev ->
  case ev of
    Removed path _ _ | match p path -> Just (let ?file = path in f)
    _ -> Nothing

infixr 0 |*
(|*) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|*) g f = let p = compile g in \ev ->
  case ev of
    Removed path _ _ | match p path -> Just (let ?file = path in f)
    Added   path _ _ | match p path -> Just (let ?file = path in f)
    _                               -> Nothing

infixr 0 |$
(|$) :: Name => String -> (File => IO ()) -> (Event -> Maybe (IO ()))
(|$) g f = let p = compile g in \ev ->
  case ev of
    _ | match p (eventPath ev) -> Just (let ?file = eventPath ev in f)
      | otherwise              -> Nothing

defaultMain :: HasCallStack => FilePath -> [Action] -> IO ()
defaultMain d as = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "Watching files."
  withManagerConf defaultConfig { confDebounce = Debounce (realToFrac (0.5 :: Double)) } $ \mgr -> do
    actions <- newMVar Map.empty
    cd <- getCurrentDirectory
    watchTree mgr d (const True) $ \(mapEventPath (makeRelative cd) -> ev) ->
      for_ as $ \(Action interrupt nm f) -> let { ?name = nm; ?file = eventPath ev } in
        for_ (f ev) $ \g ->
          let
            run interrupted g =
              forkIOWithUnmask $ \unmask -> do
                when interrupted (runInterrupt interrupt)
                unmask $ do
                  mtid <- myThreadId
                  catch @SomeException g (\_ -> pure ())
                  modifyMVar_ actions $ \case
                    as
                      | Just (_,Just x) <- Map.lookup nm as -> do
                        tid <- x
                        pure $! Map.insert nm (tid,Nothing) as
                      | Just (tid,Nothing) <- Map.lookup nm as
                      , tid == mtid ->
                        pure $! Map.delete nm as
                      | otherwise -> 
                        error "Invariant broken: self no longer exists in actions map"
                        -- just to make sure this is all right

          in
            modifyMVar_ actions $ \case
              as
                -- running and interruptible
                | Just (tid,_) <- Map.lookup nm as
                , interruptible interrupt -> do
                  -- will block if the intterupt handler is being 
                  -- executed within a masked fork in `run`
                  message "Killing"
                  killThread tid 
                  pure $! Map.insert nm (tid,Just (run True g)) as

                -- running and not interruptible
                | Just (tid,_) <- Map.lookup nm as ->
                  -- Note: putting `run g` in here instead of `g` because
                  -- the action needs `(Name,File)` constraint satisfied locally
                  pure $! Map.insert nm (tid,Just (run False g)) as

                -- not running
                | otherwise -> do
                  tid <- run False g 
                  pure $! Map.insert nm (tid,Nothing) as

    forever (threadDelay 1000000)

mapEventPath :: (FilePath -> FilePath) -> Event -> Event
mapEventPath f ev =
  case ev of
    Added    path t b -> Added    (f path) t b
    Modified path t b -> Modified (f path) t b
    Removed  path t b -> Removed  (f path) t b
    Unknown  path t s -> Unknown  (f path) t s

{-# NOINLINE output #-}
output :: MVar (Map String String,Map String String)
output = unsafePerformIO (newMVar (Map.empty,Map.empty))

writeOutput :: Map String String -> Map String String -> IO ()
writeOutput (synopsis -> ss) ms = do
  for_ ms putStrLn
  putStrLn ss
  hFlush stdout

synopsis :: Map String String -> String
synopsis ms
  | Map.null ms                 = '\x1F7E1' : " running"
  | Data.Foldable.all isGood ms = "\ESC[2J" ++ ('\x1F7E2' : " all good")
  | otherwise = Prelude.unlines (Map.elems (Map.filter (not . isGood) ms))
  where
    isGood ('\x1F7E2' : _) = True
    isGood _ = False

data Status
  = Good String
  | Running String
  | Bad String

status :: Name => Status -> IO ()
status s =
  modifyMVar_ output $ \(Map.insert ?name (s' s) -> !ss,ms) ->
    writeOutput ss ms >> pure (ss,ms)
  where
    s' (Good    msg) = '\x1F7E2' : (" <" <> ?name <> "> " <> msg)
    s' (Bad     msg) = '\x1F534' : (" <" <> ?name <> "> " <> msg)
    s' (Running msg) = '\x1F7E1' : (" <" <> ?name <> "> " <> msg)

message :: Name => String -> IO ()
message m =
  modifyMVar_ output $ \(ss,Map.insert ?name msg -> !ms) ->
    writeOutput ss ms >> pure (ss,ms)
  where
    !msg = Prelude.unlines $ fmap (("<" <> ?name <> "> ") <>) (Prelude.lines $ trim m)

appendMessage :: Name => String -> IO ()
appendMessage m = do
  modifyMVar_ output $ \(ss,Map.insertWith (flip (++)) ?name msg -> !ms) ->
    writeOutput ss ms >> pure (ss,ms)
  where
    !msg = Prelude.unlines $ fmap (("<" <> ?name <> "> ") <>) (Prelude.lines $ trim m)

clear :: Name => IO ()
clear =
  modifyMVar_ output $ \(ss,Map.delete ?name -> !ms) ->
    writeOutput ss ms >> pure (ss,ms)

spawnSilent :: String -> IO ExitCode
spawnSilent s = do
  p@(_,_,_,ph) <- createProcess_ "" (shell s)
    { std_in  = NoStream -- can't prompt for input, anyways
    , std_out = NoStream
    , std_err = NoStream
    }
  (`finally` (cleanupProcess p)) 
    (waitForProcess ph)

spawn :: String -> IO ExitCode
spawn s = do
  p@(_,Just outh,Just errh,ph) <- createProcess_ "" (shell s)
    { std_in  = NoStream -- this approach is not designed for interactive processes
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  (`finally` (cleanupProcess p)) $ do
    hSetBuffering outh LineBuffering
    hSetBuffering errh LineBuffering
    let pipe i o = (`finally` (hClose i)) (hGetContents i >>= traverse_ (hPutStrLn o) . Prelude.lines)
    forkIO (pipe outh stdout)
    forkIO (pipe errh stderr)
    waitForProcess ph

pattern Success :: ExitCode
pattern Success <- ExitSuccess

pattern Failure :: ExitCode
pattern Failure <- ExitFailure ((/= 15) -> True)

pattern Restarted :: ExitCode
pattern Restarted <- ExitFailure (-15)

withDuration :: (IO String -> IO a) -> IO a
withDuration f = do
  start <- time
  f $ do
    end <- time

    let
      Seconds ss (Milliseconds ms _) = end - start

      dur | 0 <- ms   = show ss <> " seconds"
          | otherwise = show ss <> "." <> printf "%03d" ms <> " seconds"

    dur `seq` pure dur

trim :: String -> String
trim = process . process
  where
    process = Prelude.reverse . Prelude.dropWhile (== '\n')
