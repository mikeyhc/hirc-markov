{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses
             , FlexibleInstances #-}

module Main where

import           Control.Arrow (second)
import           Control.Applicative ((<$>))
import           Control.Exception
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import           HarkerIRC.Client
import           HarkerIRC.Types
import           Markov
import           System.Directory
import           System.Environment

newtype MarkovMonadT m a = MarkovMonad (StateT (FilePath, MarkovDatabase)
                                        (HarkerClientT m) a)
    deriving (Monad, MonadIO, Functor)
type MarkovMonad a = MarkovMonadT IO a

instance (Monad m) => MonadState (FilePath, MarkovDatabase) 
                                 (MarkovMonadT m) where
    get   = MarkovMonad   get
    put   = MarkovMonad . put
    state = MarkovMonad . state

instance MonadTrans MarkovMonadT where
    lift = MarkovMonad . lift . lift

instance HarkerClientMonad (MarkovMonadT IO) where
    clientLift = MarkovMonad . lift

runMarkovMonad :: FilePath -> MarkovMonad () -> IO ()
runMarkovMonad p (MarkovMonad s) = runHarkerClient (evalStateT s (p, M.empty))

main = do
    args <- getArgs
    if length args /= 1 then do
        putStrLn "usage: "
        putStrLn "    hirc-markov [logdir]"
    else runPlugin "markov" "0.1.0.0" markov (runMarkovMonad $ head args)

markov :: MarkovMonad ()
markov = do
    msg <- getMsg
    if msg == "!help"                then help
    else if "!nom " `isPrefixOf` msg then ifauth (nomLog (drop 5 msg))
    else if msg == "!vomit"          then ifauth vomit
    else if msg == "!nom-all"        then ifauth nomAll
    else when (msg == "!quote")     
              (gets snd >>= liftIO . generate
                      >>= \mx -> case mx of 
                            Just x -> sendReply $ T.unpack x
                            _      -> sendReply "No data to quote from")

vomit = modify (second (const M.empty))

help :: MarkovMonad ()
help = sendReply "!nom logfile: read the log file"
    >> sendReply "!vomit:       empty database"
    >> sendReply "!nom-all:     read the whole directory"
    >> sendReply "!quote:       generate a random quote"

nomAll :: MarkovMonad ()
nomAll = do
    fileList <- get 
        >>= \x -> liftIO (getDirectoryContents (fst x) `catch` errorCatch) 
    mapM_ nomLog fileList
  where
    errorCatch :: SomeException -> IO [String]
    errorCatch e = do
        putStrLn $ "Exception: " ++ show e 
        return []

nomLog :: String -> MarkovMonad ()
nomLog logname = do
    (p, db) <- get
    edb <- liftIO ((fmap Right 
           $! (updateDBFromLog $ p ++ "/" ++ takeLast logname) db)
           `catch` updateException logname)
    case edb of 
        Left msg -> sendReply msg
        Right d  -> do
            sendReply $ logname ++ " added"
            put (p, d)

updateException :: String -> SomeException 
                -> IO (Either String MarkovDatabase)
updateException file e = print e >> return  (Left $ "Error reading " ++ file)

takeLast = reverse . takeWhile (\x -> x /= '/' && x /= '\\') . reverse
