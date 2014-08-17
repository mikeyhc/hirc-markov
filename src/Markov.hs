{-# LANGUAGE OverloadedStrings #-}

module Markov 
    ( Word
    , MarkovDatabase
    , generate
    , updateDB
    , updateDBFromLog
    ) where

import           Control.Applicative ((<$>))
import           Control.Arrow (first, second)
import           Control.Exception (catch, SomeException)
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Random as R
import qualified Data.Random.Distribution.Categorical as Cat
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import           System.IO

type Word            = T.Text
type CountMap        = M.Map Word Int
type MarkovDatabase  = M.Map Word (Int, CountMap)


generate' :: MarkovDatabase -> R.RVar T.Text
generate' db = do
    l <- go (drawFrom . mapMaybe onlyStarts $ M.toList db) db 
    return $ T.intercalate " " l
  where
    onlyStarts (k, (c, _)) = if c > 0 then Just (fromIntegral c, k) 
                                      else Nothing
    go :: R.RVar Word -> MarkovDatabase -> R.RVar [Word]
    go rword db = do
        word <- rword
        if word == "." 
        then return []
        else do
            let list = map (\(w, c) -> (fromIntegral c, w))
                     . M.toList
                     . fromMaybe M.empty
                     . fmap snd $ M.lookup word db
            case list of 
                [] -> return []
                _  -> (word:) <$> go (drawFrom list) db

generate :: MarkovDatabase -> IO T.Text
generate db = R.runRVar (generate' db) R.StdRandom

drawFrom :: [(Double, Word)] -> R.RVar Word
drawFrom = R.rvar . Cat.fromList 

updateDB :: [Word] -> MarkovDatabase -> MarkovDatabase
updateDB l = updateDB' l . updateStart (head l)

updateStart :: Word -> MarkovDatabase -> MarkovDatabase
updateStart w = M.insertWith (\_ o -> first (+1) o) w (1, M.empty)

updateDB' :: [Word] -> MarkovDatabase -> MarkovDatabase
updateDB' []       db = db
updateDB' [x]      db = M.insertWith (\_ o -> second (updateCount ".") o) x
                                     (0, M.singleton "." 1) db
updateDB' (x:y:xs) db = updateDB' (y:xs) 
                      $ M.insertWith (\_ o -> second (updateCount y) o) x 
                                     (0, M.singleton y 1) db

updateCount :: Word -> CountMap -> CountMap
updateCount w = M.insertWith (const (+1)) w 0

updateDBFromLog :: FilePath -> MarkovDatabase -> IO MarkovDatabase
updateDBFromLog logfile db = foldl (flip updateDB) db
                           <$> withFile logfile ReadMode parseLogFile
                           `catch` (const (return []) 
                                    :: SomeException -> IO [[Word]])

parseLogFile :: Handle -> IO [[Word]]
parseLogFile h = do
        ieof <- hIsEOF h
        if ieof then return []
        else do
            l <- T.hGetLine h
            if T.head l == '-' then parseLogFile h
            else do
                let nl        = clean . T.drop 2 $ T.dropWhile (/= '<') l
                if T.head nl == '-' then parseLogFile h
                else do
                    let r = T.drop 2 $ T.dropWhile (/= '>') nl
                        l = cleanWords $ T.words r
                    case l of
                        [] -> parseLogFile h
                        _  -> (l:) <$> parseLogFile h

clean :: T.Text -> T.Text
clean = T.filter (\x -> x /='"' && x /= '\'' && x /= ';' && x /= '\\')

cleanWords :: [T.Text] -> [T.Text]
cleanWords = filter (\x -> not $ "http://" `T.isPrefixOf` x
                        && T.head x /= '!') 
