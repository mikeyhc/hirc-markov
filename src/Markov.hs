module Markov where

import           Control.Applicative ((<$>))
import           Control.Arrow (first, second)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO

type Word = T.Text
type User = T.Text
type CountMap        = M.Map Word Int
type WordStartMap    = M.Map Word (Int, CountMap)
type MarkovDatabase  = M.Map User WordStartMap

data LogLine = LogLine
    { userName :: User
    , wordList :: [Word]
    }

updateDB :: LogLine -> MarkovDatabase -> MarkovDatabase
updateDB line = updateStart (userName line) (head $ wordList line)

updateStart :: User -> Word -> MarkovDatabase -> MarkovDatabase
updateStart u w db = let val = M.singleton w (1, M.empty)
                     in  M.insertWith (startCombine w) u val db

startCombine :: Word -> WordStartMap -> WordStartMap -> WordStartMap
startCombine w _ = M.update (Just . first (+1)) w

updateDBFromLog :: FilePath -> MarkovDatabase -> IO MarkovDatabase
updateDBFromLog logfile db = foldl (flip updateDB) db
                           <$> withFile logfile ReadMode parseLogFile

parseLogFile :: Handle -> IO [LogLine]
parseLogFile h = do
        ieof <- hIsEOF h
        if ieof then return []
        else do
            l <- T.hGetLine h
            if T.head l == '-' then parseLogFile h
            else do
                let nl        = T.drop 2 $ T.dropWhile (/= '<') l
                if T.head nl == '-' then parseLogFile h
                else do
                    let (nick, r) = second (T.drop 2) $ T.break (== '>') nl
                    (LogLine nick (T.words r):) <$> parseLogFile h
