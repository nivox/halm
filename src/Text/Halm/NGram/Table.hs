-- | A table of n-grams providing occurrences counters. Given a maximum
--   order /n/ counters are provided for each m-gram with /m/ <= /n/.
module Text.Halm.NGram.Table
    (
     -- * NGram Table type
     NGramTable

    -- * Creation/Update functions
    , empty
    , computeF
    , extend
    , extendF

    -- * Query functions
    , uniques
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Debug.Trace (trace)
import System.IO (Handle, IOMode(ReadMode), hIsEOF, withFile)


import qualified Text.Halm.NGram.Trie as NGT
import Text.Halm.Dictionary (IndexedDictionary, UnitId, indexOf)


-- | Main NGram Table data type.
data NGramTable = NGramTable {
      dict :: IndexedDictionary
    , root :: NGT.TrieNode Integer
    } deriving (Show)

-- | Empty NGram table based on the specified dictionary.
empty :: IndexedDictionary -> NGramTable
empty d = NGramTable d NGT.empty

-- | Token identifying the start/end of a sentence (i.e. context boundary).
ctxBoundary :: Text
ctxBoundary = T.pack "<s>"

-- | Converts a text token into an unit id taking care of reserved symbols
--   and Out Of Vocabolary words.
unitsToId :: Text -> IndexedDictionary -> UnitId
unitsToId t d = fromMaybe (getId t) (indexOf t d)
    where
      getId :: Text -> UnitId
      getId t | t == ctxBoundary = -1
              | otherwise = -2

-- | Updates the count for the specified ngram
countNGram :: [Text] -> NGramTable -> NGramTable
countNGram us tbl =
    let uids = map (flip unitsToId $ dict tbl) us
        root' = NGT.updateWith incCount uids (root tbl)
    in root' `seq` tbl { root = root'}
    where
      incCount :: Int -> UnitId -> Maybe Integer -> Integer
      incCount _ _ (Just occ) = occ `seq` occ + 1
      incCount _ _ Nothing = 1

-- | Extend an already computed NGram Table with the specifed text using the
--   dictionary specified at the time of the creation of the table. The
--   second argument specifies the order of ngram to compute.
--   The text is considered a full sentence with context breaks at the start
--   and end.
extend :: Text -> Int -> NGramTable -> NGramTable
extend txt n tbl =
    let tokens = T.words txt
        startNGram = replicate n ctxBoundary
    in fst $ tbl `seq` foldl' countNGram' (tbl, startNGram) (tokens ++ [ctxBoundary])
    where
      countNGram' :: (NGramTable, [Text]) -> Text -> (NGramTable, [Text])
      countNGram' (tbl, ngram) unit =
          let ngram' = (tail ngram) ++ [unit]
              tbl' = countNGram ngram' tbl
          in (tbl', ngram')

-- | Extend an already computed NGram table with the text read from the
--   specified file using the dictionary specified at the time of the creation
--   of the table. The second argument specifies the order of ngram to compute.
extendF :: FilePath -> Int -> NGramTable -> IO NGramTable
extendF f n tbl = tbl `seq` withFile f ReadMode $ parseFile n tbl
    where
      parseFile :: Int -> NGramTable -> Handle -> IO NGramTable
      parseFile n tbl h = do
        eof <- hIsEOF h
        case eof of
          True -> return tbl
          False -> do
                   l <- TIO.hGetLine h
                   let tbl' = extend l n tbl
                   tbl' `seq` parseFile n tbl' h

-- | Computers the NGram table of the specified file using the supplied
--   dictioanry. The second argument specifies the order of ngram to compute.
computeF :: FilePath -> Int -> IndexedDictionary -> IO NGramTable
computeF f order id = extendF f order $ empty id

uniques :: NGramTable -> [Int]
uniques (NGramTable _ tr) = M.elems $ NGT.fold updateCounts M.empty tr
    where
      updateCounts :: M.Map Int Int -> [UnitId] -> NGT.TrieNode a -> M.Map Int Int
      updateCounts m uids node = let l = length uids
                                 in M.insertWith (+) l 1 m