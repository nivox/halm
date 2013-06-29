module Text.Halm.Dictionary
    (
     -- * Dictionary creation/manipulation
     Dictionary
    , empty
    , compute
    , computeF
    , extend
    , extendF
    , size
    , wlist

    -- * Indexed dictionary creation/query
    , IndexedDictionary
    , UnitId
    , index
    , uniques
    , total

    -- * Indexed dictionary entry query
    , indexOf
    , occurOf
    ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.HashMap.Strict (HashMap, insertWith)
import Data.List (foldl', sortBy)
import Data.Map (Map)
import Data.Text (Text)
import Data.Tuple (swap)
import Debug.Trace (trace)
import System.IO (Handle, IOMode(ReadMode), hIsEOF, withFile)

-- | An extendable dictionary data type, mapping words to their occurrence.
type Dictionary = HashMap Text Integer

-- | Type used to represent the identifier of a unit
type UnitId = Integer

-- | Entry of a IndexedDictionary.
--   Keeps track of the index of the word alongside its occurrences.
data IndexedEntry = IndexedEntry {
      ieIndex :: UnitId
    , ieOccurrence :: Integer
    } deriving (Show)

-- | A static (i.e. cannot be extended) index on a dictionary data type.
data IndexedDictionary = IndexedDictionary {
      uniques :: Integer
    , total :: Integer
    , dict :: (HashMap Text IndexedEntry)
    } deriving (Show)

-- | Creates an empty dictionary.
empty :: Dictionary
empty = HM.empty

-- | Updates the dictionary.
countWord :: Dictionary -> Text -> Dictionary
countWord d t = insertWith (+) t 1 d

-- | Insert a single word to the dictionary.
--   If the word is already present the corresponding occurrence
--   is increased, otherwise the word is added with occurrence 1.
insertWord :: Text -> Dictionary -> Dictionary
insertWord t d = countWord d t

-- | Compute the dictionary of the text string
compute :: Text -> Dictionary
compute txt = extend txt empty

-- | Computes the dictionary of the specified file
computeF :: FilePath -> IO Dictionary
computeF f = extendF f empty

-- | Extends an existing dictionary with the specified text
extend :: Text -> Dictionary -> Dictionary
extend txt d =
    foldl' countWord d $ T.words txt

-- | Extends an existing dictionary with the text read from the specified file
extendF :: FilePath -> Dictionary -> IO Dictionary
extendF f d = withFile f ReadMode $ parseFile d
    where
      parseFile :: Dictionary -> Handle -> IO Dictionary
      parseFile d h = do
        eof <- hIsEOF h
        case eof of
          True -> return d
          False -> do
                   l <- TIO.hGetLine h
                   let d' = (extend l d)
                   d' `seq` parseFile d'  h

-- | Return the size of the dictionary
size :: Dictionary -> Int
size = HM.size

-- | Return the words forming the dictionary
wlist :: Dictionary -> [Text]
wlist = HM.keys

-- | Dictionary a dictionary based on descending words occurrence
index :: Dictionary -> IndexedDictionary
index = index' . toDescList

    where index' :: [(Text, Integer)] -> IndexedDictionary
          index' d =
              let dict = HM.fromList . (map mkDictEntry) . (zip [1..]) $ d
                  (uniques, total) = countWords dict
              in IndexedDictionary uniques total dict

          mkDictEntry :: (Integer, (Text, Integer)) -> (Text, IndexedEntry)
          mkDictEntry (i, (wrd, occ)) = (wrd, IndexedEntry i occ)

          toDescList :: Dictionary -> [(Text, Integer)]
          toDescList = sortBy (flip compare) . HM.toList

-- | Counts the unique and total words in an IndexedDictionary dict field
countWords :: HashMap Text IndexedEntry -> (Integer, Integer)
countWords = (foldl' countEntry (0,0)) . HM.elems
    where
      countEntry :: (Integer, Integer) -> IndexedEntry -> (Integer, Integer)
      countEntry (u, t) e = u `seq` t `seq` (u+1, t+(ieOccurrence e))

-- | Returns a Maybe value containing the index of the word in the dictionary
indexOf ::  Text -> IndexedDictionary -> Maybe Integer
indexOf t d = fmap ieIndex $ HM.lookup t (dict d)

-- | Returns a Maybe value containing the occurrence of the word in the dictionary
occurOf :: Text -> IndexedDictionary -> Maybe Integer
occurOf t d = fmap ieOccurrence $ HM.lookup t (dict d)