module Text.Halm.Dictionary (
                             -- Dictionary creation/manipulation
                             empty
                            , computeDictionary
                            , extendDictionary

                            -- Indexed dictionary creation/query
                            , indexDictionary
                            , uniques
                            , total

                            -- Indexed dictionary entry query
                            , indexOf
                            , occurOf
                            ) where

import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap, insertWith)

import qualified Data.Map.Strict as M
import Data.Map (Map)

import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (Text)

import Data.Tuple (swap)

-- | An extendable dictionary data type, mapping
-- | words to their occurrence
type Dictionary = HashMap Text Integer

-- | Entry of a IndexedDictionary.
-- | Keeps track of the index of the word alongside its occurrences
data IndexedEntry = IndexedEntry {
      ieIndex :: Integer
    , ieOccurrence :: Integer
    } deriving (Show)

-- | A static (i.e. cannot be extended) index on a dictionary data type
data IndexedDictionary = IndexedDictionary {
      uniques :: Integer
    , total :: Integer
    , dict :: (HashMap Text IndexedEntry)
    } deriving (Show)

-- | Creates an empty dictionary
empty :: Dictionary
empty = HM.empty

-- | Updates the dictionary
countWord :: Dictionary -> Text -> Dictionary
countWord d t = insertWith (+) t 1 d

-- | Insert a single word to the dictionary.
-- | If the word is already present the corresponding occurrence
-- | is increased, otherwise the word is added with occurrence 1
insertWord :: Text -> Dictionary -> Dictionary
insertWord t d = countWord d t

-- | Compute the dictionary of the lazy text string
computeDictionary :: Text -> Dictionary
computeDictionary txt = extendDictionary txt empty

-- | Extends an existing dictionary with the specified text
extendDictionary :: Text -> Dictionary -> Dictionary
extendDictionary txt d = let tokens = TL.words txt in
                         foldl countWord d tokens

-- | Dictionary a dictionary based on descending words occurrence
indexDictionary :: Dictionary -> IndexedDictionary
indexDictionary d = indexDictionary' . flipDictionary $ d where

    indexDictionary' :: Map Integer Text -> IndexedDictionary
    indexDictionary' d =
        let dict = HM.fromList . (map mkDictEntry) . (zip [1..]) . M.toDescList $ d
            (uniques, total) = countWords dict
        in IndexedDictionary uniques total dict

    countWords :: HashMap Text IndexedEntry -> (Integer, Integer)
    countWords = let countEntry = \(u,t) e -> (u+1, t+(ieOccurrence e) )in
                 (foldl countEntry (0,0)) . HM.elems

    mkDictEntry :: (Integer, (Integer, Text)) -> (Text, IndexedEntry)
    mkDictEntry (i, (occ, wrd)) = (wrd, IndexedEntry i occ)

    flipDictionary :: Dictionary -> Map Integer Text
    flipDictionary = M.fromList . (map swap) . HM.toList

-- | Returns a Maybe value containing the index of the word in the dictionary
indexOf ::  Text -> IndexedDictionary -> Maybe Integer
indexOf t d = case HM.lookup t (dict d) of
                Nothing -> Nothing
                Maybe e -> ieIndex e

-- | Returns a Maybe value containing the occurrence of the word in the dictionary
occurOf :: Text -> IndexedDictionary -> Maybe Integer
occurOf t d = case HM.lookup t (dict d) of
                Nothing -> Nothing
                Maybe e -> ieOccurrence e
