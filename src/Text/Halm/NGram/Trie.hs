-- | Generic trie-like structure on indexed ngrams. The trie is strict in its
--   argument (evaluated to WHNF).
module Text.Halm.NGram.Trie
    (
     -- * Types
     TrieNode

    -- * Functions ranging over tries
    , empty
    , Text.Halm.NGram.Trie.lookup
    , updateWith
    , fold

    -- * Functions ranging over single nodes
    , nodeChilds
    , nodeChildsMap
    , nodeData
    ) where

import qualified Data.HashMap.Strict as HM

import Data.Maybe (fromMaybe, isNothing)

import Text.Halm.Dictionary (UnitId)


-- | Type used to represent pointers to the children of
--   a specific node. That is pointers to the ngrams which
--   has the current mgram as a prefix.
type TrieMap a = HM.HashMap UnitId (TrieNode a)

mEmpty :: TrieMap a
mEmpty = HM.empty

-- | Lookup the TrieNode for the specified unit in the TrieMap
mLookup :: UnitId -> TrieMap a -> Maybe (TrieNode a)
mLookup = HM.lookup

-- | Insert/Updates the data associated to the specified unit in the TrieMap
mInsert :: UnitId -> TrieNode a -> TrieMap a -> TrieMap a
mInsert = HM.insert

-- | Return the size of the specified TrieMap
mSize :: TrieMap a -> Int
mSize = HM.size

-- | Data type defining the tries of units. The structure is a
--   common tries where the dictionary is represented by the units
--   identifiers. Each node represent an mgram prefix of the final ngram.
--   The pointers to the child nodes are kept in a Map in order to
--   improve lookup speed.
data TrieNode a = Root (TrieMap a)     -- ^ Represent the root of the trie
                | Branch a (TrieMap a) -- ^ Represent a mid-ngram unit
                | Leaf a               -- ^ Represent the n-th unit
                deriving (Show)


-- | Creates an empty ngram trie.
empty :: TrieNode a
empty = Root mEmpty

-- | Extracts the data associated to the given trie node
nodeData :: TrieNode a -> Maybe a
nodeData (Root _) = Nothing
nodeData (Branch d _) = Just d
nodeData (Leaf d) = Just d

-- | Extracts the childs map for the given trie node
nodeChildsMap :: TrieNode a -> Maybe (TrieMap a)
nodeChildsMap (Root trMap) = Just trMap
nodeChildsMap (Branch _ trMap) = Just trMap
nodeChildsMap (Leaf _) = Nothing

-- | Returns the number of childs for the given trie node
nodeChilds :: TrieNode a -> Int
nodeChilds (Root trMap) = mSize trMap
nodeChilds (Branch _ trMap) = mSize trMap
nodeChilds (Leaf _) = 0

-- | Lookup the data associated to the units of the specified mgram.
lookup :: [UnitId] -> TrieNode a -> Maybe [a]
lookup uids tr =
    case tr of
      Root trMap -> lookup' uids trMap []
      _ -> error "Lookup called on non-root node"
    where
      lookup' :: [UnitId] -> TrieMap a -> [a] -> Maybe [a]
      lookup' [] trMap ds = Just $ reverse ds
      lookup' (uid:uids) trMap ds =
          case mLookup uid trMap of
            Just (Branch d trMap')    -> lookup' uids trMap' (d:ds)
            Just (Leaf d) | null uids -> Just $ reverse ds
                          | otherwise -> Nothing
            Nothing                   -> Nothing

-- | Update the data associated to the units in the specified
--   ngram calling the function supplied as first argument.
--   The updating function receives in order, the index (0-based) of the
--   current unit, the unit identifier and the existing data associated
--   to the unit or 'Nothing' if the mgram prefix didn't exists.
--   The output of the function is the updated data for the unit.
updateWith :: (Int -> UnitId -> Maybe a -> a)
              -> [UnitId]
              -> TrieNode a
              -> TrieNode a
updateWith f uids tr =
    case tr of
      Root trMap -> let trMap' = updateWith' f (zip [0..] uids) trMap
                    in trMap' `seq` Root trMap'
      _ -> error "UpdateWith called on non-root node"
    where
      updateWith' :: (Int -> UnitId -> Maybe a -> a)
                     -> [(Int, UnitId)]
                     -> TrieMap a
                     -> TrieMap a
      updateWith' f [] trMap = trMap
      updateWith' f ( (i,uid):xs ) trMap =
          let node = mLookup uid trMap
              d = maybe Nothing nodeData node
              childsMap = maybe Nothing nodeChildsMap node
              d' = f i uid d
              childsMap' = updateWith' f xs $ fromMaybe mEmpty  childsMap
          in case xs of
               [] | isNothing childsMap -> d' `seq` mInsert uid (Leaf d') trMap
                  | otherwise ->  d' `seq` childsMap' `seq` mInsert uid (Branch d' childsMap') trMap
               _ -> d' `seq` childsMap' `seq` mInsert uid (Branch d' childsMap') trMap


-- | Fold the ngram trie. For each node the folding function is
--   called specifing the current accumulator, the units forming the m-gram
--   and the actual node.
fold :: (b -> [UnitId] -> TrieNode a -> b) -> b -> TrieNode a -> b
fold f acc tr =
    case tr of
      Root trMap -> fold' f acc [] trMap
      _ -> error "Fold called on non-root node"
    where
      fold' :: (b -> [UnitId] -> TrieNode a -> b)
            -> b
            -> [UnitId]
            -> TrieMap a
            -> b
      fold' f acc uids trMap =
          let (acc', _, _) = HM.foldlWithKey' foldNode (acc, f, uids) trMap
          in acc'

      -- | Fold a specific node
      foldNode :: (b, (b -> [UnitId] -> TrieNode a -> b), [UnitId])
               -> UnitId
               -> TrieNode a
               -> (b, (b -> [UnitId] -> TrieNode a -> b), [UnitId])
      foldNode (uacc, f, prefixIds) uid node =
          let uids = (prefixIds ++ [uid])
              uacc' = f uacc uids node
              childsMap = nodeChildsMap node
              uacc'' = uacc' `seq` maybe uacc' (fold' f uacc' uids) childsMap
          in uacc'' `seq` (uacc'', f, prefixIds)
