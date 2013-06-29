module Main where

import Control.Monad (foldM)
import Data.List (foldl')
import Data.Text (Text)
import Debug.Trace (trace)
import System.Environment (getArgs)

import qualified Text.Halm.Dictionary as D
import qualified Text.Halm.NGram.Table as NT

main = do
  paths <- getArgs
  case paths of
    [] -> putStrLn "Specify a text file"
    _  -> do
         dict <- foldM (flip D.extendF) D.empty paths
         putStrLn $ "Dictionary size: " ++ (show $ D.size dict)

         let idict = D.index dict
         putStrLn $ "Dictionary unique words: " ++ (show $ D.uniques idict)
         putStrLn $ "Dictionary total words: " ++ (show $ D.total idict)

         ngtbl <- foldM (\tbl f -> NT.extendF f 2 tbl) (NT.empty idict) paths
         putStrLn $ "Uniques ngrams: " ++ (show $ NT.uniques ngtbl)

-- let dict = foldl (flip extendDictionary) empty txts
--               idict = indexDictionary dict
--               ngtbl = foldr extendNGramTable'' (emptyTable idict) txts1
--           in do
--             putStrLn $ "Dictionary size: " ++ (show $ size dict)
--             -- TLIO.writeFile "ttt" $ T.unlines (wlist dict)
--             putStrLn $ "Dictionary unique words: " ++ (show $ uniques idict)
--             putStrLn $ "Dictionary total words: " ++ (show $ total idict)
--             putStrLn $ "Uniques ngrams: " ++ (show $ uniquesNGrams ngtbl)
--             --putStrLn $ "ngrams: " ++ (show $ ngtbl)