--------------------------------------------------------------------------------
module Hakyll.Snippets
    ( Snippets (..)
    , singleSnippet
    , printSnippets
    ) where


--------------------------------------------------------------------------------
import           Control.Monad (forM_)
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Monoid   (Monoid (..))


--------------------------------------------------------------------------------
newtype Snippets = Snippets {unSnippets :: Map String [String]}
    deriving (Show)


--------------------------------------------------------------------------------
instance Monoid Snippets where
    mempty                            = Snippets M.empty
    mappend (Snippets x) (Snippets y) = Snippets $ M.unionWith (++) x y


--------------------------------------------------------------------------------
singleSnippet :: String -> [String] -> Snippets
singleSnippet key ls = Snippets $ M.singleton key ls


--------------------------------------------------------------------------------
printSnippets :: Snippets -> IO ()
printSnippets (Snippets m) = forM_ (M.toList m) $ \(k, ls) -> do
    putStrLn $ replicate 80 '-'
    putStrLn $ "-- | " ++ k
    putStrLn $ unlines ls
