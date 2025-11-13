module Lib.Primitive where
import Data.Bool (bool)

class Def a where
  def :: a



newtype Entry a b = Entry { getEntry :: (a, b)}
newtype Cache a b = Cache { getCache :: [Entry a b] }

lookupCache :: Eq a => Cache a b -> a -> Maybe b
lookupCache (Cache ((Entry (key,value)):xs)) y = bool (lookupCache (Cache xs) y) (Just value) (key==y)
lookupCache (Cache []) _ = Nothing

removeFromCache :: Eq a => Cache a b -> a -> Cache a b
removeFromCache (Cache ((Entry (key, value)):xs)) y = Cache (bool [Entry (key, value)] [] (key==y) ++ getCache (removeFromCache (Cache xs) y))
removeFromCache (Cache []) _ = Cache []
