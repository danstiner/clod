
module Store.Default (
    createStore
  , Store
  , runDupes
) where

import Control.Monad.Identity

import Dupes

newtype Store = Store FilePath

createStore :: FilePath -> Store
createStore path = Store path

runDupes :: (Monad m) => Store -> DupesT Identity a -> m a
runDupes _ _ = do error "Not implemmented"

instance DupesMonad Identity where
	list _ = do error "Not implemmented"
	add _ _ = do error "Not implemmented"
	remove _ = do error "Not implemmented"