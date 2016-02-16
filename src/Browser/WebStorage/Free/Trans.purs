-- | Provides a transformer for free monad API on top of purescript-webstorage.
module Browser.WebStorage.Free.Trans
  ( StorageT
  , clear
  , getItem
  , key
  , length
  , removeItem
  , setItem
  , runStorageT
  , runLocalStorageT
  , runSessionStorageT
  ) where

import Prelude (class Monad, Unit, unit, id)

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Free.Trans (FreeT, runFreeT, liftFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe())

import Browser.WebStorage as WebStorage
import Browser.WebStorage.Free (StorageF(..), storageFI)

type StorageT = FreeT StorageF

clear :: ∀ m. (Monad m) => StorageT m Unit
clear = liftFreeT (Clear unit)

getItem :: ∀ m. (Monad m) => String -> StorageT m (Maybe String)
getItem a = liftFreeT (GetItem a id)

key :: ∀ m. (Monad m) => Number -> StorageT m (Maybe String)
key a = liftFreeT (Key a id)

length :: ∀ m. (Monad m) => StorageT m Number
length = liftFreeT (Length id)

removeItem :: ∀ m. (Monad m) => String -> StorageT m Unit
removeItem a = liftFreeT (RemoveItem a unit)

setItem :: ∀ m. (Monad m) => String -> String -> StorageT m Unit
setItem a b = liftFreeT (SetItem a b unit)

runStorageT
  :: ∀ a m s eff.
   ( WebStorage.Storage s
   , MonadEff (webStorage :: WebStorage.WebStorage | eff) m
   , MonadRec m
   )
  => s
  -> StorageT m a
  -> m a
runStorageT storage = runFreeT (storageFI storage)

runLocalStorageT
  :: ∀ a m eff.
   ( MonadEff (webStorage :: WebStorage.WebStorage | eff) m
   , MonadRec m
   )
  => StorageT m a
  -> m a
runLocalStorageT = runStorageT WebStorage.localStorage

runSessionStorageT
  :: ∀ a m eff.
   ( MonadEff (webStorage :: WebStorage.WebStorage | eff) m
   , MonadRec m
   )
  => StorageT m a
  -> m a
runSessionStorageT = runStorageT WebStorage.sessionStorage
