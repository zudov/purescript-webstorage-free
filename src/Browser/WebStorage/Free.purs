-- | Provides a free monad API on top of purescript-webstorage.
module Browser.WebStorage.Free
  ( StorageF(..)
  , Storage
  , StorageT
  , class HasStorage
  , liftStorage
  , clear
  , getItem
  , key
  , length
  , removeItem
  , setItem
  , runStorage
  , runLocalStorage
  , runSessionStorage
  , storageFI
  , runStorageT
  , runLocalStorageT
  , runSessionStorageT
  ) where

import Prelude (class Monad, class Functor, Unit, (<$>), unit, id, (<<<))

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, runFreeM, liftF)
import Control.Monad.Free.Trans (FreeT, runFreeT, liftFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe())
import Data.Functor ((<$))
import Data.NaturalTransformation (Natural())

import Browser.WebStorage as WebStorage

data StorageF a
  = Clear a
  | GetItem String (Maybe String -> a)
  | Key Number (Maybe String -> a)
  | Length (Number -> a)
  | RemoveItem String a
  | SetItem String String a

instance functorStorageF :: Functor StorageF where
  map f (Clear a) = Clear (f a)
  map f (GetItem a b) = GetItem a (f <$> b)
  map f (Key a b) = Key a (f <$> b)
  map f (Length a) = Length (f <$> a)
  map f (RemoveItem a b) = RemoveItem a (f b)
  map f (SetItem a b c) = SetItem a b (f c)

class (Functor f) <= HasStorage f where
  liftStorage :: Natural StorageF f

instance storageFHasStorage :: HasStorage StorageF where
  liftStorage = id

instance freeHasStorage :: (HasStorage f) => HasStorage (Free f) where
  liftStorage = liftF <<< liftStorage

instance freeTHasStorage :: (Monad m, HasStorage f) => HasStorage (FreeT f m) where
  liftStorage = liftFreeT <<< liftStorage

type Storage = Free StorageF
type StorageT = FreeT StorageF

clear :: ∀ f. (HasStorage f) => f Unit
clear = liftStorage (Clear unit)

getItem :: ∀ f. (HasStorage f) => String -> f (Maybe String)
getItem a = liftStorage (GetItem a id)

key :: ∀ f. (HasStorage f) => Number -> f (Maybe String)
key a = liftStorage (Key a id)

length :: ∀ f. (HasStorage f) => f Number
length = liftStorage (Length id)

removeItem :: ∀ f. (HasStorage f) => String -> f Unit
removeItem a = liftStorage (RemoveItem a unit)

setItem :: ∀ f. (HasStorage f) => String -> String -> f Unit
setItem a b = liftStorage (SetItem a b unit)

runStorage
  :: ∀ a s m eff.
   ( WebStorage.Storage s
   , MonadEff (webStorage :: WebStorage.WebStorage | eff) m
   , MonadRec m
   )
  => s -> Storage a -> m a
runStorage storage = runFreeM (storageFI storage)

storageFI
  :: ∀ s m eff.
   ( WebStorage.Storage s
   , MonadEff (webStorage :: WebStorage.WebStorage | eff) m
   , MonadRec m
   )
  => s -> Natural StorageF m
storageFI storage query = case query of
  Clear          next -> next <$  liftEff (WebStorage.clear      storage)
  GetItem    a   cont -> cont <$> liftEff (WebStorage.getItem    storage a)
  Key        a   cont -> cont <$> liftEff (WebStorage.key        storage a)
  Length         cont -> cont <$> liftEff (WebStorage.length     storage)
  RemoveItem a   next -> next <$  liftEff (WebStorage.removeItem storage a)
  SetItem    a b next -> next <$  liftEff (WebStorage.setItem    storage a b)

runLocalStorage
  :: ∀ a m eff.
   ( MonadEff (webStorage :: WebStorage.WebStorage | eff) m
   , MonadRec m
   )
  => Storage a
  -> m a
runLocalStorage = runStorage WebStorage.localStorage

runSessionStorage
  :: ∀ a m eff.
   ( MonadEff (webStorage :: WebStorage.WebStorage | eff) m
   , MonadRec m
   )
  => Storage a
  -> m a
runSessionStorage = runStorage WebStorage.sessionStorage

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
