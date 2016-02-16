-- | Provides a free monad API on top of purescript-webstorage.
module Browser.WebStorage.Free
  ( StorageF(..)
  , Storage()
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
  ) where

import Prelude (class Functor, Unit, (<$>), unit, id)

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, runFreeM, liftF)
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

type Storage a = Free StorageF a

clear :: Storage Unit
clear = liftF (Clear unit)

getItem :: String -> Storage (Maybe String)
getItem a = liftF (GetItem a id)

key :: Number -> Storage (Maybe String)
key a = liftF (Key a id)

length :: Storage Number
length = liftF (Length id)

removeItem :: String -> Storage Unit
removeItem a = liftF (RemoveItem a unit)

setItem :: String -> String -> Storage Unit
setItem a b = liftF (SetItem a b unit)

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

runLocalStorage :: ∀ a eff. Storage a -> WebStorage.EffWebStorage eff a
runLocalStorage = runStorage WebStorage.localStorage

runSessionStorage :: ∀ a eff. Storage a -> WebStorage.EffWebStorage eff a
runSessionStorage = runStorage WebStorage.sessionStorage

