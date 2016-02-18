-- | Provides a free monad API on top of purescript-webstorage.

-- | [whatwg-storage-interface]: https://html.spec.whatwg.org/multipage/webstorage.html#the-storage-interface

module Browser.WebStorage.Free
  ( StorageF(..)
  , Storage
  , StorageT
  , clear
  , getItem
  , key
  , length
  , removeItem
  , setItem
  , storageFI
  , runStorage
  , runStorageT
  ) where

import Prelude (class Monad, class Functor, Unit, (<$>), unit, (>>>), (>>=), id, (<<<), pure)

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free.Trans (FreeT, runFreeT, liftFreeT, hoistFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe())
import Data.Foldable (traverse_)
import Data.Identity (Identity, runIdentity)
import Data.Functor ((<$))
import Data.Int as Int

import Browser.WebStorage as WebStorage

-- | `StorageF` is an algebra which describes [the Storage interface][whatwg-storage-interface].
data StorageF a
  = Length (Int -> a)
  | Key Int (Maybe String -> a)
  | GetItem String (Maybe String -> a)
  | SetItem String String a
  | RemoveItem String a
  | Clear a

instance functorStorageF :: Functor StorageF where
  map f (Clear a) = Clear (f a)
  map f (GetItem a b) = GetItem a (f <$> b)
  map f (Key a b) = Key a (f <$> b)
  map f (Length a) = Length (f <$> a)
  map f (RemoveItem a b) = RemoveItem a (f b)
  map f (SetItem a b c) = SetItem a b (f c)

-- | A free monad around the `StorageF` algebra.
type Storage = StorageT Identity

-- | A free monad transformer around the `StorageF` algebra.
type StorageT = FreeT StorageF

-- | `length` returns the number of key/value pairs currently present in the storage.
length :: ∀ m. (Monad m) => StorageT m Int
length = liftFreeT (Length id)

-- | `key n` returns the name of the *n*th key in the storage.
key :: ∀ m. (Monad m) => Int -> StorageT m (Maybe String)
key n = liftFreeT (Key n id)

-- | `getItem key` returns the current value associated with the given key.
getItem :: ∀ m. (Monad m) => String -> StorageT m (Maybe String)
getItem k = liftFreeT (GetItem k id)

-- | `setItem key value` would add a new key/value pair to the storage,
-- | or update the existing.
setItem :: ∀ m. (Monad m) => String -> String -> StorageT m Unit
setItem k value = liftFreeT (SetItem k value unit)

-- | `modify key f` would get update the value associated with the given key using
-- | provided function.
modify :: ∀ m. (Monad m) => String -> (String -> String) -> StorageT m Unit
modify k f = getItem k >>= traverse_ (f >>> setItem k)

-- | `removeItem key` would remove the key/value pair with the given `key` from
-- | the storage, if it exists.
removeItem :: ∀ m. (Monad m) => String -> StorageT m Unit
removeItem a = liftFreeT (RemoveItem a unit)

-- | `clear` would empty the storage of all key/value pairs.
clear :: ∀ m. (Monad m) => StorageT m Unit
clear = liftFreeT (Clear unit)

-- | Interpret `StorageF` operation as a call to the corresponding method of a
-- | passed *Storage* object.
storageFI
  :: ∀ s m eff a.
   ( WebStorage.Storage s
   , MonadEff (webStorage :: WebStorage.WebStorage | eff) m
   , MonadRec m
   )
  => s -> StorageF a -> m a
storageFI storage query = case query of
  Clear          next -> next <$  liftEff (WebStorage.clear      storage)
  GetItem    a   cont -> cont <$> liftEff (WebStorage.getItem    storage a)
  Key        a   cont -> cont <$> liftEff (WebStorage.key        storage (Int.toNumber a))
  Length         cont -> cont <$> liftEff (Int.round <$> WebStorage.length storage)
  RemoveItem a   next -> next <$  liftEff (WebStorage.removeItem storage a)
  SetItem    a b next -> next <$  liftEff (WebStorage.setItem    storage a b)

-- | Interpret the `Storage` computation as calls to the corresponding methods
-- | of a passed *Storage* object.
runStorage
  :: ∀ a s m eff.
   ( WebStorage.Storage s
   , MonadEff (webStorage :: WebStorage.WebStorage | eff) m
   , MonadRec m
   )
  => s -> Storage a -> m a
runStorage storage = runStorageT storage <<< hoistFreeT (pure <<< runIdentity)

-- | Perform the computation in `StorageT m` as calls to the corresponding methods
-- | of a passed *Storage*, the computation is performed in the underlying monad `m`
-- | which has to allow performing `WebStorage` effects.
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
