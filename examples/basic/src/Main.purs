module Main where

import Prelude (Unit, ($), bind, (++))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans (lift)
import Data.Maybe (Maybe(..))

import Browser.WebStorage (WebStorage())
import Browser.WebStorage.Free (setItem, getItem, runLocalStorageT)

main :: forall e. Eff (console :: CONSOLE, webStorage :: WebStorage | e) Unit
main = runLocalStorageT do
  mName <- getItem "name"
  case mName of
    Just name ->
      lift $ log $ "Hello, " ++ name ++ "!"
    Nothing -> do
      lift $ log "I don't really know your name, let it be 'Sailor'"
      setItem "name" "Sailor"
