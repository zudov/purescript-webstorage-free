module Main where

import Prelude (class Monad, class Bind, class Applicative, class Apply,
                class Functor, Unit, ($), bind, (<<<), unit, pure, const, show,
                (<>), negate, apply, map, (+))

import Control.Bind ((=<<))
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Trans (lift)
import Data.Functor ((<$))
import Data.Int as Int
import Data.Maybe (fromMaybe)

import Halogen (HalogenEffects, ComponentDSL, Natural, ComponentHTML, Component,
                action, interpret, runUI, component, set, liftH, gets, modify, liftEff')
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

import Browser.WebStorage (WebStorage, localStorage)
import Browser.WebStorage.Free (class HasStorage, StorageT, runStorageT,
                                getItem, setItem, liftStorage)

data Query a
  = Restore a
  | Update Int a

type State = { counter :: Int }

updateCounter :: Int -> State -> State
updateCounter a = \state -> state { counter = state.counter + a }

initialState :: State
initialState = { counter: 0 }

newtype AppF eff a = AppF (StorageT (Aff eff) a)

runAppF :: ∀ eff a. AppF eff a -> StorageT (Aff eff) a
runAppF (AppF a) = a

instance appFFunctor :: Functor (AppF eff) where
  map f (AppF a) = AppF (map f a)

instance appFApply :: Apply (AppF eff) where
  apply (AppF f) (AppF a) = AppF (apply f a)

instance appFApplicative :: Applicative (AppF eff) where
  pure a = AppF (pure a)

instance appFBind :: Bind (AppF eff) where
  bind (AppF a) f = AppF (bind a (runAppF <<< f))

instance appFMonad :: Monad (AppF eff)

instance appFMonadEff :: MonadEff eff (AppF eff) where
  liftEff eff = AppF (lift (liftEff eff))

instance appFMonadAff :: MonadAff eff (AppF eff) where
  liftAff aff = AppF (lift (liftAff aff))

instance appFHasStorage :: HasStorage (AppF eff) where
  liftStorage a = AppF (liftStorage a)

type AppEffects
  = HalogenEffects
      ( webStorage :: WebStorage
      , console :: CONSOLE
      )

type AppFP = AppF AppEffects

ui :: Component State Query AppFP
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text (show state.counter) ]
      , H.button
          [ E.onClick (E.input_ (Update (-1))) ]
          [ H.text "-1" ]
      , H.button
          [ E.onClick (E.input_ (Update 1)) ]
          [ H.text "+1" ]
      ]

  eval :: Natural Query (ComponentDSL State Query AppFP)
  eval (Update a next) = next <$ do
    modify (updateCounter a)
    counter <- gets _.counter
    liftEff' $ log $ "Counter updated. New value: " <> show counter
    liftH $ setItem "counter" $ show counter
  eval (Restore next) = next <$ do
    liftEff' $ log "Restoring counter's value"
    mCounter <- liftH $ getItem "counter"
    set { counter: fromMaybe 0 (Int.fromString =<< mCounter) }

main :: Eff AppEffects Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI (interpret (runStorageT localStorage <<< runAppF) ui) initialState
  app.driver $ action Restore
  onLoad $ appendToBody app.node
