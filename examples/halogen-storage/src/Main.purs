module Main where

import Prelude (Unit, ($), bind, unit, pure, const, show, negate, (+))

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Functor ((<$))
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Control.Bind ((=<<))

import Halogen (HalogenEffects, ComponentDSL, Natural, ComponentHTML, Component,
                action, interpret, runUI, component, set, liftH, gets, modify)
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

import Browser.WebStorage (WebStorage, localStorage)
import Browser.WebStorage.Free (Storage, runStorage, getItem, setItem)

data Query a
  = Restore a
  | Update Int a

type State = { counter :: Int }

updateCounter :: Int -> State -> State
updateCounter a = \state -> state { counter = state.counter + a}

initialState :: State
initialState = { counter: 0 }

ui :: Component State Query Storage
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

  eval :: Natural Query (ComponentDSL State Query Storage)
  eval (Update a next) = next <$ do
    modify (updateCounter a)
    counter <- gets _.counter
    liftH $ setItem "counter" $ show counter
  eval (Restore next) = next <$ do
    mCounter <- liftH $ getItem "counter"
    set { counter: fromMaybe 0 (Int.fromString =<< mCounter) }

main :: Eff (HalogenEffects ( webStorage :: WebStorage )) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI (interpret (runStorage localStorage) ui) initialState
  app.driver $ action Restore
  onLoad $ appendToBody app.node
