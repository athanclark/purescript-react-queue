module React.Signal.LifeCycle where

import Prelude
import Data.Maybe (Maybe (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import React (ReactSpec)
import Signal.Types (READ, WRITE)
import Signal.Internal as Signal


data ReactLifeCycle props state
  = Unmounted
  | Mounting
  | Mounted
  | Unmounting
  | Updating {props :: props, state :: state}
  | Updated {props :: props, state :: state}
  | ReceivingProps {props :: props}
  | Caught {error :: Error, componentStack :: String}


withLifeCycle :: forall props state render eff
               . ReactSpec props state render (ref :: REF | eff)
              -> Eff (ref :: REF | eff)
                  { spec :: ReactSpec props state render (ref :: REF | eff)
                  , signal :: Signal.Signal (read :: READ, write :: WRITE) (ref :: REF | eff) (ReactLifeCycle props state)
                  }
withLifeCycle reactSpec = do
  signal <- Signal.make Unmounted
  pure
    { signal
    , spec: reactSpec
        { componentWillMount = \this -> do
            unsafeCoerceEff $ Signal.set Mounting signal
            reactSpec.componentWillMount this
        , componentDidMount = \this -> do
            unsafeCoerceEff $ Signal.set Mounted signal
            reactSpec.componentDidMount this
        , componentWillUnmount = \this -> do
            unsafeCoerceEff $ Signal.set Unmounting signal
            reactSpec.componentWillUnmount this
        , componentWillUpdate = \this props state -> do
            unsafeCoerceEff $ Signal.set (Updating {props,state}) signal
            reactSpec.componentWillUpdate this props state
        , componentDidUpdate = \this props state -> do
            unsafeCoerceEff $ Signal.set (Updated {props,state}) signal
            reactSpec.componentDidUpdate this props state
        , componentWillReceiveProps = \this props -> do
            unsafeCoerceEff $ Signal.set (ReceivingProps {props}) signal
            reactSpec.componentWillReceiveProps this props
        , componentDidCatch = Just \this error params -> do
            unsafeCoerceEff $ Signal.set (Caught {error, componentStack: params.componentStack}) signal
            case reactSpec.componentDidCatch of
              Nothing -> pure unit
              Just f -> f this error params
        }
    }
