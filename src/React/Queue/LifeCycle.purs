module React.Queue.LifeCycle where

import Prelude
import Data.Maybe (Maybe (..))
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import React (ReactSpec)
import Queue.Types (WRITE)
import Queue (Queue, putQueue)
import Queue.One (Queue, putQueue) as One
import IxQueue (IxQueue, putIxQueue)


data ReactLifeCycle props state
  = WillMount
  | DidMount
  | WillUnmount
  | WillUpdate {props :: props, state :: state}
  | DidUpdate {props :: props, state :: state}
  | WillReceiveProps {props :: props}
  | DidCatch {error :: Error, componentStack :: String}


type Effects eff = (ref :: REF | eff)


withLifeCycle :: forall props state render eff rw
               . Queue (write :: WRITE | rw) (Effects eff) (ReactLifeCycle props state)
              -> ReactSpec props state render (Effects eff)
              -> ReactSpec props state render (Effects eff)
withLifeCycle q reactSpec = reactSpec
  { componentWillMount = \this -> do
      unsafeCoerceEff $ putQueue q WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      unsafeCoerceEff $ putQueue q DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff $ putQueue q WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      unsafeCoerceEff $ putQueue q $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      unsafeCoerceEff $ putQueue q $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      unsafeCoerceEff $ putQueue q $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just \this error params -> do
      unsafeCoerceEff $ putQueue q $ DidCatch {error, componentStack: params.componentStack}
      case reactSpec.componentDidCatch of
        Nothing -> pure unit
        Just f -> f this error params
  }


withLifeCycleOne :: forall props state render eff rw
                  . One.Queue (write :: WRITE | rw) (Effects eff) (ReactLifeCycle props state)
                 -> ReactSpec props state render (Effects eff)
                 -> ReactSpec props state render (Effects eff)
withLifeCycleOne q reactSpec = reactSpec
  { componentWillMount = \this -> do
      unsafeCoerceEff $ One.putQueue q WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      unsafeCoerceEff $ One.putQueue q DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff $ One.putQueue q WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      unsafeCoerceEff $ One.putQueue q $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      unsafeCoerceEff $ One.putQueue q $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      unsafeCoerceEff $ One.putQueue q $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just $ \this error params -> do
      unsafeCoerceEff $ One.putQueue q $ DidCatch {error, componentStack: params.componentStack}
      case reactSpec.componentDidCatch of
        Nothing -> pure unit
        Just f -> f this error params
  }


withLifeCycleIx :: forall props state render eff rw
                 . String
                -> IxQueue (write :: WRITE | rw) (Effects eff) (ReactLifeCycle props state)
                -> ReactSpec props state render (Effects eff)
                -> ReactSpec props state render (Effects eff)
withLifeCycleIx k q reactSpec = reactSpec
  { componentWillMount = \this -> do
      unsafeCoerceEff $ putIxQueue q k WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      unsafeCoerceEff $ putIxQueue q k DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff $ putIxQueue q k WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      unsafeCoerceEff $ putIxQueue q k $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      unsafeCoerceEff $ putIxQueue q k $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      unsafeCoerceEff $ putIxQueue q k $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just \this error params -> do
      unsafeCoerceEff $ putIxQueue q k $ DidCatch {error, componentStack: params.componentStack}
      case reactSpec.componentDidCatch of
        Nothing -> pure unit
        Just f -> f this error params
  }
