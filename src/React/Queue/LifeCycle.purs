module React.Queue.LifeCycle where

import Prelude
import Data.Maybe (Maybe (..))
-- import Control.Monad.Eff.Ref (REF)
-- import Control.Monad.Eff.Exception (Error)
-- import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import React (ReactSpec)
import Queue.Types (WRITE)
import Queue (Queue, putQueue)
import Queue.One (Queue, putQueue) as One
import IxQueue (IxQueue, putIxQueue, broadcastExceptIxQueue)


data ReactLifeCycle props state
  = WillMount
  | DidMount
  | WillUnmount
  | WillUpdate {props :: props, state :: state}
  | DidUpdate {props :: props, state :: state}
  | WillReceiveProps {props :: props}
  | DidCatch {error :: Error, componentStack :: String}



withLifeCycle :: forall props state snapshot spec rw
               . Queue (write :: WRITE | rw) (ReactLifeCycle props state)
              -> { | ReactSpecOptional props state snapshot spec }
              -> { | ReactSpecOptional props state snapshot spec }
withLifeCycle q reactSpec = reactSpec
  { componentWillMount = \this -> do
      putQueue q WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      putQueue q DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      putQueue q WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      putQueue q $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      putQueue q $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      putQueue q $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just \this error params -> do
      putQueue q $ DidCatch {error, componentStack: params.componentStack}
      case reactSpec.componentDidCatch of
        Nothing -> pure unit
        Just f -> f this error params
  }


withLifeCycleOne :: forall props state snapshot spec rw
                  . One.Queue (write :: WRITE | rw) (ReactLifeCycle props state)
                 -> { | ReactSpecOptional props state snapshot spec }
                 -> { | ReactSpecOptional props state snapshot spec }
withLifeCycleOne q reactSpec = reactSpec
  { componentWillMount = \this -> do
      One.putQueue q WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      One.putQueue q DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      One.putQueue q WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      One.putQueue q $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      One.putQueue q $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      One.putQueue q $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just $ \this error params -> do
      One.putQueue q $ DidCatch {error, componentStack: params.componentStack}
      case reactSpec.componentDidCatch of
        Nothing -> pure unit
        Just f -> f this error params
  }


withLifeCycleIx :: forall props state snapshot spec rw
                 . String
                -> IxQueue (write :: WRITE | rw) (ReactLifeCycle props state)
                -> { | ReactSpecOptional props state snapshot spec }
                -> { | ReactSpecOptional props state snapshot spec }
withLifeCycleIx k q reactSpec = reactSpec
  { componentWillMount = \this -> do
      putIxQueue q k WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      putIxQueue q k DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      putIxQueue q k WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      putIxQueue q k $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      putIxQueue q k $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      putIxQueue q k $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just \this error params -> do
      putIxQueue q k $ DidCatch {error, componentStack: params.componentStack}
      case reactSpec.componentDidCatch of
        Nothing -> pure unit
        Just f -> f this error params
  }


withLifeCycleBroadcastIx :: forall props state snapshot spec rw
                          . Array String -- exception keys
                         -> IxQueue (write :: WRITE | rw) (ReactLifeCycle props state)
                         -> { | ReactSpecOptional props state snapshot spec }
                         -> { | ReactSpecOptional props state snapshot spec }
withLifeCycleBroadcastIx ks q reactSpec = reactSpec
  { componentWillMount = \this -> do
      broadcastExceptIxQueue q ks WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      broadcastExceptIxQueue q ks DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      broadcastExceptIxQueue q ks WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      broadcastExceptIxQueue q ks $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      broadcastExceptIxQueue q ks $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      broadcastExceptIxQueue q ks $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just \this error params -> do
      broadcastExceptIxQueue q ks $ DidCatch {error, componentStack: params.componentStack}
      case reactSpec.componentDidCatch of
        Nothing -> pure unit
        Just f -> f this error params
  }
