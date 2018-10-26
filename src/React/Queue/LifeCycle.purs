module React.Queue.LifeCycle where

import Prelude
import Data.Maybe (Maybe (..))
import React (ReactSpec)
import Queue.Types (WRITE)
import Queue (Queue)
import Queue as Queue
import Queue.One as One
import IxQueue as IxQueue


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
      Queue.put q WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      Queue.put q DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      Queue.put q WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      Queue.put q $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      Queue.put q $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      Queue.put q $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just \this error params -> do
      Queue.put q $ DidCatch {error, componentStack: params.componentStack}
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
      One.put q WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      One.put q DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      One.put q WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      One.put q $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      One.put q $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      One.put q $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just $ \this error params -> do
      One.put q $ DidCatch {error, componentStack: params.componentStack}
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
      IxQueue.put q k WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      IxQueue.put q k DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      IxQueue.put q k WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      IxQueue.put q k $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      IxQueue.put q k $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      IxQueue.put q k $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = Just \this error params -> do
      IxQueue.put q k $ DidCatch {error, componentStack: params.componentStack}
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
      IxQueue.broadcastExcept q ks WillMount
      reactSpec.componentWillMount this
  , componentDidMount = \this -> do
      IxQueue.broadcastExcept q ks DidMount
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      IxQueue.broadcastExcept q ks WillUnmount
      reactSpec.componentWillUnmount this
  , componentWillUpdate = \this props state -> do
      IxQueue.broadcastExcept q ks $ WillUpdate {props,state}
      reactSpec.componentWillUpdate this props state
  , componentDidUpdate = \this props state -> do
      IxQueue.broadcastExcept q ks $ DidUpdate {props,state}
      reactSpec.componentDidUpdate this props state
  , componentWillReceiveProps = \this props -> do
      IxQueue.broadcastExcept q ks $ WillReceiveProps {props}
      reactSpec.componentWillReceiveProps this props
  , componentDidCatch = \this error params -> do
      IxQueue.broadcastExcept q ks $ DidCatch {error, componentStack: params.componentStack}
      reactSpec.componentDidCatch this error params
  }
