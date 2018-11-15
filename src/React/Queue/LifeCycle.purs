module React.Queue.LifeCycle where

import Prelude (bind, pure, ($), discard)
import React (ReactSpecAll, ReactClassConstructor)
import Effect.Exception (Error)
import Queue.Types (WRITE)
import Queue (Queue)
import Queue (put) as Queue
import Queue.One (Queue, put) as One
import IxQueue (IxQueue)
import IxQueue (put, broadcastExcept) as IxQueue


data ReactLifeCycle props state snapshot
  = DidMount
  | WillUnmount
  | DidUpdate {props :: props, state :: state, snapshot :: snapshot}
  | DidCatch {error :: Error, componentStack :: String}



withLifeCycle :: forall props state snapshot rw
               . Queue (write :: WRITE | rw) (ReactLifeCycle props state snapshot)
              -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
              -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
withLifeCycle q constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        Queue.put q DidMount
        reactSpec.componentDidMount
    , componentWillUnmount = do
        Queue.put q WillUnmount
        reactSpec.componentWillUnmount
    , componentDidUpdate = \props state snapshot -> do
        Queue.put q $ DidUpdate {props,state,snapshot}
        reactSpec.componentDidUpdate props state snapshot
    , componentDidCatch = \error params -> do
        Queue.put q $ DidCatch {error, componentStack: params.componentStack}
        reactSpec.componentDidCatch error params
    }


withLifeCycleOne :: forall props state snapshot rw
                  . One.Queue (write :: WRITE | rw) (ReactLifeCycle props state snapshot)
                 -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                 -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
withLifeCycleOne q constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        One.put q DidMount
        reactSpec.componentDidMount
    , componentWillUnmount = do
        One.put q WillUnmount
        reactSpec.componentWillUnmount
    , componentDidUpdate = \props state snapshot -> do
        One.put q $ DidUpdate {props,state,snapshot}
        reactSpec.componentDidUpdate props state snapshot
    , componentDidCatch = \error params -> do
        One.put q $ DidCatch {error, componentStack: params.componentStack}
        reactSpec.componentDidCatch error params
    }


withLifeCycleIx :: forall props state snapshot rw
                 . String
                -> IxQueue (write :: WRITE | rw) (ReactLifeCycle props state snapshot)
                -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
withLifeCycleIx k q constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        IxQueue.put q k DidMount
        reactSpec.componentDidMount
    , componentWillUnmount = do
        IxQueue.put q k WillUnmount
        reactSpec.componentWillUnmount
    , componentDidUpdate = \props state snapshot -> do
        IxQueue.put q k $ DidUpdate {props,state,snapshot}
        reactSpec.componentDidUpdate props state snapshot
    , componentDidCatch = \error params -> do
        IxQueue.put q k $ DidCatch {error, componentStack: params.componentStack}
        reactSpec.componentDidCatch error params
    }


withLifeCycleBroadcastIx :: forall props state snapshot rw
                          . Array String -- exception keys
                         -> IxQueue (write :: WRITE | rw) (ReactLifeCycle props state snapshot)
                         -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                         -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
withLifeCycleBroadcastIx ks q constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        IxQueue.broadcastExcept q ks DidMount
        reactSpec.componentDidMount
    , componentWillUnmount = do
        IxQueue.broadcastExcept q ks WillUnmount
        reactSpec.componentWillUnmount
    , componentDidUpdate = \props state snapshot -> do
        IxQueue.broadcastExcept q ks $ DidUpdate {props,state,snapshot}
        reactSpec.componentDidUpdate props state snapshot
    , componentDidCatch = \error params -> do
        IxQueue.broadcastExcept q ks $ DidCatch {error, componentStack: params.componentStack}
        reactSpec.componentDidCatch error params
    }
