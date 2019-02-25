module React.Signal.WhileMounted where

import Prelude (Unit, class Eq, bind, discard, ($), pure)
import Effect (Effect)
import React (ReactSpecAll, ReactClassConstructor, ReactThis)
import Signal.Types (READ)
import Signal (Signal, subscribeLight, clear) as Signal
import IxSignal (IxSignal, subscribeLight, subscribeDiffLight, delete) as IxSignal


whileMounted :: forall props state snapshot rw a
              . Signal.Signal (read :: READ | rw) a
             -> (ReactThis props state -> a -> Effect Unit)
             -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
             -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
whileMounted sig f constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        Signal.subscribeLight (f this) sig
        reactSpec.componentDidMount
    , componentWillUnmount = do
        Signal.clear sig
        reactSpec.componentWillUnmount
    }


whileMountedIx :: forall props state snapshot rw a
                . IxSignal.IxSignal (read :: READ | rw) a
               -> String
               -> (ReactThis props state -> a -> Effect Unit)
               -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
               -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
whileMountedIx sig k f constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        IxSignal.subscribeLight k (f this) sig
        reactSpec.componentDidMount
    , componentWillUnmount = do
        IxSignal.delete k sig
        reactSpec.componentWillUnmount
    }


whileMountedIxDiff :: forall props state snapshot rw a
                    . Eq a
                   => IxSignal.IxSignal (read :: READ | rw) a
                   -> String
                   -> (ReactThis props state -> a -> Effect Unit)
                   -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                   -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
whileMountedIxDiff sig k f constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        IxSignal.subscribeDiffLight k (f this) sig
        reactSpec.componentDidMount
    , componentWillUnmount = do
        IxSignal.delete k sig
        reactSpec.componentWillUnmount
    }
