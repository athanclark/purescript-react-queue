module React.Signal.WhileMounted where

import Prelude (Unit, class Eq, bind, discard, ($), pure, show)
import Data.Maybe (Maybe (..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (new, write, read) as Ref
import Effect.Unsafe (unsafePerformEffect)
import React (ReactSpecAll, ReactClassConstructor, ReactThis)
import Signal.Types (READ)
import Signal (Signal, subscribeLight, clear) as Signal
import IxSignal (IxSignal, subscribeIxLight, subscribeIxDiffLight, delete) as IxSignal


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
        IxSignal.subscribeIxLight (f this) k sig
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
        IxSignal.subscribeIxDiffLight (f this) k sig
        reactSpec.componentDidMount
    , componentWillUnmount = do
        IxSignal.delete k sig
        reactSpec.componentWillUnmount
    }



whileMountedIxUUID :: forall props state snapshot rw a
                    . IxSignal.IxSignal (read :: READ | rw) a
                   -> (ReactThis props state -> a -> Effect Unit)
                   -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                   -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
whileMountedIxUUID sig f constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        k <- genUUID
        Ref.write (Just k) kRef
        IxSignal.subscribeIxLight (f this) (show k) sig
        reactSpec.componentDidMount
    , componentWillUnmount = do
        mk <- Ref.read kRef
        case mk of
          Nothing -> throw "No UUID ref!"
          Just k -> do
            IxSignal.delete (show k) sig
            Ref.write Nothing kRef
        reactSpec.componentWillUnmount
  }
  where
    kRef = unsafePerformEffect (Ref.new Nothing)



whileMountedIxDiffUUID :: forall props state snapshot rw a
                        . Eq a
                       => IxSignal.IxSignal (read :: READ | rw) a
                       -> (ReactThis props state -> a -> Effect Unit)
                       -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                       -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
whileMountedIxDiffUUID sig f constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        k <- genUUID
        Ref.write (Just k) kRef
        IxSignal.subscribeIxDiffLight (f this) (show k) sig
        reactSpec.componentDidMount
    , componentWillUnmount = do
        mk <- Ref.read kRef
        case mk of
          Nothing -> throw "No UUID ref!"
          Just k -> do
            IxSignal.delete (show k) sig
            Ref.write Nothing kRef
        reactSpec.componentWillUnmount
    }
  where
    kRef = unsafePerformEffect (Ref.new Nothing)
