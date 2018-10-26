module React.Signal.WhileMounted where

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React (class ReactComponentSpec, ReactClassConstructor, ReactThis)
import Signal.Types (READ)
import Signal.Internal as Signal
import IxSignal.Internal as IxSignal


whileMounted :: forall props state snapshot given spec rw a
              . ReactComponentSpec { | props } { | state } snapshot given spec
             => Signal.Signal (read :: READ | rw) a
             -> (ReactThis props state -> a -> Effect Unit)
             -> ReactClassConstructor { | props } { | state } given
             -> ReactClassConstructor { | props } { | state } given
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


whileMountedIx :: forall props state snapshot given spec rw a
                . ReactComponentSpec { | props } { | state } snapshot given spec
               => IxSignal.IxSignal (read :: READ | rw) a
               -> String
               -> (ReactThis props state -> a -> Effect Unit)
               -> ReactClassConstructor { | props } { | state } given
               -> ReactClassConstructor { | props } { | state } given
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


whileMountedIxDiff :: forall props state snapshot given spec rw a
                    . Eq a
                   => ReactComponentSpec { | props } { | state } snapshot given spec
                   => IxSignal.IxSignal (read :: READ | rw) a
                   -> String
                   -> (ReactThis props state -> a -> Effect Unit)
                   -> ReactClassConstructor { | props } { | state } given
                   -> ReactClassConstructor { | props } { | state } given
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



whileMountedIxUUID :: forall props state snapshot given spec rw a
                    . ReactComponentSpec { | props } { | state } snapshot given spec
                   => IxSignal.IxSignal (read :: READ | rw) a
                   -> (ReactThis props state -> a -> Effect Unit)
                   -> ReactClassConstructor { | props } { | state } given
                   -> ReactClassConstructor { | props } { | state } given
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



whileMountedIxDiffUUID :: forall props state snapshot given spec rw a
                        . Eq a
                       => ReactComponentSpec { | props } { | state } snapshot given spec
                       => IxSignal.IxSignal (read :: READ | rw) a
                       -> (ReactThis props state -> a -> Effect Unit)
                       -> ReactClassConstructor { | props } { | state } given
                       -> ReactClassConstructor { | props } { | state } given
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
