module React.Signal.WhileMounted where

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React (ReactSpecOptional, ReactThis)
import Signal.Types (READ)
import Signal.Internal as Signal
import IxSignal.Internal as IxSignal


whileMounted :: forall props state snapshot spec rw a
              . Signal.Signal (read :: READ | rw) a
             -> (a -> Effect Unit)
             -> { | ReactSpecOptional props state snapshot spec }
             -> { | ReactSpecOptional props state snapshot spec }
whileMounted sig f reactSpec = reactSpec
  { componentDidMount = do
      Signal.subscribeLight f sig
      reactSpec.componentDidMount
  , componentWillUnmount = do
      Signal.clear sig
      reactSpec.componentWillUnmount
  }


whileMountedIx :: forall props state snapshot spec rw a
                . IxSignal.IxSignal (read :: READ | rw) a
               -> String
               -> (a -> Effect Unit)
               -> { | ReactSpecOptional props state snapshot spec }
               -> { | ReactSpecOptional props state snapshot spec }
whileMountedIx sig k f reactSpec = reactSpec
  { componentDidMount = do
      IxSignal.subscribeIxLight f k sig
      reactSpec.componentDidMount
  , componentWillUnmount = do
      IxSignal.delete k sig
      reactSpec.componentWillUnmount
  }


whileMountedIxDiff :: forall props state snapshot spec rw a
                    . Eq a
                   => IxSignal.IxSignal (read :: READ | rw) a
                   -> String
                   -> (a -> Effect Unit)
                   -> { | ReactSpecOptional props state snapshot spec }
                   -> { | ReactSpecOptional props state snapshot spec }
whileMountedIxDiff sig k f reactSpec = reactSpec
  { componentDidMount = do
      IxSignal.subscribeIxDiffLight f k sig
      reactSpec.componentDidMount
  , componentWillUnmount = do
      IxSignal.delete k sig
      reactSpec.componentWillUnmount
  }



whileMountedIxUUID :: forall props state snapshot spec rw a
                    . IxSignal.IxSignal (read :: READ | rw) a
                   -> (a -> Effect Unit)
                   -> { | ReactSpecOptional props state snapshot spec }
                   -> { | ReactSpecOptional props state snapshot spec }
whileMountedIxUUID sig f reactSpec = reactSpec
  { componentDidMount = do
      k <- genUUID
      Ref.write (Just k) kRef
      IxSignal.subscribeIxLight f (show k) sig
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



whileMountedIxDiffUUID :: forall props state snapshot spec rw a
                        . Eq a
                       => IxSignal.IxSignal (read :: READ | rw) a
                       -> (a -> Effect Unit)
                       -> { | ReactSpecOptional props state snapshot spec }
                       -> { | ReactSpecOptional props state snapshot spec }
whileMountedIxDiffUUID sig f reactSpec = reactSpec
  { componentDidMount = do
      k <- genUUID
      Ref.write (Just k) kRef
      IxSignal.subscribeIxDiffLight f (show k) sig
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
