module React.Signal.WhileMounted where

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (genUUID, GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import React (ReactSpec, ReactThis)
import Signal.Internal as Signal
import IxSignal.Internal as IxSignal


whileMounted :: forall props state render eff a
              . Signal.Signal (ref :: REF | eff) a
             -> (ReactThis props state -> a -> Eff (ref :: REF | eff) Unit)
             -> ReactSpec props state render (ref :: REF | eff)
             -> ReactSpec props state render (ref :: REF | eff)
whileMounted sig f reactSpec = reactSpec
  { componentDidMount = \this -> do
      unsafeCoerceEff (Signal.subscribe (f this) sig)
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff (Signal.clear sig)
      reactSpec.componentWillUnmount this
  }


whileMountedIx :: forall props state render eff a
                . IxSignal.IxSignal (ref :: REF | eff) a
               -> String
               -> (ReactThis props state -> a -> Eff (ref :: REF | eff) Unit)
               -> ReactSpec props state render (ref :: REF | eff)
               -> ReactSpec props state render (ref :: REF | eff)
whileMountedIx sig k f reactSpec = reactSpec
  { componentDidMount = \this -> do
      unsafeCoerceEff (IxSignal.subscribeIx (f this) k sig)
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff (IxSignal.delete k sig)
      reactSpec.componentWillUnmount this
  }



whileMountedIxUUID :: forall props state render eff a
                    . IxSignal.IxSignal (ref :: REF, uuid :: GENUUID, exception :: EXCEPTION | eff) a
                   -> (ReactThis props state -> a -> Eff (ref :: REF , uuid :: GENUUID, exception :: EXCEPTION | eff) Unit)
                   -> ReactSpec props state render (ref :: REF, uuid :: GENUUID, exception :: EXCEPTION | eff)
                   -> ReactSpec props state render (ref :: REF, uuid :: GENUUID, exception :: EXCEPTION | eff)
whileMountedIxUUID sig f reactSpec = reactSpec
  { componentDidMount = \this -> do
      unsafeCoerceEff $ do
        k <- genUUID
        writeRef kRef (Just k)
        IxSignal.subscribeIx (f this) (show k) sig
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff $ do
        mk <- readRef kRef
        case mk of
          Nothing -> throw "No UUID ref!"
          Just k -> do
            IxSignal.delete (show k) sig
            writeRef kRef Nothing
      reactSpec.componentWillUnmount this
  }
  where
    kRef = unsafePerformEff (newRef Nothing)
