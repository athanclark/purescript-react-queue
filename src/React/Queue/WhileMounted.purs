module React.Queue.WhileMounted where

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (genUUID, GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, readRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import React (ReactSpec, ReactThis)
import Queue.Types (READ)
import Queue (Queue, onQueue, delQueue)
import Queue.One (Queue, onQueue, delQueue) as One
import IxQueue (IxQueue, onIxQueue, delIxQueue)




-- | Deletes _all_ handlers from `Queue` when unmounting
whileMounted :: forall props state render eff rw a
              . Queue (read :: READ | rw) (ref :: REF | eff) a
             -> (ReactThis props state -> a -> Eff (ref :: REF | eff) Unit)
             -> ReactSpec props state render (ref :: REF | eff)
             -> ReactSpec props state render (ref :: REF | eff)
whileMounted q f reactSpec = reactSpec
  { componentDidMount = \this -> do
      unsafeCoerceEff (onQueue q (f this))
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff (delQueue q)
      reactSpec.componentWillUnmount this
  }


-- | Uses specified index
whileMountedIx :: forall props state render eff rw a
                . IxQueue (read :: READ | rw) (ref :: REF | eff) a
               -> String
               -> (ReactThis props state -> a -> Eff (ref :: REF | eff) Unit)
               -> ReactSpec props state render (ref :: REF | eff)
               -> ReactSpec props state render (ref :: REF | eff)
whileMountedIx q k f reactSpec = reactSpec
  { componentDidMount = \this -> do
      unsafeCoerceEff (onIxQueue q k (f this))
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff (unit <$ delIxQueue q k)
      reactSpec.componentWillUnmount this
  }


-- | Generates a random index - useful for broadcasted data
whileMountedIxUUID :: forall props state render eff rw a
                    . IxQueue (read :: READ | rw) (ref :: REF, uuid :: GENUUID, exception :: EXCEPTION | eff) a
                   -> (ReactThis props state -> a -> Eff (ref :: REF, uuid :: GENUUID, exception :: EXCEPTION | eff) Unit)
                   -> ReactSpec props state render (ref :: REF, uuid :: GENUUID, exception :: EXCEPTION | eff)
                   -> ReactSpec props state render (ref :: REF, uuid :: GENUUID, exception :: EXCEPTION | eff)
whileMountedIxUUID q f reactSpec = reactSpec
  { componentDidMount = \this -> do
      unsafeCoerceEff $ do
        k <- genUUID
        writeRef kRef (Just k)
        onIxQueue q (show k) (f this)
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff $ do
        mK <- readRef kRef
        case mK of
          Nothing -> throw "No UUID ref!"
          Just k -> do
            unit <$ delIxQueue q (show k)
            writeRef kRef Nothing
      reactSpec.componentWillUnmount this
  }
  where
    kRef = unsafePerformEff (newRef Nothing)


-- | Is the only handler for the singleton queue.
whileMountedOne :: forall props state render eff rw a
                 . One.Queue (read :: READ | rw) (ref :: REF | eff) a
                -> (ReactThis props state -> a -> Eff (ref :: REF | eff) Unit)
                -> ReactSpec props state render (ref :: REF | eff)
                -> ReactSpec props state render (ref :: REF | eff)
whileMountedOne q f reactSpec = reactSpec
  { componentDidMount = \this -> do
      unsafeCoerceEff (One.onQueue q (f this))
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unsafeCoerceEff (One.delQueue q)
      reactSpec.componentWillUnmount this
  }
