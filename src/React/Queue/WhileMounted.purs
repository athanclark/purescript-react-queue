module React.Queue.WhileMounted where

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React (ReactSpecOptional, ReactThis)
import Queue.Types (READ)
import Queue (Queue)
import Queue as Queue
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue




-- | Deletes _all_ handlers from `Queue` when unmounting
whileMounted :: forall props state snapshot rw a spec
              . Queue (read :: READ | rw) a
             -> (a -> Effect Unit)
             -> { | ReactSpecOptional props state snapshot spec }
             -> { | ReactSpecOptional props state snapshot spec }
whileMounted q f reactSpec = reactSpec
  { componentDidMount = do
      Queue.on q f
      reactSpec.componentDidMount
  , componentWillUnmount = do
      Queue.del q
      reactSpec.componentWillUnmount
  }


drainingWhileUnmounted :: forall props state snapshot rw a spec
                       . Queue (read :: READ | rw) a
                       -> (ReactThis props state -> a -> Effect Unit)
                       -> { | ReactSpecOptional props state snapshot spec }
                       -> { | ReactSpecOptional props state snapshot spec }
drainingWhileUnmounted q f reactSpec = whileMounted q' f $
  reactSpec
    { componentDidMount = \this -> do
        Ref.write isMountedRef true
        reactSpec.componentDidMount this
    , componentWillUnmount = \this -> do
        Ref.write isMountedRef false
        reactSpec.componentWillUnmount this
    }
  where
    q' = unsafePerformEffect Queue.new
    isMountedRef = unsafePerformEffect (Ref.new false)
    _ = unsafePerformEffect $ Queue.on q \x -> do
      isMounted <- Ref.read isMountedRef
      when isMounted (Queue.put q' x)


-- | Uses specified index
whileMountedIx :: forall props state snapshot rw a spec
                . IxQueue (read :: READ | rw) a
               -> String
               -> (ReactThis props state -> a -> Effect Unit)
               -> { | ReactSpecOptional props state snapshot spec }
               -> { | ReactSpecOptional props state snapshot spec }
whileMountedIx q k f reactSpec = reactSpec
  { componentDidMount = \this -> do
      IxQueue.on q k (f this)
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      unit <$ IxQueue.del q k
      reactSpec.componentWillUnmount this
  }


drainingWhileUnmountedIx :: forall props state snapshot rw a spec
                          . IxQueue (read :: READ | rw) a
                          -> String
                          -> (ReactThis props state -> a -> Effect Unit)
                          -> { | ReactSpecOptional props state snapshot spec }
                          -> { | ReactSpecOptional props state snapshot spec }
drainingWhileUnmountedIx q k f reactSpec = whileMountedIx q' k f $
  reactSpec
    { componentDidMount = \this -> do
        Ref.write isMountedRef true
        reactSpec.componentDidMount this
    , componentWillUnmount = \this -> do
        Ref.write isMountedRef false
        reactSpec.componentWillUnmount this
    }
  where
    q' = unsafePerformEffect IxQueue.new
    isMountedRef = unsafePerformEffect (Ref.new false)
    _ = unsafePerformEffect $ IxQueue.on q k \x -> do
      isMounted <- Ref.read isMountedRef
      when isMounted (IxQueue.broadcast q' x)


-- | Generates a random index - useful for broadcasted data
whileMountedIxUUID :: forall props state snapshot rw a spec
                    . IxQueue (read :: READ | rw) a
                   -> (ReactThis props state -> a -> Effect Unit)
                   -> { | ReactSpecOptional props state snapshot spec }
                   -> { | ReactSpecOptional props state snapshot spec }
whileMountedIxUUID q f reactSpec = reactSpec
  { componentDidMount = \this -> do
      k <- genUUID
      Ref.write kRef (Just k)
      IxQueue.on q (show k) (f this)
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      mK <- Ref.read kRef
      case mK of
        Nothing -> throw "No UUID ref!"
        Just k -> do
          unit <$ IxQueue.del q (show k)
          Ref.write kRef Nothing
      reactSpec.componentWillUnmount this
  }
  where
    kRef = unsafePerformEffect (Ref.new Nothing)


drainingWhileUnmountedIxUUID :: forall props state snapshot rw a spec
                              . IxQueue (read :: READ | rw) a
                             -> (ReactThis props state -> a -> Effect Unit)
                             -> { | ReactSpecOptional props state snapshot spec }
                             -> { | ReactSpecOptional props state snapshot spec }
drainingWhileUnmountedIxUUID q f reactSpec = whileMountedIxUUID q' f $
  reactSpec
    { componentDidMount = \this -> do
        Ref.write isMountedRef true
        reactSpec.componentDidMount this
    , componentWillUnmount = \this -> do
        Ref.write isMountedRef false
        reactSpec.componentWillUnmount this
    }
  where
    q' = unsafePerformEffect IxQueue.new
    isMountedRef = unsafePerformEffect (Ref.new false)
    _ = unsafePerformEffect $ do
      k <- show <$> genUUID
      IxQueue.on q k \x -> do
        isMounted <- Ref.read isMountedRef
        when isMounted (IxQueue.broadcast q' x)


-- | Is the only handler for the singleton queue.
whileMountedOne :: forall props state snapshot rw a spec
                 . One.Queue (read :: READ | rw) a
                -> (ReactThis props state -> a -> Effect Unit)
                -> { | ReactSpecOptional props state snapshot spec }
                -> { | ReactSpecOptional props state snapshot spec }
whileMountedOne q f reactSpec = reactSpec
  { componentDidMount = \this -> do
      One.on q (f this)
      reactSpec.componentDidMount this
  , componentWillUnmount = \this -> do
      One.del q
      reactSpec.componentWillUnmount this
  }


drainingWhileUnmountedOne :: forall props state snapshot rw a spec
                           . One.Queue (read :: READ | rw) a
                          -> (ReactThis props state -> a -> Effect Unit)
                          -> { | ReactSpecOptional props state snapshot spec }
                          -> { | ReactSpecOptional props state snapshot spec }
drainingWhileUnmountedOne q f reactSpec = whileMountedOne q' f $
  reactSpec
    { componentDidMount = \this -> do
        Ref.write isMountedRef true
        reactSpec.componentDidMount this
    , componentWillUnmount = \this -> do
        Ref.write isMountedRef false
        reactSpec.componentWillUnmount this
    }
  where
    q' = unsafePerformEffect One.new
    isMountedRef = unsafePerformEffect (Ref.new false)
    _ = unsafePerformEffect $ One.on q \x -> do
      isMounted <- Ref.read isMountedRef
      when isMounted $ One.put q' x
