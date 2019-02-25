module React.Queue.WhileMounted where

import Prelude (Unit, bind, discard, pure, ($), (<$), when, unit)
import Effect (Effect)
import Effect.Ref (new, read, write) as Ref
import Effect.Unsafe (unsafePerformEffect)
import React (ReactSpecAll, ReactClassConstructor, ReactThis)
import Queue.Types (READ)
import Queue (Queue)
import Queue (on, del, new, put) as Queue
import Queue.One (Queue, on, del, new, put) as One
import IxQueue (IxQueue)
import IxQueue (on, del, new, broadcast) as IxQueue




-- | Deletes _all_ handlers from `Queue` when unmounting
whileMounted :: forall props state snapshot rw a
              . Queue (read :: READ | rw) a
             -> (ReactThis props state -> a -> Effect Unit)
             -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
             -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
whileMounted q f constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        Queue.on q (f this)
        reactSpec.componentDidMount
    , componentWillUnmount = do
        Queue.del q
        reactSpec.componentWillUnmount
    }


drainingWhileUnmounted :: forall props state snapshot rw a
                        . Queue (read :: READ | rw) a
                       -> (ReactThis props state -> a -> Effect Unit)
                       -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                       -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
drainingWhileUnmounted q f constructor =
  whileMounted q' f $ \this -> do
    reactSpec <- constructor this
    pure $ reactSpec
      { componentDidMount = do
          Ref.write true isMountedRef
          reactSpec.componentDidMount
      , componentWillUnmount = do
          Ref.write false isMountedRef
          reactSpec.componentWillUnmount
      }
  where
    q' = unsafePerformEffect Queue.new
    isMountedRef = unsafePerformEffect (Ref.new false)
    _ = unsafePerformEffect $ Queue.on q \x -> do
      isMounted <- Ref.read isMountedRef
      when isMounted (Queue.put q' x)


-- | Uses specified index
whileMountedIx :: forall props state snapshot rw a
                . IxQueue (read :: READ | rw) a
               -> String
               -> (ReactThis props state-> a -> Effect Unit)
               -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
               -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
whileMountedIx q k f constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        IxQueue.on q k (f this)
        reactSpec.componentDidMount
    , componentWillUnmount = do
        unit <$ IxQueue.del q k
        reactSpec.componentWillUnmount
    }


drainingWhileUnmountedIx :: forall props state snapshot rw a
                          . IxQueue (read :: READ | rw) a
                         -> String
                         -> (ReactThis props state -> a -> Effect Unit)
                         -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                         -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
drainingWhileUnmountedIx q k f constructor =
  whileMountedIx q' k f $ \this -> do
    reactSpec <- constructor this
    pure $ reactSpec
      { componentDidMount = do
          Ref.write true isMountedRef
          reactSpec.componentDidMount
      , componentWillUnmount = do
          Ref.write false isMountedRef
          reactSpec.componentWillUnmount
      }
  where
    q' = unsafePerformEffect IxQueue.new
    isMountedRef = unsafePerformEffect (Ref.new false)
    _ = unsafePerformEffect $ IxQueue.on q k \x -> do
      isMounted <- Ref.read isMountedRef
      when isMounted (IxQueue.broadcast q' x)


-- | Is the only handler for the singleton queue.
whileMountedOne :: forall props state snapshot rw a
                 . One.Queue (read :: READ | rw) a
                -> (ReactThis props state -> a -> Effect Unit)
                -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
whileMountedOne q f constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        One.on q (f this)
        reactSpec.componentDidMount
    , componentWillUnmount = do
        One.del q
        reactSpec.componentWillUnmount
    }


drainingWhileUnmountedOne :: forall props state snapshot rw a
                           . One.Queue (read :: READ | rw) a
                          -> (ReactThis props state -> a -> Effect Unit)
                          -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
                          -> ReactClassConstructor props state (ReactSpecAll props state snapshot)
drainingWhileUnmountedOne q f constructor =
  whileMountedOne q' f $ \this -> do
    reactSpec <- constructor this
    pure $ reactSpec
      { componentDidMount = do
          Ref.write true isMountedRef
          reactSpec.componentDidMount
      , componentWillUnmount = do
          Ref.write false isMountedRef
          reactSpec.componentWillUnmount
      }
  where
    q' = unsafePerformEffect One.new
    isMountedRef = unsafePerformEffect (Ref.new false)
    _ = unsafePerformEffect $ One.on q \x -> do
      isMounted <- Ref.read isMountedRef
      when isMounted (One.put q' x)
