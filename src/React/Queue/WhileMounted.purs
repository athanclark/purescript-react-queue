module React.Queue.WhileMounted where

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React (ReactSpecAll, ReactClassConstructor, ReactThis)
import Queue.Types (READ)
import Queue (Queue)
import Queue as Queue
import Queue.One as One
import IxQueue (IxQueue)
import IxQueue as IxQueue




-- | Deletes _all_ handlers from `Queue` when unmounting
whileMounted :: forall props state snapshot rw a given spec
              . ReactComponentSpec { | props } { | state } snapshot given spec
             => Queue (read :: READ | rw) a
             -> (ReactThis prop state -> a -> Effect Unit)
             -> ReactClassConstructor { | props } { | state } given
             -> ReactClassConstructor { | props } { | state } given
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


drainingWhileUnmounted :: forall props state snapshot rw a given spec
                        . ReactComponentSpec { | props } { | state } snapshot given spec
                       => Queue (read :: READ | rw) a
                       -> (ReactThis props state -> a -> Effect Unit)
                       -> ReactClassConstructor { | props } { | state } given
                       -> ReactClassConstructor { | props } { | state } given
drainingWhileUnmounted q f constructor =
  whileMounted q' f $ \this -> do
    reactSpec <- constructor this
    pure $ reactSpec
      { componentDidMount = do
          Ref.write isMountedRef true
          reactSpec.componentDidMount
      , componentWillUnmount = do
          Ref.write isMountedRef false
          reactSpec.componentWillUnmount
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


drainingWhileUnmountedIx :: forall props state snapshot rw a given spec
                          . ReactComponentSpec { | props } { | state } snapshot given spec
                         => IxQueue (read :: READ | rw) a
                         -> String
                         -> (ReactThis props state -> a -> Effect Unit)
                         -> ReactClassConstructor { | props } { | state } given
                         -> ReactClassConstructor { | props } { | state } given
drainingWhileUnmountedIx q k f constructor =
  whileMountedIx q' k f $ \this -> do
    reactSpec <- constructor this
    pure $ reactSpec
      { componentDidMount = do
          Ref.write isMountedRef true
          reactSpec.componentDidMount
      , componentWillUnmount = do
          Ref.write isMountedRef false
          reactSpec.componentWillUnmount
      }
  where
    q' = unsafePerformEffect IxQueue.new
    isMountedRef = unsafePerformEffect (Ref.new false)
    _ = unsafePerformEffect $ IxQueue.on q k \x -> do
      isMounted <- Ref.read isMountedRef
      when isMounted (IxQueue.broadcast q' x)


-- | Generates a random index - useful for broadcasted data
whileMountedIxUUID :: forall props state snapshot rw a given spec
                    . ReactComponentSpec { | props } { | state } snapshot given spec
                   => IxQueue (read :: READ | rw) a
                   -> (ReactThis props state -> a -> Effect Unit)
                   -> ReactClassConstructor { | props } { | state } given
                   -> ReactClassConstructor { | props } { | state } given
whileMountedIxUUID q f constructor = \this -> do
  reactSpec <- constructor this
  pure $ reactSpec
    { componentDidMount = do
        k <- genUUID
        Ref.write kRef (Just k)
        IxQueue.on q (show k) (f this)
        reactSpec.componentDidMount
    , componentWillUnmount = do
        mK <- Ref.read kRef
        case mK of
          Nothing -> throw "No UUID ref!"
          Just k -> do
            unit <$ IxQueue.del q (show k)
            Ref.write kRef Nothing
        reactSpec.componentWillUnmount
    }
  where
    kRef = unsafePerformEffect (Ref.new Nothing)


drainingWhileUnmountedIxUUID :: forall props state snapshot rw a given spec
                              . ReactComponentSpec { | props } { | state } snapshot given spec
                             => IxQueue (read :: READ | rw) a
                             -> (ReactThis props state -> a -> Effect Unit)
                             -> ReactClassConstructor { | props } { | state } given
                             -> ReactClassConstructor { | props } { | state } given
drainingWhileUnmountedIxUUID q f constructor =
  whileMountedIxUUID q' f $ \this -> do
    reactSpec <- constructor this
    pure $ reactSpec
      { componentDidMount = do
          Ref.write isMountedRef true
          reactSpec.componentDidMount
      , componentWillUnmount = do
          Ref.write isMountedRef false
          reactSpec.componentWillUnmount
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
whileMountedOne :: forall props state snapshot rw a given spec
                 . ReactComponentSpec { | props } { | state } snapshot given spec
                => One.Queue (read :: READ | rw) a
                -> (ReactThis { | props } { | state } -> a -> Effect Unit)
                -> ReactClassConstructor { | props } { | state } given
                -> ReactClassConstructor { | props } { | state } given
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


drainingWhileUnmountedOne :: forall props state snapshot rw a given spec
                           . ReactComponentSpec { | props } { | state } snapshot given spec
                          => One.Queue (read :: READ | rw) a
                          -> (ReactThis props state -> a -> Effect Unit)
                          -> ReactClassConstructor { | props } { | state } given
                          -> ReactClassConstructor { | props } { | state } given
drainingWhileUnmountedOne q f constructor =
  whileMountedOne q' f $ \this -> do
    reactSpec <- constructor this
    pure $ reactSpec
      { componentDidMount = do
          Ref.write isMountedRef true
          reactSpec.componentDidMount
      , componentWillUnmount = do
          Ref.write isMountedRef false
          reactSpec.componentWillUnmount
      }
  where
    q' = unsafePerformEffect One.new
    isMountedRef = unsafePerformEffect (Ref.new false)
    _ = unsafePerformEffect $ One.on q \x -> do
      isMounted <- Ref.read isMountedRef
      when isMounted $ One.put q' x
