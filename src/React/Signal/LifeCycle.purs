module React.Signal.LifeCycle where

import Prelude (bind, discard, pure)
import Effect (Effect)
import Effect.Exception (Error)
import React (ReactSpecOptional)
import Signal.Types (READ, WRITE)
import Signal (Signal, make, set) as Signal


data ReactLifeCycle props state snapshot
  = Unmounted
  | Mounted
  | Unmounting
  | Updated {props :: props, state :: state, snapshot :: snapshot}
  | Caught {error :: Error, componentStack :: String}


withLifeCycle :: forall props state snapshot spec
               . { | ReactSpecOptional props state snapshot spec }
              -> Effect
                  { spec :: { | ReactSpecOptional props state snapshot spec }
                  , signal :: Signal.Signal (read :: READ, write :: WRITE) (ReactLifeCycle props state snapshot)
                  }
withLifeCycle reactSpec = do
  signal <- Signal.make Unmounted
  pure
    { signal
    , spec: reactSpec
        { componentDidMount = do
            Signal.set Mounted signal
            reactSpec.componentDidMount
        , componentWillUnmount = do
            Signal.set Unmounting signal
            reactSpec.componentWillUnmount
        , componentDidUpdate = \props state snapshot -> do
            Signal.set (Updated {props,state,snapshot}) signal
            reactSpec.componentDidUpdate props state snapshot
        , componentDidCatch = \error params -> do
            Signal.set (Caught {error, componentStack: params.componentStack}) signal
            reactSpec.componentDidCatch error params
        }
    }
