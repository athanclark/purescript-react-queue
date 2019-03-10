module React.Signal.WhileMounted where

import Prelude (Unit, class Eq, bind, discard, ($), pure, unit)
import Effect (Effect)
import React
  ( ReactClassConstructor, ReactThis, ComponentDidMount, ComponentWillUnmount, ReactElement, ReactClass, ReactSpecRequired
  , createLeafElement, createElement, component, setState, getState, createContext)
import Signal.Types (READ)
import Signal (Signal, subscribeLight, clear, get) as Signal
import IxSignal (IxSignal, subscribeLight, subscribeDiffLight, delete, get) as IxSignal


type Mounted r = (componentDidMount :: ComponentDidMount, componentWillUnmount :: ComponentWillUnmount | r)


whileMounted :: forall props state spec rw a
              . Signal.Signal (read :: READ | rw) a
             -> (ReactThis props state -> a -> Effect Unit)
             -> ReactClassConstructor props state (Mounted spec)
             -> ReactClassConstructor props state (Mounted spec)
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


whileMountedIx :: forall props state spec rw a
                . IxSignal.IxSignal (read :: READ | rw) a
               -> String
               -> (ReactThis props state -> a -> Effect Unit)
               -> ReactClassConstructor props state (Mounted spec)
               -> ReactClassConstructor props state (Mounted spec)
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


whileMountedIxDiff :: forall props state spec rw a
                    . Eq a
                   => IxSignal.IxSignal (read :: READ | rw) a
                   -> String
                   -> (ReactThis props state -> a -> Effect Unit)
                   -> ReactClassConstructor props state (Mounted spec)
                   -> ReactClassConstructor props state (Mounted spec)
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


-- | Provider takes the component name as the first argument
signalToProvider :: forall a
                  . Signal.Signal (read :: READ) a
                 -> Effect
                      { consumer :: (a -> ReactElement) -> ReactElement
                      , provider :: String -> Array ReactElement -> ReactElement
                      }
signalToProvider sig = do
  initValue <- Signal.get sig
  let {consumer,provider} = createContext initValue
  pure
    { consumer: \f -> createLeafElement consumer {children: f}
    , provider: \name children ->
      let c :: ReactClass {}
          c = component name constructor
            where
              constructor :: ReactClassConstructor {} {value :: a} (Mounted (ReactSpecRequired {value :: a} ()))
              constructor =
                let handleValueChange :: ReactThis {} {value :: a} -> a -> Effect Unit
                    handleValueChange this value = setState this {value}
                in  whileMounted sig handleValueChange constructor'
                where
                  constructor' this = do
                    initValue' <- Signal.get sig
                    pure
                      { state: {value: initValue'}
                      , render: do
                          {value} <- getState this
                          pure (createElement provider {value} children)
                      , componentDidMount: pure unit
                      , componentWillUnmount: pure unit
                      }
      in  createLeafElement c {}
    }


-- | Provider takes the index of the signal to bind to as the first argument, the second is the component name
ixSignalToProvider :: forall a
                    . IxSignal.IxSignal (read :: READ) a
                   -> Effect
                        { consumer :: (a -> ReactElement) -> ReactElement
                        , provider :: String -> String -> Array ReactElement -> ReactElement
                        }
ixSignalToProvider sig = do
  initValue <- IxSignal.get sig
  let {consumer,provider} = createContext initValue
  pure
    { consumer: \f -> createLeafElement consumer {children: f}
    , provider: \ix name children ->
      let c :: ReactClass {}
          c = component name constructor
            where
              constructor :: ReactClassConstructor {} {value :: a} (Mounted (ReactSpecRequired {value :: a} ()))
              constructor =
                let handleValueChange :: ReactThis {} {value :: a} -> a -> Effect Unit
                    handleValueChange this value = setState this {value}
                in  whileMountedIx sig ix handleValueChange constructor'
                where
                  constructor' this = do
                    initValue' <- IxSignal.get sig
                    pure
                      { state: {value: initValue'}
                      , render: do
                          {value} <- getState this
                          pure (createElement provider {value} children)
                      , componentDidMount: pure unit
                      , componentWillUnmount: pure unit
                      }
      in  createLeafElement c {}
    }


-- | Provider takes the index of the signal to bind to as the first argument, the second is the component name
ixSignalDiffToProvider :: forall a
                        . Eq a
                       => IxSignal.IxSignal (read :: READ) a
                       -> Effect
                            { consumer :: (a -> ReactElement) -> ReactElement
                            , provider :: String -> String -> Array ReactElement -> ReactElement
                            }
ixSignalDiffToProvider sig = do
  initValue <- IxSignal.get sig
  let {consumer,provider} = createContext initValue
  pure
    { consumer: \f -> createLeafElement consumer {children: f}
    , provider: \ix name children ->
      let c :: ReactClass {}
          c = component name constructor
            where
              constructor :: ReactClassConstructor {} {value :: a} (Mounted (ReactSpecRequired {value :: a} ()))
              constructor =
                let handleValueChange :: ReactThis {} {value :: a} -> a -> Effect Unit
                    handleValueChange this value = setState this {value}
                in  whileMountedIxDiff sig ix handleValueChange constructor'
                where
                  constructor' this = do
                    initValue' <- IxSignal.get sig
                    pure
                      { state: {value: initValue'}
                      , render: do
                          {value} <- getState this
                          pure (createElement provider {value} children)
                      , componentDidMount: pure unit
                      , componentWillUnmount: pure unit
                      }
      in  createLeafElement c {}
    }
