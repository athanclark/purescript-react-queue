module React.Signal.WhileMounted where

import Prelude (Unit, class Eq, bind, discard, ($), pure, unit)
import Effect (Effect)
import React
  ( ReactClassConstructor, ReactThis, ComponentDidMount, ComponentWillUnmount, ContextProvider, ReactElement, ReactClass, ReactSpecRequired
  , createLeafElement, createElement, component, setState, getState)
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



signalToProvider :: forall a
                  . Signal.Signal (read :: READ) a
                 -> ContextProvider a
                 -> String -- ^ Component name
                 -> Array ReactElement
                 -> ReactElement
signalToProvider sig prov name children = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component name constructor
      where
        constructor :: ReactClassConstructor {} {value :: a} (Mounted (ReactSpecRequired {value :: a} ()))
        constructor =
          let handleValueChange :: ReactThis {} {value :: a} -> a -> Effect Unit
              handleValueChange this value = setState this {value}
          in  whileMounted sig handleValueChange constructor'
          where
            constructor' this = do
              initValue <- Signal.get sig
              pure
                { state: {value: initValue}
                , render: do
                    {value} <- getState this
                    pure (createElement prov {value} children)
                , componentDidMount: pure unit
                , componentWillUnmount: pure unit
                }


ixSignalToProvider :: forall a
                    . IxSignal.IxSignal (read :: READ) a
                   -> ContextProvider a
                   -> String -- ^ Signal Index
                   -> String -- ^ Component name
                   -> Array ReactElement
                   -> ReactElement
ixSignalToProvider sig prov ix name children = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component name constructor
      where
        constructor :: ReactClassConstructor {} {value :: a} (Mounted (ReactSpecRequired {value :: a} ()))
        constructor =
          let handleValueChange :: ReactThis {} {value :: a} -> a -> Effect Unit
              handleValueChange this value = setState this {value}
          in  whileMountedIx sig ix handleValueChange constructor'
          where
            constructor' this = do
              initValue <- IxSignal.get sig
              pure
                { state: {value: initValue}
                , render: do
                    {value} <- getState this
                    pure (createElement prov {value} children)
                , componentDidMount: pure unit
                , componentWillUnmount: pure unit
                }


ixSignalDiffToProvider :: forall a
                        . Eq a
                       => IxSignal.IxSignal (read :: READ) a
                       -> ContextProvider a
                       -> String -- ^ Signal Index
                       -> String -- ^ Component name
                       -> Array ReactElement
                       -> ReactElement
ixSignalDiffToProvider sig prov ix name children = createLeafElement c {}
  where
    c :: ReactClass {}
    c = component name constructor
      where
        constructor :: ReactClassConstructor {} {value :: a} (Mounted (ReactSpecRequired {value :: a} ()))
        constructor =
          let handleValueChange :: ReactThis {} {value :: a} -> a -> Effect Unit
              handleValueChange this value = setState this {value}
          in  whileMountedIxDiff sig ix handleValueChange constructor'
          where
            constructor' this = do
              initValue <- IxSignal.get sig
              pure
                { state: {value: initValue}
                , render: do
                    {value} <- getState this
                    pure (createElement prov {value} children)
                , componentDidMount: pure unit
                , componentWillUnmount: pure unit
                }
