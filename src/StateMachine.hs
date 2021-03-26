{-# LANGUAGE ConstraintKinds, DataKinds, DeriveAnyClass, EmptyCase, 
  FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, 
  InstanceSigs, LambdaCase, PartialTypeSignatures, PolyKinds, RankNTypes, 
  ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, 
  TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module StateMachine where

import Data.Kind
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeLits


$(singletons [d|
  data DoorState = Opened | Closed
    deriving (Show, Read, Eq)

  data MoveState = Stopped | Up | Down
    deriving (Show, Read, Eq)
  |])

$(singletonsOnly [d|
  nextFloor :: MoveState -> Nat -> Nat
  nextFloor Stopped f = f
  nextFloor Up f = f + 1
  nextFloor Down f =
    if f > 0
      then f - 1
      else 0

  nextMoveState :: MoveState -> Nat -> MoveState
  nextMoveState Stopped _ = Stopped
  nextMoveState Up _ = Up
  nextMoveState Down f =
    if f <= 1
      then Stopped
      else Down
  |])

infixr 2 :>>

type Elevator = (DoorState, MoveState, Nat)

type family Moving (m :: MoveState) :: Constraint where
  Moving Up = ()
  Moving Down = ()

data Action (s :: Elevator) (s' :: Elevator) :: Type where
  Open ::
    Action
      '(Closed, Stopped, f)
      '(Opened, Stopped, f)
  Close ::
    Action
      '(Opened, Stopped, f)
      '(Closed, Stopped, f)
  Move ::
    Moving m =>
    Sing (m :: MoveState) ->
    Action
      '(Closed, Stopped, f)
      '(Closed, NextMoveState m f, NextFloor m f)
  Stop ::
    Action
      '(Closed, m, f)
      '(Closed, Stopped, f)
  Wait ::
    Action
      '(d, m, f)
      '(d, NextMoveState m f, NextFloor m f)
  (:>>) :: Action s1 s2 -> Action s2 s3 -> Action s1 s3

type ElevatorProgram f f' =
  Action '(Opened, Stopped, f) '(Opened, Stopped, f')

program :: ElevatorProgram 0 0
program =
  Close
  :>> Move SUp
  :>> Wait
  :>> Wait
  :>> Stop
  :>> Open
  :>> Close
  :>> Move SDown
  :>> Wait
  :>> Wait
  :>> Stop
  :>> Open



