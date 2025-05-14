{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.StreamDeck.ButtonEvents where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import System.Hardware.StreamDeck qualified as StreamDeck
import Prelude

data ButtonEvent
    = ButtonPressed Int
    | ButtonReleased Int
    deriving stock (Show, Eq)

parseEvents
    :: forall s
     . (IsStreamDeckWithButtons s)
    => (ByteString, IntSet)
    -> ([ButtonEvent], IntSet)
parseEvents (newInput, oldState) =
    maybe ([], oldState) handleNewState (StreamDeck.parseActiveButtons @s newInput)
  where
    handleNewState (IntSet.fromDistinctAscList -> newState) = (pressEvents ButtonPressed ButtonReleased oldState newState, newState)

events
    :: forall s m
     . (IsStreamDeckWithButtons s, Monad m)
    => Automaton m ByteString [ButtonEvent]
events = feedback mempty . arr $ parseEvents @s
