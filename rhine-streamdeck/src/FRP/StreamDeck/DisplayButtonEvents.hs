{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.StreamDeck.DisplayButtonEvents where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import System.Hardware.StreamDeck qualified as StreamDeck
import Prelude

data DisplayButtonEvent
    = DisplayButtonPressed Int
    | DisplayButtonReleased Int
    deriving stock (Show, Eq)

parseEvents
    :: forall s
     . (IsStreamDeckWithDisplayButtons s)
    => (ByteString, IntSet)
    -> ([DisplayButtonEvent], IntSet)
parseEvents (newInput, oldState) =
    maybe
        ([], oldState)
        handleNewState
        (StreamDeck.parseActiveDisplayButtons @s newInput)
  where
    handleNewState (IntSet.fromDistinctAscList -> newState) =
        ( pressEvents DisplayButtonPressed DisplayButtonReleased oldState newState
        , newState
        )

events
    :: forall s m
     . (IsStreamDeckWithDisplayButtons s, Monad m)
    => Automaton m ByteString [DisplayButtonEvent]
events = feedback mempty . arr $ parseEvents @s
