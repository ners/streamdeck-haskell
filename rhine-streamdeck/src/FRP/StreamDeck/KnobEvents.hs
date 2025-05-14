{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module FRP.StreamDeck.KnobEvents where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified
import System.Hardware.StreamDeck qualified as StreamDeck
import Prelude

data KnobEvent
    = KnobPressed Int
    | KnobReleased Int
    | KnobRotated Int Int
    deriving stock (Show, Eq)

parseEvents
    :: forall s
     . (IsStreamDeckWithKnobs s)
    => (ByteString, IntSet)
    -> ([KnobEvent], IntSet)
parseEvents (newInput, oldState) =
    maybe ([], oldState) handleNewState (StreamDeck.parseActiveKnobs @s newInput)
  where
    handleNewState newState =
        let (fmap fst -> IntSet.fromDistinctAscList -> newPressed, rotated) = Data.List.partition ((== 0) . snd) newState
         in case uncurry KnobRotated <$> rotated of
                [] ->
                    (pressEvents KnobPressed KnobReleased oldState newPressed, newPressed)
                rotationEvents -> (rotationEvents, oldState)

events
    :: forall s m
     . (IsStreamDeckWithKnobs s, Monad m)
    => Automaton m ByteString [KnobEvent]
events = feedback mempty . arr $ parseEvents @s
