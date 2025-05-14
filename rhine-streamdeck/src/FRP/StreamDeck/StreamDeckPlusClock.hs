{-# LANGUAGE UndecidableInstances #-}

module FRP.StreamDeck.StreamDeckPlusClock where

import FRP.StreamDeck.DisplayButtonEvents (DisplayButtonEvent)
import FRP.StreamDeck.DisplayButtonEvents qualified as DisplayButtonEvents
import FRP.StreamDeck.InputClock
import FRP.StreamDeck.KnobEvents (KnobEvent)
import FRP.StreamDeck.KnobEvents qualified as KnobEvents
import System.Hardware.StreamDeck.StreamDeckPlus (StreamDeckPlus)
import Prelude

data StreamDeckPlusEvent
    = DisplayButtonEvent DisplayButtonEvent
    | KnobEvent KnobEvent
    | TouchScreenEvent

data StreamDeckPlusClock = StreamDeckPlusClock

instance
    ( MonadIO io
    , s ~ StreamDeckPlus
    , m ~ StreamDeckT io s
    )
    => Clock m StreamDeckPlusClock
    where
    type Time StreamDeckPlusClock = UTCTime
    type Tag StreamDeckPlusClock = StreamDeckPlusEvent
    initClock
        :: StreamDeckPlusClock
        -> RunningClockInit m (Time StreamDeckPlusClock) (Tag StreamDeckPlusClock)
    initClock StreamDeckPlusClock = do
        (inputClock, initialTime) <- initClock InputClock
        let clock = concatS $ proc () -> do
                (time, bs) <- inputClock -< ()
                displayButtonEvents <- DisplayButtonEvents.events @s -< bs
                knobEvents <- KnobEvents.events @s -< bs
                let events = (DisplayButtonEvent <$> displayButtonEvents) <> (KnobEvent <$> knobEvents)
                returnA -< (time,) <$> events
        pure (clock, initialTime)

instance GetClockProxy StreamDeckPlusClock
