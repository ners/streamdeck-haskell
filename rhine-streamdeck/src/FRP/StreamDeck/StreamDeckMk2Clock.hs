{-# LANGUAGE UndecidableInstances #-}

module FRP.StreamDeck.StreamDeckMk2Clock where

import FRP.StreamDeck.DisplayButtonEvents (DisplayButtonEvent)
import FRP.StreamDeck.DisplayButtonEvents qualified as DisplayButtonEvents
import FRP.StreamDeck.InputClock
import System.Hardware.StreamDeck.StreamDeckMk2 (StreamDeckMk2)
import Prelude

newtype StreamDeckMk2Event = DisplayButtonEvent DisplayButtonEvent

data StreamDeckMk2Clock = StreamDeckMk2Clock

instance
    ( MonadIO io
    , s ~ StreamDeckMk2
    , m ~ StreamDeckT io s
    )
    => Clock m StreamDeckMk2Clock
    where
    type Time StreamDeckMk2Clock = UTCTime
    type Tag StreamDeckMk2Clock = StreamDeckMk2Event
    initClock
        :: StreamDeckMk2Clock
        -> RunningClockInit m (Time StreamDeckMk2Clock) (Tag StreamDeckMk2Clock)
    initClock StreamDeckMk2Clock = do
        (inputClock, initialTime) <- initClock InputClock
        let clock = concatS $ proc () -> do
                (time, bs) <- inputClock -< ()
                displayButtonEvents <- DisplayButtonEvents.events @s -< bs
                let events = DisplayButtonEvent <$> displayButtonEvents
                returnA -< (time,) <$> events
        pure (clock, initialTime)

instance GetClockProxy StreamDeckMk2Clock
