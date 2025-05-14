module FRP.StreamDeck.InputClock where

import System.Hardware.StreamDeck qualified as StreamDeck
import Prelude

data InputClock = InputClock

instance
    ( MonadIO m
    , IsStreamDeck s
    )
    => Clock (StreamDeckT m s) InputClock
    where
    type Time InputClock = UTCTime
    type Tag InputClock = ByteString
    initClock
        :: InputClock
        -> RunningClockInit
            (StreamDeckT m s)
            (Time InputClock)
            (Tag InputClock)
    initClock InputClock = do
        initialTime <- getCurrentTime
        let clock = constM @(StreamDeckT m s) do
                input <- StreamDeck.readInput
                time <- getCurrentTime
                pure (time, input)
        pure (clock, initialTime)

instance GetClockProxy InputClock
