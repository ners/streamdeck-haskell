module Prelude
    ( module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader
    , module Data.Bits
    , module Data.ByteString
    , module Data.Maybe
    , module Data.Ord
    , module Data.Word
    , module Debug.Trace
    , module FRP.Rhine
    , module GHC.Generics
    , module Prelude
    , module System.Hardware.StreamDeck
    , module UnliftIO
    , module UnliftIO.Concurrent
    )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bits
import Data.ByteString (ByteString)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord
import Data.Time qualified
import Data.Word (Word16, Word8)
import Debug.Trace
import FRP.Rhine hiding (forever, newChan, try)
import GHC.Generics
import System.Hardware.StreamDeck
    ( IsStreamDeck
    , IsStreamDeckWithButtons
    , IsStreamDeckWithDisplayButtons
    , IsStreamDeckWithKnobs
    , StreamDeckT
    )
import UnliftIO
import UnliftIO.Concurrent
import "base" Prelude

getCurrentTime :: (MonadIO m) => m UTCTime
getCurrentTime = liftIO Data.Time.getCurrentTime

iterateM :: (Monad m) => (a -> m a) -> a -> m b
iterateM f = go
  where
    go x = f x >>= go

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)

type Tick cl = (Time cl, Tag cl)

pressEvents :: (Int -> event) -> (Int -> event) -> IntSet -> IntSet -> [event]
pressEvents press release old new =
    let intersection = IntSet.intersection old new
        pressed = IntSet.difference new intersection
        released = IntSet.difference old intersection
     in (press <$> IntSet.toList pressed) <> (release <$> IntSet.toList released)
