{-# OPTIONS_GHC -Wno-partial-fields #-}

module FRP.StreamDeck.Layer where

import Prelude

data LayerEvent e l
    = SwitchLayers {fromLayer :: l, toLayer :: l}
    | LayerEvent {onLayer :: l, event :: e}
    deriving stock (Show, Eq)

nextLayer :: LayerEvent e l -> l
nextLayer SwitchLayers{..} = toLayer
nextLayer LayerEvent{..} = onLayer

class Layer e l where
    layerEvent :: e -> l -> LayerEvent e l
    layerEvent event onLayer = LayerEvent{..}

layerEvents :: (Monad m, Layer e l, e ~ Tag cl) => ClSF m cl l (LayerEvent e l)
layerEvents = tagS &&& returnA >>> arrMCl (pure . uncurry layerEvent)

layer
    :: (Monad m, Layer e l, e ~ Tag cl)
    => l
    -> ClSF m cl () (LayerEvent e l)
layer = flip feedback $ arr snd >>> layerEvents >>> arr (toSnd nextLayer)
