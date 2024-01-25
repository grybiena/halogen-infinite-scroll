module Halogen.Infinite.Scroll.Options (FeedOptions(..)) where

import Data.Time.Duration (Milliseconds)

type FeedOptions e =
  { initialElement :: e
  , pageSize :: Int
  , hiddenPages :: Int
  , preloadedPages :: Int
  , deadZone :: Int
  , debounce :: Milliseconds
  , enableTop :: Boolean
  , preloadMillis :: Number
  }

