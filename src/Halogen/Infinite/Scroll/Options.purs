module Halogen.Infinite.Scroll.Options (FeedOptions(..)) where

type FeedOptions e =
  { initialElement :: e
  , pageSize :: Int
  , hiddenPages :: Int
  , preloadedPages :: Int
  , enableTop :: Boolean
  , preloadMillis :: Number
  }

