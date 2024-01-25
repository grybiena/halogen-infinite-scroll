module Halogen.Infinite.Scroll.State (FeedState(..)) where

import Prelude hiding (top, bottom)

import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.AVar (AVar)
import Halogen.Infinite.Scroll.Options (FeedOptions)
import Halogen.Infinite.Scroll.Page (Page)

type FeedState e =
  { feedParams :: FeedOptions e
  , pages :: Map Int (Page e)
  , preloaded :: Map Int (Page e)
  , lock :: Maybe (AVar Unit)
  , update :: {
        topLoad :: Maybe { key :: Int, value :: Page e }
      , bottomLoad :: Maybe { key :: Int, value :: Page e }
      , scroll :: Maybe Number
      }
  }


