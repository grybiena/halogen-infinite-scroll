module Halogen.Infinite.Scroll.Page.Find (findPage) where

import Prelude hiding (top, bottom)

import Data.Array (fold, head, last)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen.Infinite.Scroll.Page (class PageOrder, Page, pageOrder)
import Prelude as Prelude

data Pos = Below Int | Inside Int | Above Int

instance Monoid Pos where
  mempty = Below Prelude.top

instance Semigroup Pos where
  append (Inside a) _ = Inside a
  append _ (Inside a) = Inside a
  append (Below a) (Below b) = Below (min a b)
  append (Above a) (Above b) = Above (max a b)
  append (Below a) _ = Inside a
  append (Above a) _ = Inside a

findPage :: forall e . PageOrder e => e -> Map Int (Page e) -> Int
findPage e m =
  case fold $ List.toUnfoldable $ Map.values (bounds <$> m) of
    Below a -> a
    Above a -> a
    Inside a -> a
  where
    bounds { id, elements } = 
      case (head elements /\ last elements) of
        (Just h /\ Just l)->
          case (e `pageOrder` h /\ e `pageOrder` l) of
              (LT /\ _) -> Below id
              (_ /\ GT) -> Above id
              _ -> Inside id
        _ -> Above id

