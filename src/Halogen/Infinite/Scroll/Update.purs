module Halogen.Infinite.Scroll.Update (Update,UpdateF(..),stateUpdate,computeUpdate) where

import Prelude hiding (top, bottom)
import Halogen.Infinite.Scroll.State (FeedState)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.State (State, modify_)
import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Halogen.Infinite.Scroll.Page (Page)


data UpdateF e a =
    TopShift { key :: Int, value :: Page e } a 
  | TopUnshift { key :: Int, value :: Page e } a
  | TopLoad { key :: Int, value :: Page e } a
  | TopDelete { key :: Int, value :: Page e } a 
  | BottomLoad { key :: Int, value :: Page e } a
  | BottomDelete { key :: Int, value :: Page e } a

instance Functor (UpdateF e) where
  map f (TopShift page a) = TopShift page (f a)
  map f (TopUnshift page a) = TopUnshift page (f a)
  map f (TopLoad page a) = TopLoad page (f a)
  map f (TopDelete page a) = TopDelete page (f a)
  map f (BottomLoad page a) = BottomLoad page (f a)
  map f (BottomDelete page a) = BottomDelete page (f a)

type Update e = Free (UpdateF e)

topShift :: forall e. { key :: Int, value :: Page e } -> Update e Unit
topShift page = liftF $ TopShift page unit

topUnshift :: forall e. { key :: Int, value :: Page e } -> Update e Unit
topUnshift page = liftF $ TopUnshift page unit

topLoad :: forall e. { key :: Int, value :: Page e } -> Update e Unit
topLoad page = liftF $ TopLoad page unit

topDelete :: forall e. { key :: Int, value :: Page e } -> Update e Unit
topDelete page = liftF $ TopDelete page unit
 
bottomLoad :: forall e. { key :: Int, value :: Page e } -> Update e Unit
bottomLoad page = liftF $ BottomLoad page unit

bottomDelete :: forall e. { key :: Int, value :: Page e } -> Update e Unit
bottomDelete page = liftF $ BottomDelete page unit


stateUpdate :: forall e. Update e Unit -> State (FeedState e) Unit
stateUpdate f = do
  modify_ (\st -> st { update = st.update { scroll = Nothing } })
  runFreeM go f
  where
    go (TopShift page a) = do 
      modify_ (\st -> st { 
          pages = Map.insert page.key page.value st.pages 
        , preloaded = Map.delete page.key st.preloaded
        , update = st.update { scroll = Just page.value.pageHeight }
        } )
      pure a
    go (TopUnshift page a) = do
      modify_ (\st -> st { 
          pages = Map.delete page.key st.pages
        , preloaded = Map.insert page.key page.value st.preloaded
        , update = st.update { scroll = Just (-page.value.pageHeight) }
        })
      pure a
    go (TopLoad page a) = do
      modify_ (\st -> st {
          update = if ((\u -> u.key) <$> st.update.topLoad) == Just page.key
                     then st.update { topLoad = Nothing }
                     else st.update { topLoad = Just page }
        })
      pure a
    go (TopDelete page a) = do 
      modify_ (\st -> st {
          preloaded = Map.delete page.key st.preloaded
        , update = st.update { topLoad = Nothing }
        })
      pure a
    go (BottomLoad page a) = do
      modify_ (\st -> st {
          update = if ((\u -> u.key) <$> st.update.bottomLoad) == Just page.key
                     then st.update { bottomLoad = Nothing }
                     else st.update { bottomLoad = Just page }
        })
      pure a
    go (BottomDelete page a) = do 
      modify_ (\st -> st {
          pages = Map.delete page.key st.pages
        , update = st.update { bottomLoad = Nothing }
        })
      pure a 

    
computeUpdate :: forall e. FeedState e -> Update e Unit
computeUpdate { feedParams, pages, preloaded } = 
  let notVisible (Tuple _ p) = p.visibleHeight == 0.0 
      extraAbove = A.length $ A.takeWhile notVisible $ Map.toUnfoldable pages
      tooFewAbove = extraAbove < feedParams.hiddenPages
      tooManyAbove = extraAbove > 1 + feedParams.hiddenPages
      tooFewPreload = Map.size preloaded < feedParams.preloadedPages 
      tooManyPreload = Map.size preloaded > 1 + feedParams.preloadedPages 
      extraBelow = A.length $ A.takeWhile notVisible $ A.reverse $ Map.toUnfoldable pages
      tooFewBelow = extraBelow < feedParams.hiddenPages
      tooManyBelow = extraBelow > 1 + feedParams.hiddenPages
   in do
    when (feedParams.enableTop && tooFewAbove) $ traverse_ topShift (Map.findMax preloaded)
    when (feedParams.enableTop && tooManyAbove) $ traverse_ topUnshift (Map.findMin pages)
    when (feedParams.enableTop && tooFewPreload) $ traverse_ topLoad (Map.findMin preloaded)
    when (feedParams.enableTop && tooManyPreload) $ traverse_ topDelete (Map.findMin preloaded)
    when tooFewBelow $ traverse_ bottomLoad (Map.findMax pages)
    when tooManyBelow $ traverse_ bottomDelete (Map.findMax pages)


