module Halogen.Infinite.Scroll (class Feed, Query(..), onElement, feedAbove, feedBelow, feedInsert, feedDelete, defaultFeedOptions,component ) where

import Prelude hiding (top, bottom)

import CSS (height, marginRight, opacity, paddingRight, pct, position, relative, top, width, zIndex)
import CSS.Overflow (hidden, overflow, overflowY, scroll)
import CSS.Size (px)
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Control.Monad.Resource (Resource, runResource)
import Control.Monad.State (execState)
import Data.Array (cons, head, last, null)
import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Ord (abs)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (delay, Milliseconds(..))
import Effect.Aff.AVar (new, put, tryTake)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.Infinite.Scroll.Options (FeedOptions)
import Halogen.Infinite.Scroll.Page (class PageElement, Output(..), Page, pageOrder)
import Halogen.Infinite.Scroll.Page as Page
import Halogen.Infinite.Scroll.Page.Find (findPage)
import Halogen.Infinite.Scroll.State (FeedState)
import Halogen.Infinite.Scroll.Update (computeUpdate, stateUpdate, updateIntersection)
import Halogen.Subscription as HS
import Pipes ((>->))
import Pipes.Core (Producer)
import Pipes.Prelude as Pipes
import Type.Proxy (Proxy(..))
import Web.DOM.Element (Element, scrollTop, setScrollTop)

class (PageElement e m, MonadAff m) <= Feed e m where
  onElement :: e -> m Unit
  feedAbove :: e -> m (Producer e Resource Unit)
  feedBelow :: e -> m (Producer e Resource Unit)
  feedInsert :: m (Maybe (HS.Emitter e))
  feedDelete :: m (Maybe (HS.Emitter e))

defaultFeedOptions :: forall e . e -> FeedOptions e
defaultFeedOptions e = 
  { initialElement: e
  , pageSize: 10
  , hiddenPages: 6
  , preloadedPages: 6 
  , deadZone: 2
  , debounce: Milliseconds 333.0
  , enableTop: false 
  , preloadMillis: 333.3
  }


component :: forall e m .
             Ord e
          => Feed e m
          => H.Component (Query e) (FeedOptions e) (FeedState e) m
component =
  H.mkComponent
    { initialState: \feedParams ->
                          { feedParams
                          , pages: Map.empty 
                          , preloaded: Map.empty
                          , lock: Nothing
                          , update: { topLoad: Nothing, bottomLoad: Nothing, scroll: Nothing }
                          }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery
                                     , initialize = Just InitializeFeed
                                     }
    }

type Slots e = ( page :: H.Slot (Page.Query e) Page.Output Int )
_page = Proxy :: Proxy "page"

data Query :: forall k. k -> Type -> Type
data Query e a =
    ScrollFeedEffect ((Number -> Effect Unit) -> a)
  | ScrollFeed Number a
  | GetScrollTop (Number -> a)



handleQuery :: forall e o m a.
                    Feed e m
                 => Query e a -> H.HalogenM (FeedState e) (Action e) (Slots e) o m (Maybe a)
handleQuery (ScrollFeedEffect f) = do
  feed' <- H.getRef (H.RefLabel "feed")
  flip traverse feed' $ \feed -> do
    let g o = do
          t <- scrollTop feed
          setScrollTop (t + o) feed
    pure (f g)
handleQuery (ScrollFeed o a) = do
  feed' <- H.getRef (H.RefLabel "feed")
  flip traverse feed' $ \feed -> H.liftEffect do
    t <- scrollTop feed
    setScrollTop (t + o) feed
    pure a
handleQuery (GetScrollTop a) = do
  feed' <- H.getRef (H.RefLabel "feed")
  flip traverse feed' $ \feed -> H.liftEffect do
    t <- scrollTop feed
    pure (a t)



data Action e =
    InitializeFeed
  | PageOutput Page.Output
  | FeedInsertElement e


handleAction :: forall e m .
                    Feed e m
                 => Action e -> H.HalogenM (FeedState e) (Action e) (Slots e) (FeedState e) m Unit
handleAction InitializeFeed = do
  H.getRef (H.RefLabel "feed") >>= traverse_ initializeFeed
  e <- H.lift feedInsert
  traverse_ (\x -> void $ H.subscribe (FeedInsertElement <$> x)) e
  where
    initializeFeed feed = do
      zerothPage <- loadInitialPage feed
      H.modify_ (\st -> st {
          pages = Map.singleton 0 zerothPage
        }
      ) 
      params <- H.gets (\st -> st.feedParams)
      void $ tailRecM (fillPreload params feed) 0
      lock <- H.liftAff do
        delay (Milliseconds params.preloadMillis)
        new unit
      H.modify_ (\st -> st { lock = Just lock })
    fillPreload params feed i = do
      if i >= params.preloadedPages
        then pure (Done i)
        else do
          managePreload feed
          pure (Loop (i+1))
handleAction (FeedInsertElement e) = do
  state <- H.get
  H.lift $ onElement e
  let b = findPage e (Map.union state.preloaded state.pages) 
  H.modify_ (insertIntoPage b)
  H.tell _page b (Page.PageInsert e) 
  where
    insertIntoPage p st = st { pages = Map.alter insertInto p st.pages
                             , preloaded = Map.alter insertInto p st.preloaded
                             }
    insertInto Nothing = Nothing
    insertInto (Just page) = Just (page { elements = A.nub $ A.insertBy pageOrder e page.elements })
 
handleAction (PageOutput (PageHeight {id, height})) = do
  H.modify_ setPageHeight
  where
    setPageHeight st = st { pages = Map.alter setHeight id st.pages
                          , preloaded = Map.alter setHeight id st.preloaded
                          }
    setHeight Nothing = Nothing
    setHeight (Just page) = Just (page { pageHeight = height })
handleAction (PageOutput (PageIntersection page)) = do
  H.modify_ (\st -> execState (stateUpdate (updateIntersection page)) st)
  mask do
    feed' <- H.getRef (H.RefLabel "feed")
    flip traverse_ feed' $ \feed -> do  
      t <- H.liftEffect $ scrollTop feed
      st <- H.modify (\st -> execState (stateUpdate (computeUpdate st page)) st) 
      H.liftEffect $ traverse_ (\o -> setScrollTop (t + o) feed) st.update.scroll
      H.raise st
      H.modify_ (\s -> s { update = { topLoad: Nothing, bottomLoad: Nothing, scroll: Nothing } })
      flip traverse_ st.update.topLoad $ \kv -> do
        a <- loadPageAbove feed kv.value
        traverse_ (\p -> H.modify_ (\s -> s { preloaded = Map.insert p.id p st.preloaded } )) a
      flip traverse_ st.update.bottomLoad $ \kv -> do
        a <- loadPageBelow feed kv.value
        traverse_ (\p -> H.modify_ (\s -> s { pages = Map.insert p.id p s.pages } )) a
      H.liftAff $ delay st.feedParams.debounce


handleAction (PageOutput (PageEmpty i)) = do
  H.modify_ (\st -> st { pages = Map.delete i st.pages })

managePreload :: forall e o m .
                 Feed e m
              => Element -> H.HalogenM (FeedState e) (Action e) (Slots e) o m Unit
managePreload feed = do
  state <- H.get
  let tooFew = Map.size state.preloaded < state.feedParams.preloadedPages
      tooMany = Map.size state.preloaded > state.feedParams.preloadedPages
  when tooFew do
    let top = maybe (Map.findMin state.pages) Just (Map.findMin state.preloaded)
    a <- join <$> traverse (\kv -> loadPageAbove feed kv.value) top
    traverse_ (\p -> H.modify_ (\st -> st { preloaded = Map.insert p.id p st.preloaded } )) a
  when tooMany do
    H.modify_ (\st -> st { preloaded = maybe st.preloaded
                                            (\kv -> Map.delete kv.key st.preloaded)
                                            (Map.findMin st.preloaded)
                         })

mask :: forall e o m .
        Feed e m
     => H.HalogenM (FeedState e) (Action e) (Slots e) o m Unit 
     -> H.HalogenM (FeedState e) (Action e) (Slots e) o m Unit 
mask f = do
  lock <- H.gets (\st -> st.lock)
  traverse_ go lock
  where
    go lock = do
        free <- H.liftAff $ tryTake lock
        when (isJust free) (f *> (H.liftAff $ put unit lock))

loadInitialPage :: forall e o m .
                   Feed e m
                => Element -> H.HalogenM (FeedState e) (Action e) (Slots e) o m (Page e)
loadInitialPage feed = do
  elements <- loadInitialPageElements
  H.lift $ traverse_ onElement elements
  pure { id: 0
       , elements
       , parent: feed
       , pageHeight: 0.0
       , visibleHeight: 0.0
       }
  where
    loadInitialPageElements = do
      init <- H.gets (\st -> st.feedParams.initialElement)
      params <- H.gets (\st -> st.feedParams)
      below <- H.lift $ feedBelow init
      let source = A.fromFoldable <$> Pipes.toListM (below >-> Pipes.take (params.pageSize - 1))
      cons init <$> (H.liftAff $ runResource source)

loadPageAbove :: forall e o m .
                 Feed e m
              => Element
              -> Page e
              -> H.HalogenM (FeedState e) (Action e) (Slots e) o m (Maybe (Page e))
loadPageAbove feed page = join <$> traverse loadNextPageUp (head page.elements)
  where
    loadNextPageUp :: e -> H.HalogenM (FeedState e) (Action e) (Slots e) o m (Maybe (Page e))
    loadNextPageUp element = do
      params <- H.gets (\st -> st.feedParams)
      above <- H.lift $ feedAbove element
      let source = A.fromFoldable <$> Pipes.toListM (above >-> Pipes.take params.pageSize)
      elements <- A.reverse <$> (H.liftAff $ runResource source)
      H.lift $ traverse_ onElement elements
      if null elements
        then pure Nothing
        else pure $ Just { id: page.id - 1
                         , elements
                         , parent: feed
                         , pageHeight: 0.0
                         , visibleHeight: 0.0
                         }

loadPageBelow :: forall e o m .
                 Feed e m
              => Element
              -> Page e
              -> H.HalogenM (FeedState e) (Action e) (Slots e) o m (Maybe (Page e))
loadPageBelow feed page = join <$> traverse loadNextPageUp (last page.elements)
  where
    loadNextPageUp :: e -> H.HalogenM (FeedState e) (Action e) (Slots e) o m (Maybe (Page e))
    loadNextPageUp element = do
      params <- H.gets (\st -> st.feedParams)
      below <- H.lift $ feedBelow element
      let source = A.fromFoldable <$> Pipes.toListM (below >-> Pipes.take params.pageSize)
      elements <- H.liftAff $ runResource source
      H.lift $ traverse_ onElement elements
      if null elements
        then pure Nothing
        else pure $ Just { id: page.id + 1
                         , elements
                         , parent: feed
                         , pageHeight: 0.0
                         , visibleHeight: 0.0
                         }

render :: forall e m .
              Feed e m
           => FeedState e -> H.ComponentHTML (Action e) (Slots e) m
render feedState = do
  HH.div
    [ style do
        width (pct 100.0)
        height (pct 100.0)
        overflow hidden
    ]
    [ renderLoaded
    , renderBarrier
    , renderPreloaded
    ]
  where
    renderPageSlot (Tuple i page) = HH.slot _page i Page.component page PageOutput
    renderLoaded =
      HH.div [ HP.ref (H.RefLabel "feed")
             , style do
                 marginRight (px (-50.0))
                 paddingRight (px 50.0) 
                 height (pct 100.0)
                 overflowY scroll
             ]
             (renderPageSlot <$> Map.toUnfoldable feedState.pages) 
    renderBarrier =
      HH.div [ style do
                 marginRight (px (-50.0))
                 paddingRight (px 50.0) 
                 height (pct 100.0)
                 zIndex (-1)
                 position relative
                 top (pct (-100.0))
             ]
             [
             ]
    renderPreloaded =
      HH.div [ style do
                zIndex (-2)
                position relative
             ]
             (preload <$> Map.toUnfoldable feedState.preloaded)
    preload page@(Tuple i _) =
      HH.div [ style do
                 zIndex ((-2) - (abs i))
                 opacity 0.0
                 position relative
             ]
             [ renderPageSlot page
             ]
   
