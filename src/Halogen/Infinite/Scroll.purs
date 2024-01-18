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
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (abs)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (delay, Milliseconds(..))
import Effect.Aff.AVar (new)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen (SubscriptionId)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.Infinite.Scroll.Options (FeedOptions)
import Halogen.Infinite.Scroll.Page (class PageElement, Output(..), Page, pageOrder)
import Halogen.Infinite.Scroll.Page as Page
import Halogen.Infinite.Scroll.Page.Find (findPage)
import Halogen.Infinite.Scroll.State (FeedState)
import Halogen.Infinite.Scroll.Update (computeUpdate, stateUpdate)
import Halogen.Subscription as HS
import Pipes ((>->))
import Pipes.Core (Producer)
import Pipes.Prelude as Pipes
import Type.Proxy (Proxy(..))
import Web.DOM.Element (Element, scrollTop, setScrollTop)
import Web.HTML (window)
import Web.HTML.Window (requestIdleCallback)

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
  , hiddenPages: 3
  , preloadedPages: 3 
  , enableTop: false 
  , preloadMillis: 333.3
  }


component :: forall e o m .
             Ord e
          => Feed e m
          => H.Component (Query e) (FeedOptions e) o m
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


data Action e =
    InitializeFeed
--  | AnimationFrame SubscriptionId
  | IdleCallback SubscriptionId 
  | PageOutput Page.Output
  | FeedInsertElement e


handleAction :: forall e o m .
                    Feed e m
                 => Action e -> H.HalogenM (FeedState e) (Action e) (Slots e) o m Unit
handleAction InitializeFeed = do
  H.getRef (H.RefLabel "feed") >>= traverse_ initializeFeed
  e <- H.lift feedInsert
  traverse_ (\x -> void $ H.subscribe (FeedInsertElement <$> x)) e

  { emitter, listener } <- H.liftEffect HS.create
  j <- H.subscribe emitter
  void $ H.liftEffect $ window >>= requestIdleCallback {timeout: 500} (HS.notify listener (IdleCallback j))

  pure unit

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
handleAction (PageOutput (PageIntersection { id, height })) = do
  let setIntersection Nothing = Nothing
      setIntersection (Just page) = Just (page { visibleHeight = height })
  H.modify_ (\st -> st {
      pages = Map.alter setIntersection id st.pages
    })

handleAction (IdleCallback e) = do
    H.unsubscribe e
    feed' <- H.getRef (H.RefLabel "feed")
    flip traverse_ feed' $ \feed -> do  
      st <- H.modify (\st -> execState (stateUpdate (computeUpdate st)) st) 
      H.modify_ (\s -> s { update = { topLoad: Nothing, bottomLoad: Nothing, scroll: Nothing } })
      t <- H.liftEffect $ scrollTop feed
      H.liftEffect $ traverse_ (\o -> log (show t <> " + " <> show o) *> setScrollTop (t + o) feed) st.update.scroll
      let update = (execState (stateUpdate (computeUpdate st)) st).update
      flip traverse_ update.topLoad $ \kv -> do
        a <- loadPageAbove feed kv.value
        traverse_ (\p -> H.modify_ (\s -> s { preloaded = Map.insert p.id p s.preloaded } )) a
      flip traverse_ update.bottomLoad $ \kv -> do
        a <- loadPageBelow feed kv.value
        traverse_ (\p -> H.modify_ (\s -> s { pages = Map.insert p.id p s.pages } )) a
    H.liftAff $ delay (Milliseconds 500.0)
    { emitter, listener } <- H.liftEffect HS.create
    j <- H.subscribe emitter
    void $ H.liftEffect $ window >>= requestIdleCallback {timeout: 500} (HS.notify listener (IdleCallback j))



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
   
