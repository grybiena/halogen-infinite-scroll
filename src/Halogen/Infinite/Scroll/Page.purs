module Halogen.Infinite.Scroll.Page (Page,PageIntersection,Query(..),Output(..),class PageElement,class PageOrder,element,pageOrder,component) where

import Prelude hiding (top, bottom)

import CSS.Overflow (overflow, overflowAuto)
import Data.Array (null)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Traversable (sum, traverse_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Web.DOM.Element (Element)
import Web.Intersection.Observer (newIntersectionObserver)
import Web.Intersection.Observer as WIO
import Web.Intersection.Observer.Entry (IntersectionObserverEntry)
import Web.Intersection.Observer.Options (root)
import Web.Resize.Observer (newResizeObserver)
import Web.Resize.Observer as WRO
import Web.Resize.Observer.Entry (ResizeObserverEntry)

class (Ord e) <= PageOrder e where
  pageOrder :: e -> e -> Ordering

class (PageOrder e, MonadAff m) <= PageElement e m where
  element :: forall q o . H.Component q e o m

type Page e =
  { id :: Int
  , elements :: Array e
  , parent :: Element
  , pageHeight :: Number
  , visibleHeight :: Number
  } 

data Query e a =
    PageInsert e a
  | PageDelete e a

data Action =
    InitializePage
  | PageResize (Array ResizeObserverEntry)
  | PageIntersect (Array IntersectionObserverEntry)

type PageIntersection = {
      id :: Int
    , height :: Number
    } 

data Output =
    PageHeight {
      id :: Int
    , height :: Number
    }
  | PageIntersection PageIntersection
  | PageEmpty Int

type Slots e = ( pageElement :: forall q o . H.Slot q o e )

_pageElement = Proxy :: Proxy "pageElement"

component :: forall e m .
                 PageElement e m
              => H.Component (Query e) (Page e) Output m
component =
  H.mkComponent
    { initialState: identity
    , render: renderPage
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery
                                     , initialize = Just InitializePage
                                     }
    }
  where
    renderPage page =
      HH.div
        [ HP.ref (H.RefLabel "page")
        , style do
            overflow overflowAuto
        ] 
        ((\e -> HH.slot_ _pageElement e element e) <$> page.elements)

    handleQuery :: forall a . Query e a -> H.HalogenM (Page e) Action (Slots e) Output m (Maybe a)
    handleQuery (PageInsert e a) = do
       H.modify_ (\st -> st { elements = A.nub $ A.insertBy pageOrder e st.elements })
       pure $ Just a
    handleQuery (PageDelete e a) = do
      state <- H.modify (\st -> st { elements = A.delete e st.elements })
      when (null state.elements) $ H.raise (PageEmpty state.id)
      pure $ Just a

    handleAction :: Action -> H.HalogenM (Page e) Action (Slots e) Output m Unit
    handleAction InitializePage = do
       H.getRef (H.RefLabel "page") >>= traverse_ observe
      where
        observe page = observeHeight page *> observeIntersection page
        observeIntersection page = do
          parent <- H.gets (\p -> p.parent)
          { emitter, listener } <- H.liftEffect HS.create
          void $ H.subscribe emitter
          io <- H.liftEffect $ newIntersectionObserver
                                (\a _ -> HS.notify listener (PageIntersect a))
                                (root := parent)
          liftEffect $ WIO.observe io page
        observeHeight page = do
          { emitter, listener } <- H.liftEffect HS.create
          void $ H.subscribe emitter
          ro <- H.liftEffect $ newResizeObserver (\a _ -> HS.notify listener (PageResize a))
          liftEffect $ WRO.observe ro page
    handleAction (PageResize o) = do
      pageId <- H.gets (\page -> page.id)
      let h = sum ((\e -> e.contentRect.height) <$> o)
      H.raise (PageHeight { id: pageId, height: h })
    handleAction (PageIntersect o) = do
      let h = sum ((\e -> e.intersectionRect.height) <$> o)
      pageId <- H.gets (\page -> page.id)      
      H.modify_ (\st -> st { visibleHeight = h})
      H.raise (PageIntersection { id: pageId, height: h })
 
