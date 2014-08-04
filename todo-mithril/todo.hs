{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Main where

import Haste
import Haste.Foreign
import System.IO.Unsafe
import Unsafe.Coerce
import Debug.Trace 

data TodoContainerElem = TodoContainerElem {
	allEl         :: [Elem]
}

newtype VirtualElement = VirtualElement JSAny deriving (Pack, Unpack)
newtype Attributes = Attributes JSAny deriving (Pack, Unpack)

data Children = ChildText    JSString |
                ChildVirt    VirtualElement |
                ChildSubtree JSString |
                ChildList    [Children]

-- copied from Haste's Foreign.hs
lst2arr :: Opaque [Unpacked] -> Unpacked
lst2arr = unsafePerformIO . ffi "lst2arr"

instance Pack Children where
	pack = pack

instance Unpack Children where
	unpack (ChildList xs)    = lst2arr $ toOpaque $ (map unpack xs)
	unpack (ChildText s)     = unpack s
	unpack (ChildSubtree s)  = unpack s
	unpack (ChildVirt s)     = unpack s

m :: JSString -> Maybe Attributes -> Maybe Children -> IO VirtualElement
m = ffi "(function(sel,attr,ch) { console.log('sel='+sel); console.log('attr='+JSON.stringify(attr)); console.log('ch='+JSON.stringify(ch)); if (attr && ch) { return Mithril(sel,attr,ch); } else if (ch) { return Mithril(sel,ch); } else { return Mithril(sel,attr); } })"

-- Naughty use of unsafePerformIO...
-- provided so we can build ChildLists
unsafeM :: JSString -> Maybe Attributes -> Maybe Children -> VirtualElement
unsafeM s a c = unsafePerformIO $ (m s a c)

-- Naughty use of unsafePerformIO...
newAttrs :: [ (JSString, JSString) ] -> Attributes
newAttrs = unsafePerformIO . ffi "(function(xs) { var i, o = {}; for (i = 0; i < xs.length; i++) { o[xs[i][0]] = xs[i][1]; }; return o; })"

render :: Elem -> VirtualElement -> IO ()
render = ffi "(function(el,v) { Mithril.render(el,v); })"

renderList :: Elem -> [VirtualElement] -> IO ()
renderList = ffi "(function(el,v) { Mithril.render(el,v); })"

renderHeader :: IO VirtualElement
renderHeader = do
	m "header#header" Nothing
	  (Just $ ChildList [ 
	  	ChildVirt $ unsafeM "h1" Nothing (Just $ ChildText "todos"),
	  	ChildVirt $ unsafeM "input#new-todo[placeholder='What needs to be done?']" Nothing Nothing ])

renderMainSection :: IO VirtualElement
renderMainSection = do
	m "section#main" Nothing
	  (Just $ ChildList [
	  	ChildVirt $ unsafeM "input#toggle-all[type=checkbox]" Nothing Nothing,
	  	ChildVirt $ unsafeM "label[for=toggle-all]" Nothing (Just $ ChildText "Mark all as complete"),
	  	ChildVirt $ unsafeM "ul#todo-list" Nothing Nothing ])

newFilterElem :: JSString -> VirtualElement
newFilterElem text = do
	unsafeM "li" Nothing (Just $ ChildList [
		ChildVirt $ unsafeM "a" Nothing (Just $ ChildText text) ])

renderFooter :: IO VirtualElement
renderFooter = do
	m "footer#footer" Nothing
	  (Just $ ChildList [
	  	ChildVirt $ unsafeM "span#todo-count" Nothing
	  	  (Just $ ChildList [
	  	  	ChildVirt $ unsafeM "strong" Nothing (Just $ ChildText "0"),
	  	  	ChildText " items left" ]),
	  	ChildVirt $ unsafeM "ul#filters" Nothing
	  	  (Just $ ChildList [ ChildVirt (newFilterElem "All"), ChildVirt (newFilterElem "Active"), ChildVirt (newFilterElem "Completed") ]),
	  	ChildVirt $ unsafeM "button#clear-completed" Nothing
	  	  (Just $ ChildText "Clear completed (0)") ])

main :: IO ()
main = withElems ["todoapp"] start

start [parentEl] = do
	header <- renderHeader
	mainSection <- renderMainSection
	footer <- renderFooter
	renderList parentEl [header, mainSection, footer]
