{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Gui (mkWindow) where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import Database
import Data.Text
import Data.String

getVerb :: [Gtk.Entry] -> IO (Verb)
getVerb entries = do
  texts <- traverse ((flip get) #text) entries
  return $ Verb (texts !! 0)
                (texts !! 1)
                (texts !! 2)
                (Just $ texts !! 3)
                False

insertVerbFromFields fields = do
  verb <- getVerb fields
  insertVerb verb
  traverse ((flip set) [#text := ""]) fields
  Gtk.widgetGrabFocus (fields !! 0)
  return ()
    
-- fields are the Gtk.Entry text fields that will be used in making
-- a database entry, which will be cleared afterwards
mkButton :: [Gtk.Entry] -> IO (Gtk.Button)
mkButton fields = do
  button <- new Gtk.Button [#label := "Add word"]
  on button #clicked $ insertVerbFromFields fields
  return button

-- returns several Entry objects such that when return is pressed
-- in one, the next is focused
linkedTextFields :: Int -> IO ([Gtk.Entry])
linkedTextFields k = do
  fst <- new Gtk.Entry []
  tfs <- go (k-1) fst
  return (fst : tfs)
  where
    go 0 prev = return []
    go n prev = do
      tf <- new Gtk.Entry []
      on prev #activate $ do
        Gtk.widgetGrabFocus tf
      tfs <- go (n-1) tf
      return (tf : tfs)

mkVerbBox :: IO (Gtk.Box)
mkVerbBox = do
  fb <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  tfs <- linkedTextFields 4
  on (tfs !! 3) #activate $ insertVerbFromFields tfs
  traverse ((flip set) [#placeholderText := "Enter Principal Part"]) tfs
  traverse (#add fb) tfs
  button <- mkButton tfs
  #add fb button
  return fb

mkSelectionBox :: Gtk.Stack -> IO (Gtk.Box)
mkSelectionBox stack = do
  b <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  combo <- Gtk.comboBoxTextNew
  mapM_ (#append combo Nothing) ["Noun", "Verb", "Adjective", "Adverb"]
  but <- new Gtk.Button [#label := "Select"]
  on but #clicked $ do
    set stack [#visibleChildName := "verb"]
  #add b combo
  #add b but
  return b

mkStack :: IO (Gtk.Stack)
mkStack = do
  stack <- new Gtk.Stack []
  vb <- mkVerbBox
  sb <- mkSelectionBox stack
  #addNamed stack sb "selection"
  #addNamed stack vb "verb"
  set stack [#visibleChildName := "verb",
             #transitionType := Gtk.StackTransitionTypeSlideDown]
  return stack

mkWindow :: IO (Gtk.Window)
mkWindow = do
  win <- new Gtk.Window [#title := "Latin Lexicon"]
  on win #destroy Gtk.mainQuit
  #resize win 400 400
  stack <- mkStack
  #add win stack
  Gtk.stackSetVisibleChildName stack "verb"
  return win
