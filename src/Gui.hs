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
    
-- fields are the Gtk.Entry text fields that will be used in making
-- a database entry, which will be cleared afterwards
mkButton :: [Gtk.Entry] -> IO (Gtk.Button)
mkButton fields = do
  button <- new Gtk.Button [#label := "Add word"]
  on button #clicked $ do
    verb <- getVerb fields
    insertVerb verb
    traverse ((flip set) [#text := ""]) fields
    return ()
  return button
  
mkVerbBox :: IO (Gtk.Box)
mkVerbBox = do
  fb <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  tf1 <- new Gtk.Entry [#placeholderText := "1st Principal Part"]
  tf2 <- new Gtk.Entry [#placeholderText := "2nd Principal Part"]
  tf3 <- new Gtk.Entry [#placeholderText := "3rd Principal Part"]
  tf4 <- new Gtk.Entry [#placeholderText := "4th Principal Part"]
  button <- mkButton [tf1, tf2, tf3, tf4]
  #add fb tf1
  #add fb tf2
  #add fb tf3
  #add fb tf4
  #add fb button
  return fb

mkStack :: IO (Gtk.Stack)
mkStack = do
  stack <- new Gtk.Stack []
  vb <- mkVerbBox
  #add stack vb
  set stack [#visibleChild := vb,
             #transitionType := Gtk.StackTransitionTypeSlideRight]
  return stack

mkWindow :: IO (Gtk.Window)
mkWindow = do
  win <- new Gtk.Window [#title := "Latin Lexicon"]
  on win #destroy Gtk.mainQuit
  #resize win 400 400

  stack <- mkStack
  #add win stack

  return win
