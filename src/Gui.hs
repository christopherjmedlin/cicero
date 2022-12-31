{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Gui (mkWindow) where

import Data.GI.Base
import qualified GI.Gtk as Gtk

mkFlowBox :: IO (Gtk.FlowBox)
mkFlowBox = do
  fb <- new Gtk.FlowBox [#orientation := Gtk.OrientationVertical]
  tf1 <- new Gtk.Entry []
  tf2 <- new Gtk.Entry []
  tf3 <- new Gtk.Entry []
  tf4 <- new Gtk.Entry []
  button <- new Gtk.Button []
  #add fb tf1
  #add fb tf2
  #add fb tf3
  #add fb tf4
  #add fb button
  return fb

mkWindow :: IO (Gtk.Window)
mkWindow = do
  win <- new Gtk.Window [#title := "Latin Lexicon"]
  on win #destroy Gtk.mainQuit
  #resize win 640 480

  fb <- mkFlowBox
  #add win fb

  return win
