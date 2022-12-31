{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
import Data.GI.Base
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  Gtk.init Nothing
  win <- new Gtk.Window [#title := "Introduction"]
  on win #destroy Gtk.mainQuit
  #resize win 640 480

  fb <- new Gtk.FlowBox [#orientation := Gtk.OrientationVertical]
  tf1 <- new Gtk.Entry []
  tf2 <- new Gtk.Entry []
  #add fb tf1
  #add fb tf2
  #add win fb

  #showAll win
  Gtk.main
