{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Database
import Gui
import Data.GI.Base
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  initDb
  Gtk.init Nothing
  win <- mkWindow
  #showAll win
  Gtk.main
