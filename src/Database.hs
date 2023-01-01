{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database (Verb, Noun, insertVerb, insertNoun,
                 Verb(Verb), Noun(Noun), initDb) where

import Database.Esqueleto
import qualified Database.Persist as P
import Database.Persist.TH
import Data.Text (Text)
import Data.Word (Word8)
import Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Noun sql=nouns
  nominativeSingular Text
  genitiveSingular   Text
  gender             Word8
Verb sql=verbs
  firstPresentActiveIndicative Text
  activeInfinitive             Text
  firstPerfectActiveIndicative Text
  supine                       Text Maybe
  deponent                     Bool
|]

initDb :: IO ()
initDb = runSqlite "/home/cmedlin/.lexicon/lexicon.db"
         $ runMigration migrateTables

insertNoun :: Noun -> IO ()
insertNoun noun = do
  x <- runSqlite "/home/cmedlin/.lexicon/lexicon.db" (insert noun)
  return ()

insertVerb :: Verb -> IO ()
insertVerb verb = do
  x <- runSqlite "/home/cmedlin/.lexicon/lexicon.db" (insert verb)
  return ()
