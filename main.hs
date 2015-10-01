{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Yesod
import Data.Text

data Wizard = Wizard

mkYesod "Wizard" [parseRoutes|
/ EntryR GET POST
|]

instance Yesod Wizard

instance RenderMessage Wizard FormMessage where
  renderMessage _ _ = defaultFormMessage

data Person = Person { personName :: Text
                     , personAge :: Int
                     }
              deriving Show

data Address = Address { addressPostcode :: Text
                       , addressPrefecture :: Text
                       , addressCity :: Text
                       }
               deriving Show

personForm mv = renderDivs $ Person
                <$> areq textField "Name" (personName <$> mv)
                <*> areq intField "Age" (personAge <$> mv)

getEntryR :: Handler Html
getEntryR = do
  ((_, w), e) <- runFormGet $ personForm Nothing
  defaultLayout $ do
    setTitle "Person"
    [whamlet|
     <form method=post action=@{EntryR} enctype=#{e}>
       ^{w}
       <input type=submit value=Next>
     |]

postEntryR :: Handler Html
postEntryR = do
  ((r, w), e) <- runFormPost $ personForm Nothing
  defaultLayout $ do
    setTitle "Address"
    [whamlet|
     <form method=post action=@{EntryR} enctype=#{e}>
       ^{w}
       <input type=submit value=Next>
     |]


main :: IO ()
main = warp 3000 Wizard

