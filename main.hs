{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Yesod
import Data.Text
import Data.Monoid

data App = App

mkYesod "App" [parseRoutes|
/    EntryR  GET POST
/c   InsertR POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data Person = Person { personName :: Text
                     , personAge :: Int
                     }
              deriving Show

data Address = Address { addressPostcode :: Text
                       , addressPrefecture :: Maybe Text
                       , addressCity :: Maybe Text
                       }
               deriving Show

personForm mv = renderDivs $ Person
                <$> areq textField "Name" (personName <$> mv)
                <*> areq intField "Age" (personAge <$> mv)

hiddenForm v = renderDivs $ Person
               <$> pure (personName v)
               <*> pure (personAge v)

addressForm mv = renderDivs $ Address
                 <$> areq textField "Postcode" (addressPostcode <$> mv)
                 <*> aopt textField "Prefecture" (addressPrefecture <$> mv)
                 <*> aopt textField "City" (addressCity <$> mv)

getEntryR :: Handler Html
getEntryR = do
  ((_, w), e) <- runFormPost $ personForm Nothing
  defaultLayout
    [whamlet|
     <form method=post action=@{EntryR} enctype=#{e}>
       ^{w}
       <input type=submit>
     |]

postEntryR :: Handler Html
postEntryR = do
  ((r, w), e) <- runFormPost $ personForm Nothing
  case r of
    FormSuccess p -> do
      ((_, wp), ep) <- runFormPost $ identifyForm "person" $ hiddenForm p
      ((_, wa), ea) <- runFormPost $ identifyForm "address" $ addressForm Nothing
      let e = ep <> ea
      defaultLayout
        [whamlet|
          <form method=post action=@{InsertR} enctype=#{e}>
            ^{wp}
            ^{wa}
            <input type=submit>
         |]
    _ -> invalidArgs ["error"]

postInsertR :: Handler Html
postInsertR = do
  ((rp, _), _) <- runFormPost $ identifyForm "person" $ personForm Nothing
  ((ra, _), _) <- runFormPost $ identifyForm "address" $ addressForm Nothing
  defaultLayout $
    case (rp, ra) of
      (FormSuccess p, FormSuccess a)
        -> [whamlet|both success. TODO:insert into person and address|]
      (FormFailure (x:_), FormFailure (y:_))
        -> [whamlet|both failure|]
      (FormMissing, FormMissing)
        -> [whamlet|both formmissing|]
      _ -> [whamlet|other pattern.|]

main :: IO ()
main = warp 3000 App
