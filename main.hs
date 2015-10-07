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

personForm mv = Person
                <$> areq textField "Name" (personName <$> mv)
                <*> areq intField "Age" (personAge <$> mv)

personHiddenForm mv = Person
                      <$> areq hiddenField "" (personName <$> mv)
                      <*> areq hiddenField "" (personAge <$> mv)

hiddenForm mp ma = (,) <$> personHiddenForm mp <*> addressForm ma

addressForm mv = Address
                 <$> areq textField "Postcode" (addressPostcode <$> mv)
                 <*> aopt textField "Prefecture" (addressPrefecture <$> mv)
                 <*> aopt textField "City" (addressCity <$> mv)

getEntryR :: Handler Html
getEntryR = do
  ((_, w), e) <- runFormPost $ renderDivs $ personForm Nothing
  defaultLayout
    [whamlet|
     <form method=post action=@{EntryR} enctype=#{e}>
       ^{w}
       <input type=submit>
     |]

postEntryR :: Handler Html
postEntryR = do
  ((r, w), e) <- runFormPost $ renderDivs $ personForm Nothing
  case r of
    FormSuccess p -> do
      ((_, w), e) <- runFormPost $ renderDivs $ hiddenForm (Just p) Nothing
      defaultLayout
        [whamlet|
          <form method=post action=@{InsertR} enctype=#{e}>
            ^{w}
            <input type=submit>
         |]
    _ -> invalidArgs ["error"]

postInsertR :: Handler Html
postInsertR = do
  ((r, _), _) <- runFormPost $ renderDivs $ hiddenForm Nothing Nothing
  defaultLayout $
    case r of
      FormSuccess (p, a) ->
        [whamlet|
         <p>
           #{personName p} : #{personAge p}
         <p>
           #{addressPostcode a}
           $maybe x <- addressPrefecture a
             #{x}
           $maybe x <- addressCity a
             #{x}
         |]
      FormFailure (x:_) -> [whamlet|#{x}|]
      _ -> [whamlet|missing|]

main :: IO ()
main = warp 3000 App
