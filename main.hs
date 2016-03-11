{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Yesod
import Data.Text as T
import Data.Time
import Data.Monoid

data App = App

mkYesod "App" [parseRoutes|
/    EntryR  GET POST
/c   InsertR POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

data Sex = Male | Female deriving (Show, Read, Eq, Enum, Bounded)
instance PathPiece Sex where
  fromPathPiece = Just . read . T.unpack
  toPathPiece = T.pack . show

data Person = Person { personName :: Text
                     , personAge :: Int
                     , personBirth :: Maybe Day
                     , personSex :: Sex
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
                <*> aopt dayField "Birth" (personBirth <$> mv)
                <*> areq (selectField optionsEnum) "Sex" (personSex <$> mv)

personHiddenForm mv = Person
                      <$> areq hiddenField "" (personName <$> mv)
                      <*> areq hiddenField "" (personAge <$> mv)
                      <*> areq hiddenField "" (personBirth <$> mv)
                      <*> areq hiddenField "" (personSex <$> mv)

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
      (w, e) <- generateFormPost $ renderDivs $ hiddenForm (Just p) Nothing
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
           #{personName p} : #{personAge p} : #{show $ personSex p}
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
