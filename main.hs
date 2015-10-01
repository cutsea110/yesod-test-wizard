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

data Wizard = Wizard

mkYesod "Wizard" [parseRoutes|
/    EntryR    GET POST
/c   ConfirmR  POST
|]

instance Yesod Wizard

instance RenderMessage Wizard FormMessage where
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
  ((_, w), e) <- runFormGet $ personForm Nothing
  defaultLayout $ do
    setTitle "Person"
    [whamlet|
     <form method=post action=@{EntryR} enctype=#{e}>
       ^{w}
       <input type=submit>
     |]

postEntryR :: Handler Html
postEntryR = do
  ((r, _), _) <- runFormPost $ personForm Nothing
  case r of
    FormSuccess p -> do
      ((_, wp), ep) <- runFormPost $ hiddenForm p
      ((_, wa), ea) <- runFormPost $ addressForm Nothing
      let e = ep <> ea
      defaultLayout $ do
        setTitle "Address"
        [whamlet|
          <form method=post action=@{ConfirmR} enctype=#{e}>
            ^{wp}
            ^{wa}
            <input type=submit>
         |]
    FormFailure (x:_) -> invalidArgs [x]
    FormMissing -> invalidArgs ["error"]

postConfirmR :: Handler Html
postConfirmR = do
  ((rp, _), _) <- runFormPost $ personForm Nothing
  ((ra, _), _) <- runFormPost $ addressForm Nothing
  defaultLayout $ do
    case (rp, ra) of
      (FormSuccess p, FormSuccess a) -> [whamlet|both success|]
      (FormFailure _, FormFailure _) -> [whamlet|both failure|]
      (FormMissing, FormMissing) -> [whamlet|both formmissing|]
      _ -> [whamlet|other case|]

main :: IO ()
main = warp 3000 Wizard
