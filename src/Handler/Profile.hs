{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import Utils.Types
import Utils.Helpers
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    (formWidget, formEnctype) <- generateFormPost lookupWeatherForm
    let errorMessage = Nothing :: Maybe Text

    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")


lookupWeatherForm :: Form WeatherForm
lookupWeatherForm = renderBootstrap3 BootstrapBasicForm $ WeatherForm
    <$> areq textField citySettings Nothing
    where 
        citySettings = getFieldSettings "" "Enter City" True