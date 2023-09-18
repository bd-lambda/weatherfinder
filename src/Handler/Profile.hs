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
    (formWidget, formEnctype) <- generateFormPost lookupWeatherForm
    let errorMessage = Nothing :: Maybe Text
        weatherDetails = Nothing :: Maybe WeatherJSONResult

    defaultLayout $ do
        (_, user) <- requireAuthPair
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")


postProfileR :: Handler Html
postProfileR = do
    ((result, formWidget), formEnctype) <- runFormPost lookupWeatherForm
    
    let validationResult = validateFormData result
    let errorMessage = "Could not find weather for city"

    case validationResult of
        ValidCity city -> do 
            weatherDetails <- liftIO $ getWeatherDetails city
            renderView Nothing weatherDetails formWidget formEnctype
        InvalidCity -> renderView (Just errorMessage) Nothing formWidget formEnctype

    where 
        renderView :: Maybe Text -> Maybe WeatherJSONResult -> Widget -> Enctype -> Handler Html
        renderView errorMessage weatherDetails formWidget formEnctype = defaultLayout $ do
            (_, user) <- requireAuthPair
            setTitle $ toHtml $ fromMaybe "Success" errorMessage
            $(widgetFile "profile")


toCity :: FormResult WeatherForm -> City
toCity result = case result of
    FormSuccess (WeatherForm city) -> City city
    _ -> City ""


lookupWeatherForm :: Form WeatherForm
lookupWeatherForm = renderBootstrap3 BootstrapBasicForm $ WeatherForm
    <$> areq textField citySettings Nothing
    where 
        citySettings = getFieldSettings "" "Enter City" True