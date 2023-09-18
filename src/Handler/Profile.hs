{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Handler.Profile where

import Import
import Utils.Types
import Utils.Helpers
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)


getProfileR :: Handler Html
getProfileR = do
    (formWidget, formEnctype) <- generateFormPost lookupWeatherForm
    (_, user) <- requireAuthPair
    searchHistory <- fetchHistory user

    let errorMessage = Nothing :: Maybe Text
        weatherDetails = Nothing :: Maybe WeatherJSONResult

    defaultLayout $ do
        let timeToInt :: UTCTime -> Int
            timeToInt utctime = floor $ utctDayTime utctime :: Int

        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")


postProfileR :: Handler Html
postProfileR = do
    ((result, formWidget), formEnctype) <- runFormPost lookupWeatherForm
    (_, user) <- requireAuthPair
    searchHistory <- fetchHistory user
    
    let validationResult = validateFormData result
    let errorText = "City is invalid, length must be longer than 2 characters"

    case validationResult of
        ValidCity city -> do 
            (weatherDetails, errorMessage) <- liftIO $ getWeatherDetails city
            storeSearch user city weatherDetails
            renderView errorMessage weatherDetails user searchHistory formWidget formEnctype
        InvalidCity -> renderView (Just errorText) Nothing user searchHistory formWidget formEnctype

    where 
        -- couldn't figure out how to share a function in the template 
        timeToInt :: UTCTime -> Int
        timeToInt utctime = floor $ utctDayTime utctime :: Int

        -- horrible argument list, should use data type for this
        renderView 
            :: Maybe Text 
            -> Maybe WeatherJSONResult 
            -> User
            -> [WeatherQuery]
            -> Widget 
            -> Enctype 
            -> Handler Html
        renderView errorMessage weatherDetails user searchHistory formWidget formEnctype = defaultLayout $ do
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

storeSearch :: User -> City -> Maybe WeatherJSONResult -> Handler ()
storeSearch user (City city) mresult = do 
    now <- liftIO getCurrentTime
    case mresult of
        Just result -> runDB $ insert_ $ WeatherQuery user.userIdent city (show result) now
        Nothing -> pure ()

fetchHistory :: User -> Handler [WeatherQuery]
fetchHistory user = (fmap . fmap) entityVal $ runDB $ selectList [WeatherQueryUsername ==. user.userIdent] []

