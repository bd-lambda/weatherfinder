{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE  OverloadedStrings #-}

module Utils.Types where

import Import

data LoginForm = LoginForm
    { username :: Text
    , password :: Text } deriving Show

data RegisterForm = RegisterForm
    { rFullname :: Text
    , rUsername :: Text
    , rPassword :: Text
    , rRetryPassword :: Text } deriving Show

data CreateUserResult = BadFormData | PasswordsDoNotMatch | UsernameAlreadyInUse | UserCreatedSuccessfully

data LoginResult = LoginSuccessful | BadCredentials

newtype WeatherForm = WeatherForm { wCity :: Text }

newtype City = City Text deriving Show

data LookupCityResult = CityNotFound | LookupFailed | LookupSuccess WeatherJSONResult

data WeatherFormValidationResult = InvalidCity | ValidCity City

newtype WeatherCondition = WeatherCondition Text deriving Show

data CurrentWeatherPayload = CurrentWeatherPayload
    { tempC :: Float
    , tempF :: Float
    , feelsLikeC :: Float
    , feelsLikeF :: Float
    , windMPH :: Float
    , windKPH :: Float
    } deriving (Show, Read)

data LocationPayload = LocationPayload
    { name :: Text
    , region :: Text
    , country :: Text
    } deriving (Show, Read)

data WeatherJSONResult = WeatherJSONResult 
    { location :: LocationPayload
    , current :: CurrentWeatherPayload} deriving (Show, Read)


instance FromJSON CurrentWeatherPayload where
    parseJSON (Object v) = do
        tempC <- v .: "temp_c"
        tempF <- v .: "temp_f"
        feelsLikeC <- v .: "feelslike_c"
        feelsLikeF <- v .: "feelslike_f"
        windMPH <- v .: "wind_mph"
        windKPH <- v .: "wind_kph"

        return CurrentWeatherPayload {..}
    parseJSON _ = empty
    

instance FromJSON LocationPayload where
    parseJSON (Object v) = do
        name <- v .: "name"
        country <- v .: "country"
        region <- v .: "region"

        return LocationPayload {..}
    parseJSON _ = empty


instance FromJSON WeatherJSONResult where
    parseJSON (Object v) = do
        location <- v .: "location"
        current <- v .: "current"

        return WeatherJSONResult{..}
    parseJSON _ = empty



data WeatherHistoryItem = WeatherHistoryItem 
    { whiCity :: City
    , whiCreatedAt :: UTCTime
    , whiWeatherDetails :: WeatherJSONResult
    }  