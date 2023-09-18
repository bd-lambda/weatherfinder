{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-} -- Ask goose about this

module Utils.Helpers where

import Import
import Utils.Types
import Network.HTTP.Simple


apiKey :: ByteString
apiKey = "5d9cd22fe1934a5fbe5190325231509"

setCredentials :: (MonadHandler m, YesodAuth (HandlerSite m)) => Text -> m ()
setCredentials username = setCreds False Creds 
    { credsPlugin = "Session"
    , credsExtra=[("", "" )]
    , credsIdent=username
    } 


validateLoginParams :: FormResult LoginForm -> Handler LoginResult
validateLoginParams result = case result of
    FormMissing -> pure BadCredentials 
    FormFailure _ -> pure  BadCredentials
    FormSuccess (LoginForm {..}) -> do
        dbresult <- runDB $ selectList [UserIdent ==. username, UserPassword ==. password] []
        if null dbresult 
            then pure BadCredentials
            else do 
                setCredentials username
                pure LoginSuccessful


createNewUser :: RegisterForm -> Handler CreateUserResult
createNewUser RegisterForm{..} = runDB $ (
    do 
        now <- liftIO getCurrentTime
        insert_ (User rUsername rPassword (Just rFullname) now)
        pure UserCreatedSuccessfully
    ) `catch` (\(SomeException _) -> pure UsernameAlreadyInUse)


passwordsDoNotMatch :: RegisterForm -> Bool
passwordsDoNotMatch RegisterForm{..} = rRetryPassword /= rPassword


createNewUserAndSetCreds :: RegisterForm -> Handler CreateUserResult
createNewUserAndSetCreds formPayload@RegisterForm{ rUsername } = do
    createUserResult <- createNewUser formPayload
    case createUserResult of 
        UserCreatedSuccessfully -> setCredentials rUsername >> pure createUserResult
        _ -> pure createUserResult


registerNewUser :: FormResult RegisterForm -> Handler CreateUserResult
registerNewUser formresult = do 
    case formresult of
        FormSuccess formData -> do 
            if passwordsDoNotMatch formData 
                then pure PasswordsDoNotMatch 
                else createNewUserAndSetCreds formData
        _ -> pure BadFormData


type Placeholder = Text


getFieldSettings :: SomeMessage master -> Placeholder -> Bool -> FieldSettings master
getFieldSettings label placeholder isRequired = FieldSettings 
    { fsLabel = label
    , fsTooltip = Nothing
    , fsId = Nothing
    , fsName = Nothing
    , fsAttrs = 
        [ ("class", "form-control")
        , ("placeholder", placeholder)
        , ("required", if isRequired then "required" else "")
        ]
    }


validateFormData :: FormResult WeatherForm -> WeatherFormValidationResult
validateFormData result = case result of
    FormSuccess WeatherForm {..} -> if cityIsValid (City wCity) 
        then ValidCity $ City wCity 
        else InvalidCity
    _ -> InvalidCity


cityIsValid :: City -> Bool
cityIsValid (City city) = length city > 2

getApiRequest :: City -> IO Request
getApiRequest (City city) = do
    let
        apiUrl = "api.weatherapi.com"
        path = "/v1/current.json?key=" <> apiKey <> "&q=" <> encodeUtf8 city

    pure $ setRequestMethod "GET" 
        $ setRequestHost apiUrl
        $ setRequestPort 443
        $ setRequestPath path 
        $ setRequestSecure True defaultRequest

lookupCity :: City ->  IO LookupCityResult
lookupCity city = do
    request <- getApiRequest city

    (do 
        response <- httpJSON request :: IO (Response WeatherJSONResult)
        let status = getResponseStatusCode response

        if status == 200
            then do
                let jsonBody = getResponseBody response
                print jsonBody
                pure $ LookupSuccess jsonBody
            else pure LookupFailed 
        ) `catch` (\(SomeException _) -> pure LookupFailed)


getWeatherDetails :: City -> IO (Maybe WeatherJSONResult)
getWeatherDetails city = do
    lookupResult <- lookupCity city
    case lookupResult of
        LookupFailed -> pure Nothing
        CityNotFound -> pure Nothing
        LookupSuccess weatherDetails -> pure $ Just weatherDetails