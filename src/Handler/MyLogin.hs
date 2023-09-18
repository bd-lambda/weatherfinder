{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.MyLogin where

import Import
import Utils.Types
import Utils.Helpers
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getMyLoginR :: Handler Html
getMyLoginR = do
    (formWidget, formEnctype) <- generateFormPost loginForm
    let formErrors = Nothing
    defaultLayout $ do
        setTitle "Login Now"
        $(widgetFile "loginpage")


postMyLoginR :: Handler Html
postMyLoginR = do
    ((result, formWidget), formEnctype) <- runFormPost loginForm
    validationResult <- validateLoginParams result

    case validationResult of
        LoginSuccessful -> redirect ProfileR
        BadCredentials -> goBackWithErrors formWidget formEnctype

    where
        formErrors = Just True
        
        goBackWithErrors :: Widget -> Enctype -> Handler Html
        goBackWithErrors formWidget formEnctype = defaultLayout $ do
            setTitle "Login Now"
            $(widgetFile "loginpage")


loginForm :: Form LoginForm
loginForm = renderBootstrap3 BootstrapBasicForm $ LoginForm
    <$> areq textField usernameSettings Nothing
    <*> areq passwordField passwordSettings Nothing
    where 
        usernameSettings = FieldSettings
            { fsLabel = "Username"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = 
                [ ("class", "form-control")
                , ("placeholder", "Enter Username")
                , ("required", "required")
                ]
            }
        passwordSettings = FieldSettings
            { fsLabel = "Password"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = 
                [ ("class", "form-control")
                , ("placeholder", "Enter password")
                , ("required", "required")
                ]
            }