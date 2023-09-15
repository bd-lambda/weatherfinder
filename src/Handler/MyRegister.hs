{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.MyRegister where

import Import
import Utils.Types
import Utils.Helpers
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)



getMyRegisterR :: Handler Html
getMyRegisterR = do
    (formWidget, formEnctype) <- generateFormPost registerForm
    let formErrors = Nothing
    defaultLayout $ do
        setTitle "Register Now"
        $(widgetFile "registerpage")

postMyRegisterR :: Handler Html
postMyRegisterR = do
    ((result, formWidget), formEnctype) <- runFormPost registerForm
    print result
    newUserCreated <- registerNewUser result
    print newUserCreated
    
    if newUserCreated then redirect ProfileR else goBackWithErrors formWidget formEnctype
    where
        formErrors = Just True
        
        goBackWithErrors :: Widget -> Enctype -> Handler Html
        goBackWithErrors formWidget formEnctype = defaultLayout $ do
            setTitle "Register Now"
            $(widgetFile "registerpage")



registerForm :: Form RegisterForm
registerForm = renderBootstrap3 BootstrapBasicForm $ RegisterForm 
    <$> areq textField fullnameSettings Nothing
    <*> areq textField usernameSettings Nothing
    <*> areq passwordField passwordSettings Nothing
    <*> areq passwordField retryPasswordSettings Nothing
    where
        usernameSettings = getFieldSettings "Username" "Enter Username" True
        passwordSettings = getFieldSettings "Password" "Enter Password" True
        retryPasswordSettings = getFieldSettings "Retry password" "Enter Retry Password" True
        fullnameSettings = getFieldSettings "Full Name" "Enter Full name" True