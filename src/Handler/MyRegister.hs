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

    let errorMessage = Nothing :: Maybe Text

    defaultLayout $ do
        setTitle "Register Now"
        $(widgetFile "registerpage")

postMyRegisterR :: Handler Html
postMyRegisterR = do
    ((result, formWidget), formEnctype) <- runFormPost registerForm
    newUserCreated <- registerNewUser result
    
    case newUserCreated of
        UserCreatedSuccessfully -> redirect ProfileR
        _ -> goBackWithErrors formWidget formEnctype $ getRegisterErrorMessage newUserCreated
    where
        goBackWithErrors :: Widget -> Enctype -> Maybe Text -> Handler Html
        goBackWithErrors formWidget formEnctype errorMessage = defaultLayout $ do
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

getRegisterErrorMessage :: CreateUserResult -> Maybe Text
getRegisterErrorMessage result = case result of
    BadFormData -> Just "Invalid form data"
    PasswordsDoNotMatch -> Just "Passwords do not match"
    UsernameAlreadyInUse -> Just "This username is already taken"
    _ -> Nothing