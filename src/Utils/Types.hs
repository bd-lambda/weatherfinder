{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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