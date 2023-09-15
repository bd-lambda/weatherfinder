{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Types where

import Import

data LoginForm = LoginForm
    { username :: Text
    , password :: Text } deriving Show