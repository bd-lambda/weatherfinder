{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.MyLogout where

import Import

getMyLogoutR :: Handler Html
getMyLogoutR = clearCreds False >> redirect HomeR

