{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Helpers where

import Import
import Utils.Types

validateLoginParams :: FormResult LoginForm -> Handler Bool
validateLoginParams result = case result of
    FormMissing -> pure False 
    FormFailure _ -> pure  False
    FormSuccess (LoginForm {..}) -> do
        dbresult <- runDB $ selectList [UserIdent ==. username, UserPassword ==. password] []
        if null dbresult 
            then pure False
            else setCreds False Creds 
                { credsPlugin = pack "Session"
                , credsExtra=[(pack "" , pack "" )]
                , credsIdent=username
                } >> pure True

