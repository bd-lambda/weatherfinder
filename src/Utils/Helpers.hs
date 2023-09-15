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

setCredentials :: (MonadHandler m, YesodAuth (HandlerSite m)) => Text -> m ()
setCredentials username = setCreds False Creds 
    { credsPlugin = "Session"
    , credsExtra=[("", "" )]
    , credsIdent=username
    } 

validateLoginParams :: FormResult LoginForm -> Handler Bool
validateLoginParams result = case result of
    FormMissing -> pure False 
    FormFailure _ -> pure  False
    FormSuccess (LoginForm {..}) -> do
        dbresult <- runDB $ selectList [UserIdent ==. username, UserPassword ==. password] []
        if null dbresult 
            then pure False
            else do 
                setCredentials username
                pure True


registerNewUser :: FormResult RegisterForm -> Handler Bool
registerNewUser formresult = do 
    now <- liftIO getCurrentTime

    case formresult of
        FormMissing -> pure False
        FormFailure _ -> pure False
        FormSuccess formData -> do 
            if passwordsDoNotMatch formData 
                then pure False 
                else createNewUserAndSetCreds formData

            where 
                createNewUser :: RegisterForm -> HandlerFor App ()
                createNewUser RegisterForm{..} = runDB $ do 
                    (insert_ $ User rUsername rPassword (Just rFullname) now) `catch` (\(SomeException e) -> pure ())

                passwordsDoNotMatch :: RegisterForm -> Bool
                passwordsDoNotMatch RegisterForm{..} = rRetryPassword /= rPassword

                createNewUserAndSetCreds :: RegisterForm -> HandlerFor App Bool
                createNewUserAndSetCreds formPayload@RegisterForm{ rUsername } = do
                    _ <- createNewUser formPayload
                    setCredentials rUsername
                    pure True


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