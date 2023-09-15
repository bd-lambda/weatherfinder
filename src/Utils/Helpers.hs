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

registerNewUser :: FormResult RegisterForm -> Handler CreateUserResult
registerNewUser formresult = do 
    now <- liftIO getCurrentTime

    case formresult of
        FormMissing -> pure BadFormData
        FormFailure _ -> pure BadFormData
        FormSuccess formData -> do 
            if passwordsDoNotMatch formData 
                then pure PasswordsDoNotMatch 
                else createNewUserAndSetCreds formData

            where 
                createNewUser :: RegisterForm -> Handler CreateUserResult
                createNewUser RegisterForm{..} = runDB $ (
                    do 
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