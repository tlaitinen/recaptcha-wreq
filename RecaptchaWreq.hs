{-# LANGUAGE OverloadedStrings #-}
module RecaptchaWreq (verifyRecaptcha, RecaptchaResult(..)) where

import Network.Wreq.Session
import Network.Wreq.Types
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import qualified Control.Exception as E
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HMS

data RecaptchaResult = RecaptchaOK | RecaptchaError A.Value | RecaptchaHttpError HTTP.HttpException

verifyRecaptcha :: Session -> Text -> Text -> Maybe Text -> IO RecaptchaResult
verifyRecaptcha s secret response remoteIp = E.catch verify (return . RecaptchaHttpError)
    where
        verify = do 
            r <- post s "https://www.google.com/recaptcha/api/siteverify" $ [
                        "secret"   := secret,
                        "response" := response
                    ] ++ (maybe [] (\ip -> [ "remoteip" := ip ]) remoteIp)
            let rb = HTTP.responseBody r
                mv = A.decode rb
            return $ case mv of
                Just v@(A.Object o) -> if HMS.lookup "success" o == Just (A.Bool True) 
                    then RecaptchaOK
                    else RecaptchaError v
                Just v -> RecaptchaError v
                Nothing -> RecaptchaError $ A.String "non-JSON-response"

        
    


