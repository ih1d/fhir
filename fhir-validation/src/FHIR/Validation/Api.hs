{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module FHIR.Validation.Api (
    API,
    api,
    server,
    app,
) where

import Lucid (Html)
import Servant
import Servant.API.ContentTypes.Lucid (HTML)

import FHIR.Validation.Views (landingPage)

-- API Definition
type API = Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

server :: Server API
server = return landingPage

app :: Application
app = serve api server
