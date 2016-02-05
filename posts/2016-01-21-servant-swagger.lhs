---
title: Announcing servant-swagger and swagger2
author: David Johnson, Nickolay Kudasov, Julian Arni
date: 2016-02-05 03:00
---

Swagger
--------

`Servant` is not the first project to provide a unified way of documenting APIs.
There is `API Blueprint`, `RAML`, `Apiary`, and finally `swagger`. While these
Web API Description languages don't do much in the way of helping you build web
services in a type-safe way, they are generally very mature, and have some
amazing tooling. For example, take a look at what `swagger-ui`, a client-side
HTML, CSS, and JS bundle, does with your `swagger` API description
[here](http://petstore.swagger.io/?url=https://gist.githubusercontent.com/fizruk/1037ddb2c81c017f4de6/raw/c4061c9655d7f0a6a51b0eebf1e16f64cc969a07/gist.swagger.json#/default).

As you can see, it's a very convenient and approachable way of exploring your
API. In addition to an easily-navigable structure, you can easily build up
requests and send them to your server, and see its responses.

But it doesn't end there.
If you have a `swagger` specification of your API, you can also take advantage
of the large variety of [languages](https://github.com/swagger-api/swagger-codegen/blob/master/README.md#customizing-the-generator) for which you can generate a client
library automatically. You don't even need to build the Java code - you can
just use the "Generate Client" button in the beautiful
[swagger editor](http://editor.swagger.io/#/).

There are a wide array of other [tools](http://swagger.io/open-source-integrations/)
that support `swagger`. Obviously, having access to them would be a great boon.
The problem so far has been that writing and maintaining a `swagger`
specification, that you are sure matches your service, isn't fun.

swagger2 and servant-swagger
------------------------------

Thankfully David Johnson and Nickolay Kudasov have written two wonderful Haskell
libraries, [swagger2](https://hackage.haskell.org/package/swagger2) and
[servant-swagger](https://hackage.haskell.org/package/servant-swagger), that
automate nearly all of that process for `servant` APIs. They use the mechanism
that guides most of the `servant` ecosystem — interpreters for the type-level
DSL for APIs that is `servant` — to generate a swagger spec for that API.

Let's see it's used by; as an example, we're going to take the Gists part of the
[GitHub API v3](https://developer.github.com/v3/gists/). For the purpose of this
post we will ignore authentication and consider only `GET` requests which do not
require one. Furthermore, we'll use simplified representation for the responses
(i.e. we are also ignoring some fields of the response objects).

First the imports and pragmas (this is a [literate haskell file](https://github.com/haskell-servant/haskell-servant.github.io/blob/fizruk/announce-servant-swagger/posts/2016-01-21-servant-swagger.lhs)):

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TypeOperators #-}
> module Gists where
>
> import Control.Lens
> import Data.Aeson
> import Data.Aeson.Types (camelTo2)
> import qualified Data.Aeson.Types as JSON
> import qualified Data.ByteString.Lazy.Char8 as BL8
> import Data.HashMap.Strict (HashMap)
> import Data.Proxy
> import Data.Swagger
> import Data.Text (Text)
> import Data.Time (UTCTime)
> import GHC.Generics (Generic)
> import Servant
> import Servant.Swagger

The API:

> type GitHubGistAPI
>     = "users" :> Capture "username" Username :> "gists" :> QueryParam "since" UTCTime :> Get '[JSON] [Gist]
>  :<|> "gists" :> GistsAPI
>
> type GistsAPI
>     = "public"  :> QueryParam "since" UTCTime :> Get '[JSON] [Gist]
>  :<|> "starred" :> QueryParam "since" UTCTime :> Get '[JSON] [Gist]
>  :<|> Capture "id" GistId :> GistAPI
>
> type GistAPI
>     = Get '[JSON] Gist
>  :<|> Capture "sha" Revision :> Get '[JSON] Gist
>
> api :: Proxy GitHubGistAPI
> api = Proxy

Data types:

> newtype Username = Username Text deriving (Generic, ToText, FromJSON)
>
> newtype GistId = GistId Text deriving (Generic, ToText, FromJSON)
>
> newtype SHA = SHA Text deriving (Generic, ToText)
>
> type Revision = SHA
>
> data Gist = Gist
>   { gistId          :: GistId
>   , gistDescription :: Text
>   , gistOwner       :: Owner
>   , gistFiles       :: HashMap FilePath GistFile
>   , gistTruncated   :: Bool
>   , gistComments    :: Integer
>   , gistCreatedAt   :: UTCTime
>   , gistUpdatedAt   :: UTCTime
>   } deriving (Generic)
>
> data OwnerType = User | Organization
>   deriving (Generic)
>
> data Owner = Owner
>   { ownerLogin      :: Username
>   , ownerType       :: OwnerType
>   , ownerSiteAdmin  :: Bool
>   } deriving (Generic)
>
> data GistFile = GistFile
>   { gistfileSize      :: Integer
>   , gistfileLanguage  :: Text
>   , gistfileRawUrl    :: Text
>   } deriving (Generic)

`FromJSON` instances:

> modifier :: String -> String
> modifier = drop 1 . dropWhile (/= '_') . camelTo2 '_'
>
> prefixOptions :: JSON.Options
> prefixOptions = JSON.defaultOptions { JSON.fieldLabelModifier = modifier }
>
> instance FromJSON OwnerType
> instance FromJSON Owner    where parseJSON = genericParseJSON prefixOptions
> instance FromJSON GistFile where parseJSON = genericParseJSON prefixOptions
> instance FromJSON Gist     where parseJSON = genericParseJSON prefixOptions

So far this is what you would usually have when working with `servant`.
Now in to generate Swagger specification we need to define schemas for our types.
This is done with `ToParamSchema` and `ToSchema` instances:

> prefixSchemaOptions :: SchemaOptions
> prefixSchemaOptions = defaultSchemaOptions { fieldLabelModifier = modifier }
>
> instance ToParamSchema SHA
> instance ToParamSchema Username
> instance ToParamSchema GistId
>
> instance ToSchema Username
> instance ToSchema GistId
> instance ToSchema OwnerType
> instance ToSchema Owner    where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions
> instance ToSchema GistFile where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions
> instance ToSchema Gist     where declareNamedSchema = genericDeclareNamedSchema prefixSchemaOptions

These will give us a generically-derived Swagger schema (which is sort of
a deterministic version of JSON Schema).

Part of the `swagger2` package, `Schema` and `ParamSchema` can be quite useful
in their own right if you want to e.g. respond with a schema in case of bad request
bodies, or `OPTIONS` requests.

The next step will traverse the `GitHubGistAPI`, gathering information about it
and `swagger2` schemas to generate a `Swagger` value:

> swaggerDoc1 :: Swagger
> swaggerDoc1 = toSwagger api

Now we can generate the swagger documentation:

> genSwaggerDoc1 :: IO ()
> genSwaggerDoc1 = BL8.putStr $ encode swaggerDoc1

You can attach more information to your `Swagger` doc quite easily, using the
lenses provided by `swagger2`:

> swaggerDoc2 :: Swagger
> swaggerDoc2 = swaggerDoc1
>   & host ?~ "api.github.com"
>   & info.title .~ "GitHub Gists API"
>   & info.version .~ "v3"

> main :: IO ()
> main = BL8.putStr $ encode swaggerDoc2

Which results in
[this](https://gist.githubusercontent.com/fizruk/1037ddb2c81c017f4de6/raw/c4061c9655d7f0a6a51b0eebf1e16f64cc969a07/gist.swagger.json).

There's a lot more you can do with both `servant-swagger` and `swagger2` — write
manual `ToSchema` instances for more detailed information, conveniently add
tags or change responses of parts of your API, use convenient lenses to modify
any part of your schema, generate automatic tests, etc.

Check out the
[`servant-swagger`](https://hackage.haskell.org/package/servant-swagger) and
[`swagger2`](https://hackage.haskell.org/package/swagger2) docs for more.

These two new packages vastly expand the landscape of tools within easy reach
of application developers using `servant`. Time to explore that landscape!
