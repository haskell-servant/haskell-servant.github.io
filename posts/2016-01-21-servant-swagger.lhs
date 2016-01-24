---
title: Announcing servant-swagger and swagger2
author: David Johnson, Nickolay Kudasov, Julian Arni
date: 2015-05-25 12:00
---

Swagger
~~~~~~~

`Servant` is not the first project to provide a unified way of documenting APIs.
There is `API Blueprint`, `RAML`, `Apiary`, and finally `swagger`. While these
Web API Description languages don't do much in the way of helping you build web
services in a type-safe way, they are generally very mature, and have some
amazing tooling. For example, take a look at what `swagger-ui`, a client-side
HTML, CSS, and JS bundle, does with your `swagger` API description
[here](http://petstore.swagger.io/?url=https://cdn.rawgit.com/jkarni/a33dd150ac998e586f87/raw/16258a2a9f1784ecde541845ea88c7661f30a588/swagger1.json#/default).

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

Swagger2 and Servant-swagger
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Thankfully David Johnson and Nickolay Kudasov have written two wonderful Haskell
libraries, [swagger2](https://hackage.haskell.org/package/swagger2) and
[servant-swagger](https://hackage.haskell.org/package/servant-swagger), that
automate nearly all of that process for `servant` APIs. They use the mechanism
that guides most of the `servant` ecosystem - interpreters for the type-level
DSL for APIs that is `servant` - to generate a swagger spec for that API.
Here's an example - the `user` part of the
[hackage API](https://hackage.haskell.org/api):

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE DeriveAnyClass #-}
> {-# LANGUAGE OverloadedStrings #-}
> import Control.Lens ((.~), (?~), (&))
> import Servant.API
> import Data.Swagger
> import qualified Data.ByteString.Lazy.Char8 as BL8
> import Data.Proxy (Proxy(Proxy))
> import Data.Aeson (encode, ToJSON(..), FromJSON(..))
> import Servant.Swagger
> import GHC.Generics (Generic)

> type UserNameAPI
>        =    Get '[JSON] User
>       :<|>  ReqBody '[JSON] User :> Put '[JSON] ()
>       :<|>  Delete '[JSON] ()
>       :<|> "enabled" :> (Get '[JSON] Bool :<|> Put '[JSON] Bool)
>
> type UserNameAdminsAPI
>        =    Get '[JSON] [Admin]
>       :<|>  "user" :> Capture "username" String :>
>               ( ReqBody '[JSON] String :> Put '[JSON] ()
>            :<|> Delete '[JSON] ()
>               )
>
> type API = "users" :> Get '[JSON] [User]
>       :<|> "user" :> ( Capture "username" String :> UserNameAPI
>                   :<|> "admins" :> UserNameAdminsAPI
>                      )
>
> data User = User { username :: String, userpwd :: String, userenabled :: Bool }
>  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON, ToSchema)
>
> data Admin = Admin { adminUser :: User, otherDeets :: String }
>  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON, ToSchema)
>
> api :: Proxy API
> api = Proxy

(Note that this is almost certainly not a faithful representation, since I've
 had to do some guesswork with respect to requests and response bodies.)

So far this is what you would usually have when working with `servant`. The
only new thing you might notice is the `ToSchema` class in the `deriving`
clauses. This will give us a generically-derived `swagger` schema (which is
quite similar to, but not entirely the same as, JSON Schema). Part of the
`swagger2` package, it can be quite useful in its own right if you want to e.g.
respond with a schema in case of bad request bodies, or OPTIONS requests.

The next step will traverse the `API`, gathering information about it and
`swagger2` schemas to generate a `Swagger` value:

> swaggerDoc1 :: Swagger
> swaggerDoc1 = toSwagger api

(If you're keeping tabs, so far the amount of extra work we've done compared to
 what we would have to do anyway for a `servant` server or client is these two
 lines, plus two "ToSchema" strings.)

Now we can generate the swagger documentation:

> genSwaggerDoc1 :: IO ()
> genSwaggerDoc1 = BL8.putStr $ encode swaggerDoc1

You can attach more information to your `Swagger` doc quite easily, using the
lenses provided by `swagger2`:

> swaggerDoc2 :: Swagger
> swaggerDoc2 = swaggerDoc1
>   & info.infoTitle .~ "Hackage Users API"
>   & info.infoDescription ?~ "A demo of servant-swagger"

> main :: IO ()
> main = BL8.putStr $ encode swaggerDoc2

Which results in [this](https://gist.github.com/jkarni/a33dd150ac998e586f87).
https://cdn.rawgit.com/jkarni/a33dd150ac998e586f87/raw/16258a2a9f1784ecde541845ea88c7661f30a588/swagger1.json

There's a lot more you can do with both `servant-swagger` and `swagger2 - write
manual `ToSchema` instances for more detailed information, conveniently add
tags or change responses of parts of your API, use convenient lenses to modify
any part of your schema. Check of the
(`servant-swagger`)[https://hackage.haskell.org/package/servant-swagger] and
(`swagger2`)[https://hackage.haskell.org/package/swagger2] docs for more.

These two new packages vastly expand the landscape of tools within easy reach
of application developers using `servant`. Time to explore that landscape!
