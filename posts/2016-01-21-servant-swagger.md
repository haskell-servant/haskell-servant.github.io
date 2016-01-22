---
title: Announcing servant-swagger and swagger2
author: David Johnson, Nickolay Kudasov, Julian Arni
date: 2015-05-25 12:00
---

# Swagger

`Servant` is not the first project to provide a unified way of documenting APIs.
There is `API Blueprint`, `RAML`, `Apiary`, and finally `swagger`. While these
Web API Description languages don't do much in the way of helping you build web
services in a type-safe way, they are generally very mature, and have some
amazing tooling. For example, take a look at what `swagger-ui`, a client-side
HTML, CSS, and JS bundle, does with your `swagger` API description:

...

This is a pretty nifty way of documenting your API! But it doesn't end there.
If you have a `swagger` specification of you API, you can also take advantage
of the large variety of [languages](https://github.com/swagger-api/swagger-codegen/blob/master/README.md#customizing-the-generator) for which you can generate a client
library automatically. You don't even need to build the Java code - you can
just use the "Generate Client" button in the beautiful
[swagger editor](http://editor.swagger.io/#/).

There are a wide array of other [tools](http://swagger.io/open-source-integrations/)
that support `swagger`. Obviously, having access to them would be a great boon.
The problem so far has been that writing and maintaining a `swagger`
specification, that you are sure matches your service, isn't fun.

# Swagger-servant

Thankfully David Johnson and Nickolay Kudasov have written two wonderful Haskell
libraries, [swagger2](https://hackage.haskell.org/package/swagger2) and
[servant-swagger](https://hackage.haskell.org/package/servant-swagger), that
automate nearly all of that process for `servant` APIs. They use the mechanism
that guides most of the `servant` ecosystem - interpreters for the type-level
DSL for APIs that is `servant` - to generate a swagger spec for that API.
Here's an example - the `user` part of the
[hackage API](https://hackage.haskell.org/api):

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
`swagger2` schemas to generate a `Swagger` object:

> swaggerDoc :: Swagger
> swaggerDoc = toSwagger api

(If you're keeping tabs, so far the amount of extra work we've done compared to
 what we would have to do anyway for a `servant` server or client is these two
 lines, plus two "ToSchema" strings.)


