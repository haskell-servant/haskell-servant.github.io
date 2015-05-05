---
title: Getting started with servant
author: Alp Mestanogullari
toc: true
---

This is an introductory tutorial to the current version of *servant*, which is **0.3**. Any comment or issue can be directed to [this website's issue tracker](http://github.com/haskell-servant/haskell-servant.github.io/issues).

# Introduction

*servant* was born out of the following needs:

- do not require users to write *any* kind of boilerplate (encoding and decoding things from URL captures or from a JSON request body, etc) ;
- get client-side functions to query the endpoints for free ;
- get API doc for your web API for free ;
- be able to abstract *any* repeating pattern in your web application into a reusable "component" ;
- overall, make the high-level spec of your webapp something you can inspect and transform, therefore making it a first class citizen and providing more flexibility and extensibility than what we were used to. This isn't something we could get with any existing library so we went ahead and researched a way to make this happen.

Just like one can define parsers by combining smaller ones using various operations, we thought there must be a way to *describe* web APIs by combining smaller pieces. Something like "the endpoint at `/users` expects a query string parameter `sortby` whose value can be one of `age` or `name` and returns a list/array of JSON objects describing users, with fields `age`, `name`, `email`, `registration_date`". Such a description is enough for us to be able to automatically interface with the webservice from Haskell or Javascript. We know where to reach the endpoint, what parameters it expects and the shape of what it returns. This is also enough to be able to generate API docs -- as long as we can provide additional details next to the automatically generated bits.

The above descriptions kind of looks like a type though, doesn't it? We know it takes a `sortby` argument (that it gets from the query string) and produces a list of users. We could have the following code to describe our endpoint:

``` haskell
data SortBy = Age | Name
  deriving Eq

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  }

endpoint :: SortBy -> IO [User]
endpoint sortby = ...
```

However, this doesn't let us say where we want to get `sortby` from, which means we'll have to wire everything manually, which is a no-go. What if we instead could create some kind of DSL to describe once and forall everything that relates to HTTP requests and responses so that we can actually describe the endpoint from above and reuse that information in any way we want? A fixed data type with all the possible constructions wouldn't work, we want extensibility. What we came up with is an adaptation of the [tagless-final](http://okmij.org/ftp/tagless-final/) approach which lets us describe web APIS *with types* and interpret these descriptions with typeclasses and type families.

# A web API as a type

Let's recall the example of endpoint description from the previous section:

> the endpoint at `/users` expects a query string parameter `sortby` whose value can be one of `age` or `name` and returns a list/array of JSON objects describing users, with fields `age`, `name`, `email`, `registration_date`"

This could informally be described as:


> `/users[?sortby={age, name}]`
> 
> Responds with JSON like:
>
> ``` javascript
> [ {"name": "Isaac Newton", "age": 372, "email": "isaac@newton.co.uk", "registration_date": "1683-03-01"}
> , {"name": "Albert Einstein", "age": 136, "email": "ae@mc2.org", "registration_date": "1905-12-01"}
> , ...
> ]
> ```

Now let's describe it with servant. As mentionned earlier, an endpoint description is a good old Haskell **type**. We use a couple of recent GHC extensions to make our type-level vocabulary concise and expressive. Among them is the ability to have type-level strings, which lets us express static path fragments (like `"users"` in our example) in URLs.

So here's the full type that describes our endpoint, broken down and explained right after.

``` haskell
type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
```

- `"users"` simply says that our endpoint will be accessible under `/users`;
- `QueryParam "sortby" SortBy`, where `SortBy` is defined by `data SortBy = Age | Name`, says that the endpoint has a query string parameter named `sortby` whose value will be extracted as a value of type `SortBy`.
- `Get '[JSON] [User]` says that the endpoint will be accessible through HTTP GET requests, returning a list of users encoded as JSON. For any reader not familiar with the notation `'[JSON]`, it's a type-level list of types that represent the content types in which the data can be accessed. You will see later how you can make use of this to make your data available under different formats, the choice being made depending on the [Accept header](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) specified in the client's request.
- the `:>` operator that separates the various "combinators" just lets you sequence static path fragments, URL captures and other combinators. The ordering only matters for static path fragments and URL captures. `"users" :> "list-all" :> Get '[JSON] [User]`, equivalent to `/users/list-all`, is obviously not the same as `"list-all" :> "users" :> Get '[JSON] [User]`, which is equivalent to `/list-all/users`. This means that sometimes `:>` is somehow equivalent to `/`, but sometimes it just lets you chain another combinator.

One might wonder: how do we describe an API with more than one endpoint? Our answer to this is simple, just a little operator that we named `:<|>`. Here's an example:

``` haskell
type UserAPI = "users" :> "list-all" :> Get '[JSON] [User]
          :<|> "list-all" :> "users" :> Get '[JSON] [User]
```

*servant* provides a fair amount of combinators out-of-the-box and lets you write your owns when you need it. Here's a quick overview of all the combinators that servant comes with.

## Combinators

### Static strings

As you've already seen, you can use type-level strings (enabled with the `DataKinds` language extension) for static path fragments. Chaining them amounts to `/`-separating them in an URL.

``` haskell
type UserAPI = "users" :> "list-all" :> "now" :> Get '[JSON] [User]
             -- describes an endpoint reachable at:
             -- /users/list-all/now
```

### `Delete`, `Get`, `Patch`, `Post` and `Put`

These 5 combinators are very similar except that they obviously each describe a different HTTP method. They all are "dummy", empty types that take a list of content types and the type of the value that will be returned in the response body except for `Delete` which should not return any response body. In fact, here are the 5 declarations for them:

``` haskell
data Delete
data Get (contentTypes :: [*]) a
data Patch (contentTypes :: [*]) a
data Post (contentTypes :: [*]) a
data Put (contentTypes :: [*]) a
```

An endpoint must always end with one of the 5 combinators above. Examples:

``` haskell
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "admins" :> Get '[JSON] [User]
```

### `Capture`

URL captures are parts of the URL that are variable and whose actual value is captured and passed to the request handlers. In may web frameworks, you'll see it written as in `/users/:userid`, with that leading `:` denoting that `userid` is just some kind of variable name or placeholder. For instance, if `userid` is supposed to range over all integers greater or equal to 1, our endpoint will
match requests made to `/users/1`, `/users/143` and so on.

The `Capture` combinator in servant takes a (type-level) string representing the "name of the variable" and a type, which indicates the type we want to decode the "captured value" to.

``` haskell
data Capture (s :: Symbol) a
-- s :: Symbol just says that 's' must be a type-level string.
```

Examples, as usual:

``` haskell
type UserAPI = "user" :> Capture "userid" Integer :> Get '[JSON] User
               -- equivalent to 'GET /user/:userid'
               -- except that we explicitly say that "userid"
               -- must be an integer

          :<|> "user" :> Capture "userid" Integer :> Delete
               -- equivalent to 'DELETE /user/:userid'
```

### `QueryParam`, `QueryParams`, `QueryFlag`, `MatrixParam`, `MatrixParams` and `MatrixFlag`

`QueryParam`, `QueryParams` and `QueryFlag` are about query string parameters, i.e those parameters that come after the question mark (`?`) in URLs, like `sortby` in `/users?sortby=age`, whose value is here set to `age`. The difference is that `QueryParams` lets you specify that the query parameter is actually a list of values, which can be specified using `?param[]=value1&param[]=value2`. This represents a list of values composed of `value1` and `value2`. `QueryFlag` lets you specify a boolean-like query parameter where a client isn't forced to specify a value. The absence or presence of the parameter's name in the query string determines whether the parameter is considered to have value `True` or `False`. `/users?active` would list only active users whereas `/users` would list them all.

Here are the corresponding data type declarations.

``` haskell
data QueryParam (sym :: Symbol) a
data QueryParams (sym :: Symbol) a
data QueryFlag (sym :: Symbol) -- no need for a type argument, it's a boolean
```

[Matrix parameters](http://www.w3.org/DesignIssues/MatrixURIs.html), on the other hand, are like query string parameters that can appear anywhere in the paths (click the link for more details). An URL with matrix parameters in it looks like `/users;sortby=age`, as opposed to `/users?sortby=age` with query string parameters. The big advantage is that they are not necessarily at the end of the URL. You could have `/users;active=true;registered_after=2005-01-01/locations` to get geolocation data about your users that are still active and who registered after *January 1st, 2005*.

Corresponding data type declarations below.

``` haskell
data MatrixParam (sym :: Symbol) a
data MatrixParams (sym :: Symbol) a
data MatrixFlag (sym :: Symbol)
```

Examples:

``` haskell
type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
               -- equivalent to 'GET /users?sortby={age, name}'

          :<|> "users" :> MatrixParam "sortby" SortBy :> Get '[JSON] [User]
               -- equivalent to 'GET /users;sortby={age, name}'
```

### `ReqBody`

Each HTTP request can carry some additional data that the server can use in its *body* and the said data can be encoded in any format -- as long as the server understands it. This can be used for example for an endpoint for creating new users: instead of passing each field of the user as a separate query string parameter or anything dirty like that, we can group all the data into a JSON object. This has the advantage of supporting nested objects.

*servant*'s `ReqBody` combinator takes a list of content types in which the data encoded in the request body can be represented and the type of that data. Here's the data type declaration for it.

``` haskell
data ReqBody (contentTypes :: [*]) a
```

Here are the now traditional examples.

``` haskell
type UserAPI = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
               -- - equivalent to 'POST /users' with a JSON object
               --   describing an User in the request body
               -- - returns an User encoded in JSON

          :<|> "users" :> Capture "userid" Integer
                       :> ReqBody '[JSON] User
                       :> Put '[JSON] User
               -- - equivalent to 'PUT /users/:userid' with a JSON
               --   object describing an User in the request body
               --- returns an User encoded in JSON
```

### Request `Header`s

Request headers are used for various purposes, from specifying the format in which we want the response (`Accept` header) to carrying auth-related data (`Authorize` header). They consist of a header name and an associated value. An example would be `Accept: application/json`.

The `Header` combinator in servant takes a type-level string for the header name and the type to which we want to decode the header's value (from some textual representation), as illustrated below.

``` haskell
data Header (sym :: Symbol) a
```

Here's an example where we declare that an endpoint makes use of the `User-Agent` header which specifies the name of the software/library used by the client to send the request.

``` haskell
type UserAPI = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]
```

### Content types

So far, whenever we have used a combinator that carries a list of content types, we've always specified `'[JSON]`. *servant* however lets you use several content types and define your owns.

4 content-types are provided out of the box by the core *servant* package: `JSON`, `PlainText`, `FormUrlEncoded` and `OctetStream`. If for some obscure reason you wanted one of your endpoints to make your user data available under those 4 formats, you would write the API type as below.

``` haskell
type UserAPI = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]
```

We obviously provide an HTML content-type, but since there's no single library that everyone uses, we decided to release 2 packages, *servant-lucid* and *servant-blaze*, to provide HTML encoding of your data.

We will further explain how these content types and your data types can play together in the section about serving an API.

### Response `Headers`

Just like an HTTP request, the response generated by a webserver can carry headers too. *servant* provides a `Headers` combinator that carries a list of `Header` and can be used by simply wrapping the "return type" of an endpoint with it.

``` haskell
data Headers (ls :: [*]) a
```

If you want to describe an endpoint that returns a "User-Count" header in each response, you could write it as below.

``` haskell
type UserAPI = "users" :> Get '[JSON] (Headers [Header "User-Count" Integer] [User])
```

### Interoperability with other WAI `Application`s: `Raw`

Finally, we also include a combinator named `Raw` that can be used for two reasons:

- You want to serve static files from a given directory. In that case you can just say:

    ``` haskell
    type UserAPI = "users" :> Get '[JSON] [User]
                   -- a /users endpoint

              :<|> Raw
                   -- requests to anything else than /users
                   -- go here, where the server will try to
                   -- find a file with the right name
                   -- at the right path
    ```

- You more generally want to plug a [WAI `Application`](http://hackage.haskell.org/package/wai) into your webservice. Static file serving is a specific example of that. The API type would look the same as above though.

# Serving an API

Enough chitchat about type-level combinators and representing an API as a type, can we have a webservice already?

If you want to follow along with the code and run the examples while you read this guide:

``` bash
git clone https://github.com/haskell-servant/servant.git
cd servant
cabal sandbox init
cabal sandbox add-source servant/ servant-client/ servant-server/ servant-jquery/ servant-docs/ servant-examples/
cd servant-examples
cabal install --only-dependencies
cabal configure && cabal build
```

This will produce a `getting-started` executable in the `dist/build/getting-started` directory that just runs the example corresponding to the number specified as a command line argument:

``` bash
$ dist/build/getting-started/getting-started
Usage:   getting-started N
        where N is the number of the example you want to run.
```

## A first example

Equipped with some basic knowledge about the way we represent API, let's now write our first webservice.

We will write a server that will serve the following API.

``` haskell
type UserAPI = "users" :> Get '[JSON] [User]
```

In other words, we want a `/users` endpoint that shows us a list of users encoded in JSON. To keep things simple, we won't even have a database and just return a fixed list of two users, our two science geniuses from before. So here's what we would like to see when making a GET request to `/users`.

``` javascript
[ {"name": "Isaac Newton", "age": 372, "email": "isaac@newton.co.uk", "registration_date": "1683-03-01"}
, {"name": "Albert Einstein", "age": 136, "email": "ae@mc2.org", "registration_date": "1905-12-01"}
]
```

Let's start with some imports and `LANGUAGE` pragmas.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Servant
```

Alright, now let's define our User data type and write some instances for it.

``` haskell
data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

-- orphan ToJSON instance for Day. necessary to derive one for User
instance ToJSON Day where
  -- display a day in YYYY-mm-dd format
  toJSON d = toJSON (showGregorian d)

instance ToJSON User
```

Nothing funny going on here. But we now can define our list of two users.

``` haskell
users :: [User]
users = 
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]
```

Let's also write our API type.

``` haskell
type UserAPI = "users" :> Get '[JSON] [User]
```

We can now take care of writing the actual webservice that will handle requests to such an API. This one will be very simple, being reduced to just a single endpoint. The type of the web application is determined by the API type, through a *type family* named `Server`. For a decent introduction to this extension, see [the haskellwiki](https://wiki.haskell.org/GHC/Type_families). You can think of them as functions that take types as input and return types. The `Server` type family will compute the right type that a bunch of request handlers should have just from the corresponding API type.

The first thing to know about the `Server` type family is that behind the scenes it will drive the routing , letting you focus only on the business logic. The second thing to know is that for each endpoint, your handlers will by default run in the `EitherT ServantErr IO` monad. This is overridable very easily, as explained near the end of this guide. Third thing, the type of the value returned in that monad must be the same as the second argument of the HTTP method combinator used for the corresponding endpoint. In our case, it means we must provide a handler of type `EitherT ServantErr IO [User]`. Well, we have a monad, let's just `return` our list:

``` haskell
server :: Server UserAPI
server = return users
```

That's it. Now we can turn `server` into an actual webserver using [wai](http://hackage.haskell.org/package/wai) and [warp](http://hackage.haskell.org/package/warp):

``` haskell
userAPI :: Proxy UserAPI
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve userAPI server
```

What is this `Proxy` business? Well, imagine that it's not there and that `serve` just takes one argument, the `Server UserAPI`. That basically means making `serve` have the following type:

``` haskell
serve :: HasServer api => Server api -> Application

-- with an explicit 'forall'
serve :: forall api. HasServer api => Server api -> Application
```

But there's a problem with this, which might not be obvious. `Server` is a type family (or function from types to types) that belongs to the `HasServer` class. This means that the instance that GHC picks is determined by the concrete type that replaces the `api` type variable in the type signature above, `UserAPI` in our case. However, `Server`, like any other type of function, can send two different inputs to the same output. This means that if we provide a value of type `Server UserAPI` to `serve`, GHC can't guess we want the `HasServer` instance that goes with `UserAPI`, because it directly sees what `Server UserAPI` resolves to and can't just "extract" `UserAPI` from there.

`Proxy` helps fixing just that. It's a dummy data type, whose definition is the following:

``` haskell
data Proxy a = Proxy
```

Its role is to have one single value that represents a type. Indeed, for each distinct type `a`, there's a single value of type `Proxy a`, namely `Proxy` (the value constructor).

This is why we have to define `userAPI`:

``` haskell
userAPI :: Proxy UserAPI
userAPI = Proxy
```

Now, since `serve` takes a `Proxy api` argument, we can precisely specify which API type servant must look at by just passing it `userAPI`.

... And we're done. Let's run our webservice on the port 8081.

``` haskell
-- add Network.Wai.Handler.Warp to the imports

main :: IO ()
main = run 8081 app
```

You can put this all into a file or just grab [servant's repo](http://github.com/haskell-servant/servant) and look at the *servant-examples* directory. The code we have just explored is in *getting-started/GS1.hs*, runnable with `dist/build/getting-started/getting-started 2`.

If you run it, you can go to `http://localhost:8081/users` in your browser or query it with curl and you see:

``` bash
$ curl http://localhost:8081/users
[{"email":"isaac@newton.co.uk","registration_date":"1683-03-01","age":372,"name":"Isaac Newton"},{"email":"ae@mc2.org","registration_date":"1905-12-01","age":136,"name":"Albert Einstein"}]
```

## More endpoints

What if we want more than one endpoint? Let's add `/albert` and `/isaac` to view the corresponding users encoded in JSON.

``` haskell
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "albert" :> Get '[JSON] User
          :<|> "isaac" :> Get '[JSON] User
```

And let's adapt our code a bit.

``` haskell
isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users :: [User]
users = [isaac, albert]
```

Now, just like we separate the various endpoints in `UserAPI` with `:<|>`, we are going to separate the handlers with `:<|>` too! They are provided in the same order as the one they appear in in the API type.

``` haskell
server :: Server UserAPI
server = return users
    :<|> return albert
    :<|> return isaac
```

And that's it! You can run this example with `dist/build/getting-started/getting-started 2` and check out the data available at `/users`, `/albert` and `/isaac`.

## From combinators to handler arguments


## The `FromText`/`ToText` classes

## Using content-types with your own data types

## Serving static files

# Deriving Haskell functions to query an API

# Deriving Javascript functions to query an API

# API docs generation

# Links, community and more
