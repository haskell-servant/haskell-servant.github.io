---
title: Getting started with servant
author: Alp Mestanogullari
toc: true
---

This is an introductory tutorial to the current version of *servant*, which is **0.3**. Any comment or issue can be directed to [this website's issue tracker](http://github.com/haskell-servant/haskell-servant.github.io/issues).

# Introduction

*servant* has the following guiding principles:

- concision

   This is a pretty wide-ranging principle. You should be able to get nice
   documentation for you web servers, and client libraries, without repeating
   yourself. You should not have to manualy serialize and deserialize your
   resources, but only delcare how to do those things *once per type*. If a
   bunch of your handlers take the same query parameters, you shouldn't have to
   repeat that logic for each handler, but instead just "apply" it to all of
   them at once. Your handlers shouldn't be where composition goes to die. And
   so on

- flexibility

   If we haven't thought of your use case, it should still be easily
   achievable. If you want to use templating library X, go ahead. Forms? Do
   them however you want, but without difficulty. We're not opinionated.

- separatation of concerns

   Your handlers and your HTTP logic should be separate. True to the philosphy
   at the core of HTTP and REST, with *servant* your handlers return normal
   Haskell datatypes - that's the resource. And then from a description of your
   API, *servant* handles the *presentation* (i.e., the Content-Types). But
   that's just one example.

- type safety

   Want to be sure your API meets a specification? Your compiler can check
   that for you. Links you can be sure exist? You got it.

To stick true to these principles, we do things a little differently than you
might expect. The core idea is *reifying the description of your API*. Once
reified, everything follows. We think we might be the first web framework to
reify API descriptions in an extensible way. We're pretty sure we're the first
to reify it as *types*.



To be able to write a webservice you only need to read the first two sections,
but the goal of this document being to get you started with servant, we also
cover the couple of ways you can extend servant for a great good.


# A web API as a type

Consider the following informal specificatoin of an API:

> The endpoint at `/users` expects a GET request with query string parameter
> `sortby` whose value can be one of `age` or `name` and returns a
> list/array of JSON objects describing users, with fields `age`, `name`,
> `email`, `registration_date`".

You *should* be able to formalize that. And then use the formalized version to
get you much of the way towards writing a web app. And all the way towards
getting some client libraries, and documentation (and in the future, who knows
- tests, HATEOAS, ...).

How would we describe it with servant? As mentioned earlier, an endpoint
description is a good old Haskell **type**:

``` haskell
type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
```

Let's break that down:

- `"users"` says that our endpoint will be accessible under `/users`;
- `QueryParam "sortby" SortBy`, where `SortBy` is defined by `data SortBy = Age
| Name`, says that the endpoint has a query string parameter named `sortby`
whose value will be extracted as a value of type `SortBy`.
- `Get '[JSON] [User]` says that the endpoint will be accessible through HTTP
GET requests, returning a list of users encoded as JSON. You will see
later how you can make use of this to make your data available under different
formats, the choice being made depending on the [Accept
header](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) specified in
the client's request.
- the `:>` operator that separates the various "combinators" just lets you
sequence static path fragments, URL captures and other combinators. The
ordering only matters for static path fragments and URL captures. `"users" :>
"list-all" :> Get '[JSON] [User]`, equivalent to `/users/list-all`, is
obviously not the same as `"list-all" :> "users" :> Get '[JSON] [User]`, which
is equivalent to `/list-all/users`. This means that sometimes `:>` is somehow
equivalent to `/`, but sometimes it just lets you chain another combinator.

We can also describe APIs with multiple endpoints, of course, using the `:<|>`
combinators. Here's an example:

``` haskell
type UserAPI = "users" :> "list-all" :> Get '[JSON] [User]
          :<|> "list-all" :> "users" :> Get '[JSON] [User]
```

*servant* provides a fair amount of combinators out-of-the-box, but you can
always write your own when you need it. Here's a quick overview of all the
combinators that servant comes with.

## Combinators

### Static strings

As you've already seen, you can use type-level strings (enabled with the
`DataKinds` language extension) for static path fragments. Chaining
them amounts to `/`-separating them in an URL.

``` haskell
type UserAPI = "users" :> "list-all" :> "now" :> Get '[JSON] [User]
             -- describes an endpoint reachable at:
             -- /users/list-all/now
```

### `Delete`, `Get`, `Patch`, `Post` and `Put`

These 5 combinators are very similar except that they obviously each describe a
different HTTP method. This is how they're declared

``` haskell
data Delete (contentTypes :: [*]) a
data Get (contentTypes :: [*]) a
data Patch (contentTypes :: [*]) a
data Post (contentTypes :: [*]) a
data Put (contentTypes :: [*]) a
```

An endpoint ends with one of the 5 combinators above (unless you write your
own). Examples:

``` haskell
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "admins" :> Get '[JSON] [User]
```

### `Capture`

URL captures are parts of the URL that are variable and whose actual value is
captured and passed to the request handlers. In may web frameworks, you'll see
it written as in `/users/:userid`, with that leading `:` denoting that `userid`
is just some kind of variable name or placeholder. For instance, if `userid` is
supposed to range over all integers greater or equal to 1, our endpoint will
match requests made to `/users/1`, `/users/143` and so on.

The `Capture` combinator in servant takes a (type-level) string representing
the "name of the variable" and a type, which indicates the type we want to
decode the "captured value" to.

``` haskell
data Capture (s :: Symbol) a
-- s :: Symbol just says that 's' must be a type-level string.
```

In some web frameworks, you use regexes for captures. We use a `FromText` class
which the captured value must be an instance of.

Examples:

``` haskell
type UserAPI = "user" :> Capture "userid" Integer :> Get '[JSON] User
               -- equivalent to 'GET /user/:userid'
               -- except that we explicitly say that "userid"
               -- must be an integer

          :<|> "user" :> Capture "userid" Integer :> Delete
               -- equivalent to 'DELETE /user/:userid'
```

### `QueryParam`, `QueryParams`, `QueryFlag`, `MatrixParam`, `MatrixParams` and `MatrixFlag`

`QueryParam`, `QueryParams` and `QueryFlag` are about query string parameters,
i.e those parameters that come after the question mark (`?`) in URLs, like
`sortby` in `/users?sortby=age`, whose value is here set to `age`. The
difference is that `QueryParams` lets you specify that the query parameter
is actually a list of values, which can be specified using
`?param[]=value1&param[]=value2`. This represents a list of values composed
of `value1` and `value2`. `QueryFlag` lets you specify a boolean-like query
parameter where a client isn't forced to specify a value. The absence or
presence of the parameter's name in the query string determines whether the
parameter is considered to have value `True` or `False`. `/users?active`
would list only active users whereas `/users` would list them all.

Here are the corresponding data type declarations.

``` haskell
data QueryParam (sym :: Symbol) a
data QueryParams (sym :: Symbol) a
data QueryFlag (sym :: Symbol)
```

[Matrix parameters](http://www.w3.org/DesignIssues/MatrixURIs.html), on the
other hand, are like query string parameters that can appear anywhere in the
paths (click the link for more details). An URL with matrix parameters in it
looks like `/users;sortby=age`, as opposed to `/users?sortby=age` with query
string parameters. The big advantage is that they are not necessarily at the
end of the URL. You could have
`/users;active=true;registered_after=2005-01-01/locations` to get geolocation
data about your users that are still active and who registered after *January
1st, 2005*.

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

Again, your handlers don't have to deserialize these things (into, for example,
a `SortBy`). *servant* takes care of it.

### `ReqBody`

Each HTTP request can carry some additional data that the server can use in its
*body* and the said data can be encoded in any format -- as long as the server
understands it. This can be used for example for an endpoint for creating new
users: instead of passing each field of the user as a separate query string
parameter or anything dirty like that, we can group all the data into a JSON
object. This has the advantage of supporting nested objects.

*servant*'s `ReqBody` combinator takes a list of content types in which the
data encoded in the request body can be represented and the type of that data.
And, as you might have guessed, you don't have to check the content-type
header, and do the deserialization yourself. We do it for you. And return `Bad
Request` or `Unsupported Content Type` as appropriate.

Here's the data type declaration for it.

``` haskell
data ReqBody (contentTypes :: [*]) a
```

Examples:

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

Request headers are used for various purposes, from caching to carrying
auth-related data. They consist of a header name and an associated value. An
example would be `Accept: application/json`.

The `Header` combinator in servant takes a type-level string for the header
name and the type to which we want to decode the header's value (from some
textual representation), as illustrated below.

``` haskell
data Header (sym :: Symbol) a
```

Here's an example where we declare that an endpoint makes use of the
`User-Agent` header which specifies the name of the software/library used by
the client to send the request.

``` haskell
type UserAPI = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]
```

### Content types

So far, whenever we have used a combinator that carries a list of content
types, we've always specified `'[JSON]`. *servant* however lets you use several
content types and define your owns.

Four content-types are provided out-of-the-box by the core *servant* package:
`JSON`, `PlainText`, `FormUrlEncoded` and `OctetStream`. If for some obscure
reason you wanted one of your endpoints to make your user data available under
those 4 formats, you would write the API type as below.

``` haskell
type UserAPI = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]
```

We obviously provide an HTML content-type, but since there's no single library
that everyone uses, we decided to release 2 packages, *servant-lucid* and
*servant-blaze*, to provide HTML encoding of your data.

We will further explain how these content types and your data types can play
together in the section about serving an API.

### Response `Headers`

Just like an HTTP request, the response generated by a webserver can carry
headers too. *servant* provides a `Headers` combinator that carries a list of
`Header` and can be used by simply wrapping the "return type" of an endpoint
with it.

``` haskell
data Headers (ls :: [*]) a
```

If you want to describe an endpoint that returns a "User-Count" header in each
response, you could write it as below.

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

- You more generally want to plug a [WAI `Application`](http://hackage.haskell.org/package/wai)
into your webservice. Static file serving is a specific example of that. The API type would look the
same as above though. (You can even combine *servant* with other web frameworks
this way!)

# Serving an API

Enough chit-chat about type-level combinators and representing an API as a
type. Can we have a webservice already?

If you want to follow along with the code and run the examples while you read this guide:

<--! We should provide a script for this -->
``` bash
git clone https://github.com/haskell-servant/servant.git
cd servant
cabal sandbox init
cabal sandbox add-source servant/ servant-client/ servant-server/ servant-jquery/ servant-docs/ servant-examples/
cd servant-examples
cabal install --only-dependencies
cabal configure && cabal build
```

This will produce a `getting-started` executable in the
`dist/build/getting-started` directory that just runs the example corresponding
to the number specified as a command line argument:

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

Here's what we would like to see when making a GET request to `/users`.

``` javascript
[ {"name": "Isaac Newton", "age": 372, "email": "isaac@newton.co.uk", "registration_date": "1683-03-01"}
, {"name": "Albert Einstein", "age": 136, "email": "ae@mc2.org", "registration_date": "1905-12-01"}
]
```

Some imports and `LANGUAGE` pragmas:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
```

Now let's define our `User` data type and write some instances for it.

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

We can now take care of writing the actual webservice that will handle requests
to such an API. This one will be very simple, being reduced to just a single
endpoint. The type of the web application is determined by the API type,
through a *type family* named `Server`. (Type families are just functions that
take types as input and return types.)  The `Server` type family will compute
the right type that a bunch of request handlers should have just from the
corresponding API type.

The first thing to know about the `Server` type family is that behind the
scenes it will drive the routing , letting you focus only on the business
logic. The second thing to know is that for each endpoint, your handlers will
by default run in the `EitherT ServantErr IO` monad. This is overridable very
easily, as explained near the end of this guide. Third thing, the type of the
value returned in that monad must be the same as the second argument of the
HTTP method combinator used for the corresponding endpoint. In our case, it
means we must provide a handler of type `EitherT ServantErr IO [User]`. Well,
we have a monad, let's just `return` our list:

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
The `userAPI` bit is, alas, boilerplate (we need it to guide type inference).
But that's about as much boilerplate as you get.

And we're done! Let's run our webservice on the port 8081.

``` haskell
main :: IO ()
main = run 8081 app
```

You can put this all into a file or just grab [servant's
repo](http://github.com/haskell-servant/servant) and look at the
*servant-examples* directory. The code we have just explored is in
*getting-started/GS1.hs*, runnable with
`dist/build/getting-started/getting-started 2`.

If you run it, you can go to `http://localhost:8081/users` in your browser or
query it with curl and you see:

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

Now, just like we separate the various endpoints in `UserAPI` with `:<|>`, we
are going to separate the handlers with `:<|>` too! They must be provided in
the same order as the one they appear in in the API type.

``` haskell
server :: Server UserAPI
server = return users
    :<|> return albert
    :<|> return isaac
```

And that's it! You can run this example with
`dist/build/getting-started/getting-started 2` and check out the data available
at `/users`, `/albert` and `/isaac`.

## From combinators to handler arguments

Fine, we can write trivial webservices easily, but none of the two above use
any "fancy" combinator from servant. Let's address this and use `QueryParam`,
`Capture` and `ReqBody` right away. You'll see how each occurence of these
combinators in an endpoint makes the corresponding handler receive an
argument of the appropriate type automatically. You don't have to worry about
manually looking up URL captures or query string parameters, or
decoding/encoding data from/to JSON. Never.

First off, again, some pragmas and imports.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
import Control.Monad.Trans.Either
import Data.Aeson
import Data.List
import GHC.Generics
import Network.Wai
import Servant
```

We are going to use the following data types and functions to implement a server for `API`.

``` haskell
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { x :: Int
  , y :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { name :: String
  , email :: String
  , age :: Int
  , interested_in :: [String]
  } deriving Generic

instance FromJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = email c
        subject' = "Hey " ++ name c ++ ", we miss you!"
        body'    = "Hi " ++ name c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (age c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (interested_in c)
                ++ " products? Give us a visit!"
```

We can implement handlers for the three endpoints:

``` haskell
server :: Server API
server = position
    :<|> hello
    :<|> marketing

  where position :: Int -> Int -> EitherT ServantErr IO Position
        position x y = return (Position x y)

        hello :: Maybe String -> EitherT ServantErr IO HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> EitherT ServantErr IO Email
        marketing clientinfo = return (emailForClient clientinfo)
```

Did you see that? The types for your handlers changed to be just what we
needed! In particular:
- a `Capture "something" a` becomes an argument of type `a` (for `position`);
- a `QueryParam "something" a` becomes an argument of type `Maybe a` (because
an endpoint can technically be accessed without specifying any query
string parameter, we decided to "force" handlers to be aware that the
parameter might not always be there);
- a `ReqBody contentTypeList a` becomes an argument of type `a`;

And that's it. You can see this example in action by running `dist/build/getting-started/getting-started 3`.

``` bash
$ curl http://localhost:8081/position/1/2
{"x":1,"y":2}
$ curl http://localhost:8081/hello
{"msg":"Hello, anonymous coward"}
$ curl http://localhost:8081/hello?name=Alp
{"msg":"Hello, Alp"}
$ curl -X POST -d '{"name":"Alp Mestanogullari", "email" : "alp@foo.com", "age": 25, "interested_in": ["haskell", "mathematics"]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/marketing
{"subject":"Hey Alp Mestanogullari, we miss you!","body":"Hi Alp Mestanogullari,\n\nSince you've recently turned 25, have you checked out our latest haskell, mathematics products? Give us a visit!","to":"alp@foo.com","from":"great@company.com"}
```

For reference, here's a list of some combinators from *servant* and for those
that get turned into arguments to the handlers, the type of the argument.

> - `Delete`, `Get`, `Patch`, `Post`, `Put`: these do not become arguments. They provide the return type of handlers, which usually is `EitherT ServantErr IO <something>`.
> - `Capture "something" a` becomes an argument of type `a`.
> - `QueryParam "something" a`, `MatrixParam "something" a`, `Header "something" a` both become an argument of type `Maybe a`, because there might be no value at all specified by the client for these.
> - `QueryFlag "something"` and `MatrixFlag "something"` get turned into arguments of type `Bool`.
> - `QueryParams "something" a` and `MatrixParams "something" a` get turned into arguments of type `[a]`.
> - `ReqBody contentTypes a` gets turned into an argument of type `a`.

## The `FromText`/`ToText` classes

Wait... How does *servant* know how to decode the `Int`s from the URL? Or how
to decode a `ClientInfo` value from the request body? This is what this and the
following two sections address.

`Capture`s and `QueryParam`s are represented by some textual value in URLs.
`Header`s are similarly represented by a pair of a header name and a
corresponding (textual) value in the request's "metadata". This is why we
decided to provide a pair of typeclasses, `FromText` and `ToText` which just
let you say that you can respectively *extract* or *encode* values of some type
*from*/*to* text. Here are the definitions:

``` haskell
class FromText a where
  fromText :: Text -> Maybe a

class ToText a where
  toText :: a -> Text
```

And as long as the type that a `Capture`/`QueryParam`/`Header`/etc will be
decoded to provides a `FromText` instance, it will Just Work. *servant*
provides a decent number of instances, but here are a some examples of defining
your own.

``` haskell
-- A typical enumeration
data Direction
  = Up
  | Down
  | Left
  | Right

instance FromText Direction where
  -- requires {-# LANGUAGE OverloadedStrings #-}
  fromText "up"    = Just Up
  fromText "down"  = Just Down
  fromText "left"  = Just Left
  fromText "right" = Just Right
  fromText       _ = Nothing

instance ToText Direction where
  toText Up    = "up"
  toText Down  = "down"
  toText Left  = "left"
  toText Right = "right"

newtype UserId = UserId Int64
  deriving (FromText, ToText)
  -- requires GeneralizedNewtypeDeriving

-- or writing the instances by hand:
instance FromText UserId where
  fromText = fmap UserId fromText

instance ToText UserId where
  toText (UserId i) = toText i
```

There's not much else to say about these classes. You will need instances for
them when using `Capture`, `QueryParam`, `QueryParams`, `MatrixParam`,
`MatrixParams` and `Header` with your types. You will need `FromText` instances
for server-side request handlers and `ToText` instances only when using
*servant-client*, described in the section about deriving haskell
functions to query an API.

## Using content-types with your data types

The same principle was operating when decoding request bodies from JSON, and
responses *into* JSON. (JSON is just the running example - you can do this with
any content-type.)

This section introduces a couple of typeclasses provided by *servant* that make
all of this work.

### The truth behind `JSON`

What exactly is `JSON`? Like the 3 other content types provided out of the box
by *servant*, it's a really dumb data type.

``` haskell
data JSON
data PlainText
data FormUrlEncoded
data OctetStream
```

Obviously, this is not all there is to `JSON`, otherwise it would be quite
pointless. Like most of the data types in *servant*, `JSON` is mostly there as
a special *symbol* that's associated with encoding (resp. decoding) to (resp.
from) the *JSON* format. The way this association is performed can be
decomposed into two steps.

The first step is to provide a proper
[`MediaType`](https://hackage.haskell.org/package/http-media-0.6.2/docs/Network-HTTP-Media.html)
representation for `JSON`, or for your own content types. If you look at the
haddocks from this link, you can see that we just have to specify
`application/json` using the appropriate functions. In our case, we can just
use `(//) :: ByteString -> ByteString -> MediaType`. The precise way to specify
the `MediaType` is to write an instance for the `Accept` class:

``` haskell
-- for reference:
class Accept ctype where
    contentType   :: Proxy ctype -> MediaType

instance Accept JSON where
    contentType _ = "application" // "json"
```

The second step is centered around the `MimeRender` and `MimeUnrender` classes.
These classes just let you specify a way to respectively encode and decode
values respectively into or from your content-type's representation.

``` haskell
class Accept ctype => MimeRender ctype a where
    mimeRender  :: Proxy ctype -> a -> ByteString
    -- alternatively readable as:
    mimeRender  :: Proxy ctype -> (a -> ByteString)
```

Given a content-type and some user type, `MimeRender` provides a function that
encodes values of type `a` to lazy `ByteString`s.

In the case of `JSON`, this is easily dealt with! For any type `a` with a
`ToJSON` instance, we can render values of that type to JSON using
`Data.Aeson.encode`.

``` haskell
instance ToJSON a => MimeRender JSON a where
  mimeRender _ = encode
```

And now the `MimeUnrender` class, which lets us extract values from lazy
`ByteString`s, alternatively failing with an error string.

``` haskell
class Accept ctype => MimeUnrender ctype a where
    mimeUnrender :: Proxy ctype -> ByteString -> Either String a
    -- alternatively:
    mimeUnrender :: Proxy ctype -> (ByteString -> Either String a)
```

We don't have much work to do there either, `Data.Aeson.eitherDecode` is
precisely what we need. However, it only allows arrays and objects as toplevel
JSON values and this has proven to get in our way more than help us so we wrote
our own little function around *aeson* and *attoparsec* that allows any type of
JSON value at the toplevel of a "JSON document". Here's the definition in case
you are curious.

``` haskell
eitherDecodeLenient :: FromJSON a => ByteString -> Either String a
eitherDecodeLenient input = do
    v :: Value <- parseOnly (Data.Aeson.Parser.value <* endOfInput) (cs input)
    parseEither parseJSON v
```

This function is exactly what we need for our `MimeUnrender` instance.

``` haskell
instance FromJSON a => MimeUnrender JSON a where
    mimeUnrender _ = eitherDecodeLenient
```

And this is all the code that lets you use `JSON` for with `ReqBody`, `Get`,
`Post` and friends. We can check our understanding by implementing support
for an `HTML` content type, so that users of your webservice can access an
HTML representation of the data they want, ready to be included in any HTML
document, e.g using [jQuery's `load` function](https://api.jquery.com/load/), simply by adding `Accept:
text/html` to their request headers.

### Case-studies: *servant-blaze* and *servant-lucid*

These days, most of the haskellers who write their HTML UIs directly from
Haskell use either [blaze-html](http://hackage.haskell.org/package/blaze-html)
or [lucid](http://hackage.haskell.org/package/lucid). The best option for
*servant* is obviously to support both (and hopefully other templating
solutions!).

``` haskell
data HTML
```

Once again, the data type is just there as a symbol for the encoding/decoding
functions, except that this time we will only worry about encoding since
*blaze-html* and *lucid* don't provide a way to extract data from HTML.

Both packages also have the same `Accept` instance for their `HTML` type.

``` haskell
instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
```

Note that this instance uses the `(/:)` operator from *http-media* which lets
us specify additional information about a content-type, like the charset here.

The rendering instances for both packages both call similar functions that take
types with an appropriate instance to an "abstract" HTML representation and
then write that to a `ByteString`.

For *lucid*:

``` haskell
instance ToHtml a => MimeRender HTML a where
    mimeRender _ = renderBS . toHtml

-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTML (Html a) where
    mimeRender _ = renderBS
```

For *blaze-html*:

``` haskell
instance ToMarkup a => MimeRender HTML a where
    mimeRender _ = renderHtml . toHtml

-- while we're at it, just like for lucid we can
-- provide an instance for rendering blaze's 'Html' type
instance MimeRender HTML Html where
    mimeRender _ = renderHtml
```

Both [servant-blaze](http://hackage.haskell.org/package/servant-blaze) and
[servant-lucid](http://hackage.haskell.org/package/servant-lucid) let you use
`HTML` in any content type list as long as you provide an instance of the
appropriate class (`ToMarkup` for *blaze-html*, `ToHtml` for *lucid*).

We can now write webservice that uses *servant-lucid* to show the `HTML`
content type in action. First off, imports and pragmas as usual.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Foldable (foldMap)
import GHC.Generics
import Lucid
import Network.Wai
import Servant
import Servant.HTML.Lucid
```

We will be serving the following API:

``` haskell
type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]
```

where `Person` is defined as follows:

``` haskell
data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  } deriving Generic -- for the JSON instance

instance ToJSON Person
```

Now, let's teach *lucid* how to render a `Person` as a row in a table, and then
a list of `Person`s as a table with a row per person.

``` haskell
-- HTML serialization of a single person
instance ToHtml Person where
  toHtml p =
    tr_ $ do
      td_ (toHtml $ firstName p)
      td_ (toHtml $ lastName p)
      td_ (toHtml . show $ age p)

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      td_ "first name"
      td_ "last name"
      td_ "age"

    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml persons

  toHtmlRaw = toHtml
```

We create some `Person` values and serve them as a list:

``` haskell
persons :: [Person]
persons =
  [ Person "Isaac"  "Newton"   372
  , Person "Albert" "Einstein" 136
  ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server :: Server PersonAPI
server = return persons

app :: Application
app = serve personAPI server
```

And we're good to go. You can run this example with `dist/build/getting-started/getting-started 4`.

``` bash
$ curl http://localhost:8081/persons
[{"lastName":"Newton","age":372,"firstName":"Isaac"},{"lastName":"Einstein","age":136,"firstName":"Albert"}]
$ curl -H 'Accept: text/html' http://localhost:8081/persons
<table><tr><td>first name</td><td>last name</td><td>age</td></tr><tr><td>Isaac</td><td>Newton</td><td>372</td></tr><tr><td>Albert</td><td>Einstein</td><td>136</td></tr></table>
# or just point your browser to http://localhost:8081/persons
```

## The `EitherT ServantErr IO` monad

At the heart of the handlers is the monad they run in, namely `EitherT
ServantErr IO`. One might wonder: why this monad? The answer is that it is the
simplest monad with the following properties:

- it lets us both return a successful result (with the `Right` branch of
`Either`) or "fail" with a descriptive error (with the `Left` branch of
`Either`);
- it lets us perform IO, which is absolutely vital since most webservices exist
as interfaces to databases that we interact with in `IO`;

Let's recall some definitions.

``` haskell
-- from the Prelude
data Either e a = Left e | Right a

-- from the 'either' package at
-- http://hackage.haskell.org/package/either-4.3.3.2/docs/Control-Monad-Trans-Either.html
newtype EitherT e m a
  = EitherT { runEitherT :: m (Either e a) }
```

In short, this means that a handler of type `EitherT ServantErr IO a` is simply
equivalent to a computation of type `IO (Either ServantErr a)`, that is, an IO
action that either returns an error or a result.

The aforementioned `either` package is worth taking a look at. Perhaps most
importantly:

``` haskell
left :: Monad m => e -> EitherT e m a
```
Allows you to return an error from your handler (whereas `return` is enough to
return a success).

Most of what you'll be doing in your handlers is running some IO and,
depending on the result, you might sometimes want to throw an error of some
kind and abort early. The next two sections cover how to do just that.

### Performing IO

Another important instance from the list above is `MonadIO m => MonadIO (EitherT e m)`. [`MonadIO`](http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-IO-Class.html) is a class from the *transformers* package defined as:

``` haskell
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
```

Obviously, the `IO` monad provides a `MonadIO` instance. Hence for any type `e`, `EitherT e IO` has a `MonadIO` instance. So if you want to run any kind of IO computation in your handlers, just use `liftIO`:

``` haskell
type IOAPI = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

server :: Server IOAPI
server = do
  filecontent <- liftIO (readFile "myfile.txt")
  return (FileContent filecontent)
```

### Failing, through `ServantErr`

If you want to explicitly fail at providing the result promised by an endpoint
using the appropriate HTTP status code (not found, unauthorized, etc) and some
error message, all you have to do is use the `left` function mentionned above
and provide it with the appropriate value of type `ServantErr`, which is
defined as:

``` haskell
data ServantErr = ServantErr
    { errHTTPCode     :: Int
    , errReasonPhrase :: String
    , errBody         :: ByteString -- lazy bytestring
    , errHeaders      :: [Header]
    }
```

Many standard values are provided out of the box by the `Servant.Server`
module.  If you want to use these values but add a body or some headers, just
use record update syntax:

``` haskell
failingHandler = left myerr

  where myerr :: ServantErr
        myerr = err503 { errBody = "Sorry dear user." }
```

Here's an example where we return a customised 404-Not-Found error message in
the response body if "myfile.txt" isn't there:

``` haskell
type IOAPI = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

server :: Server IOAPI
server = do
  exists <- liftIO (doesFileExist "myfile.txt")
  if exists
    then liftIO (readFile "myfile.txt") >>= return . FileContent
    else left custom404Err

  where custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }
```

Let's run this server (`dist/build/getting-started/getting-started 5`) and
query it, first without the file and then with the file.

``` bash
$ curl --verbose http://localhost:8081/myfile.txt
[snip]
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /myfile.txt HTTP/1.1
> User-Agent: curl/7.30.0
> Host: localhost:8081
> Accept: */*
>
< HTTP/1.1 404 Not Found
[snip]
myfile.txt just isnt there, please leave this server alone.

$ echo Hello > myfile.txt

$ curl --verbose http://localhost:8081/myfile.txt
[snip]
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /myfile.txt HTTP/1.1
> User-Agent: curl/7.30.0
> Host: localhost:8081
> Accept: */*
>
< HTTP/1.1 200 OK
[snip]
< Content-Type: application/json
[snip]
{"content":"Hello\n"}
```

## Serving static files

*servant-server* also provides a way to just serve the content of a directory
under some path in your web API. As mentionned earlier in this document, the
`Raw` combinator can be used in your APIs to mean "plug here any WAI
application". Well, servant-server provides a function to get a file and
directory serving WAI application, namely:

``` haskell
-- exported by Servant and Servant.Server
serveDirectory :: FilePath -> Server Raw
```

`serveDirectory`'s argument must be a path to a valid directory. You can see a
example below, runnable with `dist/build/getting-started/getting-started 6`
(you **must** run it from within the *servant-examples/* directory!), which is
a webserver that serves the various bits of code covered in this
getting-started.

The API type will be the following.

``` haskell
type API = "code" :> Raw
```

And the server:

``` haskell
api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "getting-started"

app :: Application
app = serve api server
```

This server will match any request whose path starts with `/code` and will look for a file at the path described by the rest of the request path, inside the *getting-started/* directory of the path you run the program from.

In other words:

- If a client requests `/code/foo.txt`, the server will look for a file at `./getting-started/foo.txt` (and fail)
- If a client requests `/code/GS1.hs`, the server will look for a file at `./getting-started/GS1.hs` (and succeed)
- If a client requests `/code/foo/bar/baz/movie.mp4`, the server will look for a file at `./getting-started/foo/bar/baz/movie.mp4` (and fail)

Here is our little server in action.

``` haskell
$ curl http://localhost:8081/code/GS1.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module GS1 where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Servant

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

type UserAPI = "users" :> Get '[JSON] [User]

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = return users

app :: Application
app = serve userAPI server
$ curl http://localhost:8081/code/getting-started.hs
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

import qualified GS1
import qualified GS2
import qualified GS3
import qualified GS4
import qualified GS5
import qualified GS6

app :: String -> Maybe Application
app n = case n of
  "1" -> Just GS1.app
  "2" -> Just GS2.app
  "3" -> Just GS3.app
  "4" -> Just GS4.app
  "5" -> Just GS5.app
  "6" -> Just GS6.app
  _   -> Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> maybe usage (run 8081) (app n)
    _   -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:\t getting-started N"
  putStrLn "\t\twhere N is the number of the example you want to run."
$ curl http://localhost:8081/foo
not found
```

## Using another monad for your handlers

Remember how `Server` turns combinators for HTTP methods into `EitherT ServantErr IO`? Well, actually, there's more to that. `Server` is actually a simple type synonym.

``` haskell
type Server api = ServerT api (EitherT ServantErr IO)
```

`ServerT` is the actual type family that computes the required types for the handlers that's part of the `HasServer` class. It's like `Server` except that it takes a third parameter which is the monad you want your handlers to run in, or more generally the return types of your handlers. This third parameter is used for specifying the return type of the handler for an endpoint, e.g when computing `ServerT (Get '[JSON] Person) SomeMonad`. The result would be `SomeMonad Person`.

The first and main question one might have then is: how do we write handlers that run in another monad? How can we "bring back" the value from a given monad into something *servant* can understand?

### Natural transformations

If we have a function that gets us from an `m a` to an `n a`, for any `a`, what
do we have?

``` haskell
newtype m :~> n = Nat { unNat :: forall a. m a -> n a}

-- For example
-- listToMaybeNat ::`[] :~> Maybe`
-- listToMaybeNat = Nat listToMaybe  -- from Data.Maybe
```
(`Nat` comes from "natural transformation", in case you're wondering.)

So if you want to write handlers using another monad/type than `EitherT
ServantErr IO`, say the `Reader String` monad, the first thing you have to
prepare is a function:

``` haskell
readerToEither :: Reader String :~> EitherT ServantErr IO
```

Let's start with `readerToEither'`. We obviously have to run the `Reader`
computation by supplying it with a `String`, like `"hi"`. We get an `a` out
from that and can then just `return` it into `EitherT`. We can then just wrap
that function with the `Nat` constructor to make it have the fancier type.

``` haskell
readerToEither' :: forall a. Reader String a -> EitherT ServantErr IO a
readerToEither' r = return (runReader r "hi")

readerToEither :: Reader String :~> EitherT ServantErr IO
readerToEither = Nat . readerToEither'
```

We can write some simple webservice with the handlers running in `Reader String`.

``` haskell
type ReaderAPI = "a" :> Get '[JSON] Int
            :<|> "b" :> Get '[JSON] String

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b

  where a :: Reader String Int
        a = return 1797

        b :: Reader String String
        b = ask
```

we unfortunately can't use `readerServerT` as an argument of `serve`, because
`serve` wants a `Server ReaderAPI`, i.e with handlers running in `EitherT
ServantErr IO`. But there's a simple solution to this.

### Enter `enter`

That's right. We have just written `readerToEither`, which is exactly what we
could need to call on the results of all handlers to make the handlers have the
right type for `serve`. Being cumbersome to do by hand, we provide a function
`enter` which takes a natural transformation between two parametrized types `m`
and `n` and a `ServerT someapi m`, and returns a `ServerT someapi n`.

In our case, we can wrap up our little webservice by using `enter readerToEither` on our handlers.

``` haskell
readerServer :: Server ReaderAPI
readerServer = enter readerToEither readerServerT

app :: Application
app = serve readerAPI readerServer
```

And we can indeed see this webservice in action by running `dist/build/getting-started/getting-started 7`.

``` bash
$ curl http://localhost:8081/a
1797
$ curl http://localhost:8081/b
"hi"
```

## Conclusion

You're now equipped to write any kind of webservice/web-application using *servant*. One thing not covered here is how to incorporate your own combinators and will be the topic of a page on the website. The rest of this document focuses on *servant-client*, *servant-jquery* and *servant-docs*.

# Deriving Haskell functions to query an API

While defining handlers that serve an API has a lot to it, querying an API however appears simpler: we do not care about what happens inside the webserver, we just know how to talk to it and get a response back. Except that we usually have to write the querying functions by hand because the structure of the API isn't a first class citizen and can't be inspected to generate a bunch of client-side functions.

*servant* however has a way to inspect API, because APIs are just Haskell types and (GHC) Haskell lets us do quite a few things with types. In the same way that we look at an API type to deduce the types the handlers should have, we can inspect the structure of the API to *derive* Haskell functions that take one argument for each occurence of `Capture`, `ReqBody`, `QueryParam`
and friends. By *derive*, we here mean that there's no code generation involved, the functions are defined just by the structure of the API type.

Enough chitchat, let's see an example. Consider the following API type from the previous section:

``` haskell
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
```

What we are going to get with *servant-client* here is 3 functions, one to query each endpoint:

```
position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> EitherT ServantError IO Position

hello :: Maybe String -- ^ an optional value for "name"
      -> EitherT ServantError IO HelloMessage

marketing :: ClientInfo -- ^ value for the request body
          -> EitherT ServantError IO Email
```

Each function makes available as an argument any value that the response may depend on, as evidenced in the API type. How do we get these functions? Just give a `Proxy` to your API and a host to make the requests to:

``` haskell
api :: Proxy API
api = Proxy

position :<|> hello :<|> marketing = client api (BaseUrl Http "localhost" 8081)
```

As you can see in the code above, we just "pattern match our way" to these functions. If we try to derive less or more functions than there are endpoints in the API, we obviously get an error. The `BaseUrl` value there is just:

``` haskell
-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort :: Int      -- ^ port (eg 80)
  }
```

That's it. Let's now write some code that uses our client functions.

``` haskell
queries :: EitherT ServantError IO (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  msg <- hello (Just "servant")
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, msg, em)

run :: IO ()
run = do
  res <- runEitherT queries
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, msg, em) -> do
      print pos
      print msg
      print em
```

You can now run `dist/build/getting-started/getting-started 8` (the server) and
`dist/build/gs8-main/gs8-main` (the client) to see them both in action.

``` bash
$ dist/build/getting-started/getting-started 8
# and in another terminal:
$ dist/build/gs8-main/gs8-main
Position {x = 10, y = 10}
HelloMessage {msg = "Hello, servant"}
Email {from = "great@company.com", to = "alp@foo.com", subject = "Hey Alp, we miss you!", body = "Hi Alp,\n\nSince you've recently turned 26, have you checked out our latest haskell, mathematics products? Give us a visit!"}
```

The types of the arguments for the functions are the same as for (server-side) request handlers. You now know how to use *servant-client*!

# Deriving Javascript functions to query an API

We will now see how *servant* lets you turn an API type into javascript functions that you can call to query a webservice. The derived code assumes you use *jQuery* but you could very easily adapt the code to generate ajax requests based on vanilla javascript or another library than *jQuery*.

For this, we will consider a simple page divided in two parts. At the top, we will have a search box that lets us search in a list of Haskell books by author/title with a list of results that gets updated every time we enter or remove a character, while at the bottom we will be able to see the classical [probabilistic method to approximate &pi;](http://en.wikipedia.org/wiki/Approximations_of_%CF%80#Summing_a_circle.27s_area), using a webservice to get random points. Finally, we will serve an HTML file along with a couple of Javascript files, among which one that's automatically generated from the API type and which will provide ready-to-use functions to query your API.

Let's start with the API type(s) and the accompanying datatypes.

``` haskell
type API = "point" :> Get '[JSON] Point
      :<|> "books" :> QueryParam "q" Text :> Get '[JSON] (Search Book)

type API' = API :<|> Raw

data Point = Point
  { x :: Double
  , y :: Double
  } deriving Generic

instance ToJSON Point

data Search a = Search
  { query   :: Text
  , results :: [a]
  } deriving Generic

mkSearch :: Text -> [a] -> Search a
mkSearch = Search

instance ToJSON a => ToJSON (Search a)

data Book = Book
  { author :: Text
  , title  :: Text
  , year   :: Int
  } deriving Generic

instance ToJSON Book

book :: Text -> Text -> Int -> Book
book = Book
```

We need a "book database". For the purpose of this guide, let's restrict ourselves to the following books.

``` haskell
books :: [Book]
books =
  [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
  , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
  , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
  , book "Graham Hutton" "Programming in Haskell" 2007
  , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
  , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
  ]
```

Now, given an optional search string `q`, we want to perform a case insensitive search in that list of books. We're obviously not going to try and implement the best possible algorithm, this is out of scope for this tutorial. The following simple linear scan will do, given how small our list is.

``` haskell
searchBook :: Monad m => Maybe Text -> m (Search Book)
searchBook Nothing  = return (mkSearch "" books)
searchBook (Just q) = return (mkSearch q books')

  where books' = filter (\b -> q' `T.isInfixOf` T.toLower (author b)
                            || q' `T.isInfixOf` T.toLower (title b)
                        )
                        books
        q' = T.toLower q
```

We also need an endpoint that generates random points `(x, y)` with `-1 <= x,y <= 1`. The code below uses [probable](http://hackage.haskell.org/package/probable) because of the `Applicative` interface but any random generation library will do.

``` haskell
randomPoint :: MonadIO m => m Point
randomPoint = liftIO . mwc $ Point <$> d <*> d
  
  where d = doubleIn (-1, 1)
```

If we add static file serving, our server is now complete.

``` haskell
api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: Server API
server = randomPoint
    :<|> searchBook

server' :: Server API'
server' = server
     :<|> serveDirectory "getting-started/gs9"

app :: Application
app = serve api' server'
```

Why two different API types, proxies and servers though? Simply because we don't want to generate javascript functions for the `Raw` part of our API type, so we need a `Proxy` for our API type `API'` without its `Raw` endpoint.

Very similarly to how one can derive haskell functions, we can derive the javascript with just a simple function call to `jsForAPI` from `Servant.JQuery`.

``` haskell
apiJS :: String
apiJS = jsForAPI api
```

This `String` contains 2 Javascript functions:

``` javascript

function getpoint(onSuccess, onError)
{
  $.ajax(
    { url: '/point'
    , success: onSuccess
    , error: onError
    , method: 'GET'
    });
}

function getbooks(q, onSuccess, onError)
{
  $.ajax(
    { url: '/books' + '?q=' + encodeURIComponent(q)
    , success: onSuccess
    , error: onError
    , method: 'GET'
    });
}
```

Right before starting up our server, we will need to write this `String` to a file, say `api.js`, along with a copy of the *jQuery* library, as provided by the [js-jquery](http://hackage.haskell.org/package/js-jquery) package.

``` haskell
writeJSFiles :: IO ()
writeJSFiles = do
  writeFile "getting-started/gs9/api.js" apiJS
  jq <- readFile =<< JQ.file
  writeFile "getting-started/gs9/jq.js" jq
```

And we're good to go. Start the server with `dist/build/getting-started/getting-started 9` and go to [http://localhost:8081/](http://localhost:8081/). Start typing in the name of one of the authors of our database or part of a book title and check out how long it takes to approximate &pi; using the method mentionned above.

# API docs generation

# Links, community and more
