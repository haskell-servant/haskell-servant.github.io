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
  , registration_date :: ZonedTime
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
- the `:>` operator that separates the various "combinators" just lets you sequence static path fragments, URL captures and other combinators. The ordering only matters for static path fragments and URL captures. `"users" :> "list-all" :> Get '[JSON] [User]`, equivalent to `/users/list-all`, is obviously to the same as `"list-all" :> "users" :> Get '[JSON] [User]`, which is equivalent to `/list-all/users`. This means that sometimes `:>` is somehow equivalent to `/`, but sometimes it just lets you chain another combinator.

One might wonder: how do we describe an API with more than one endpoint? Our answer to this is simple, just a little operator that we named `:<|>`. Here's an example:

``` haskell
type UserAPI = "users" :> "list-all" :> Get '[JSON] [User]
          :<|> "list-all" :> "users" :> Get '[JSON] [User]
```

*servant* provides a fair amount of combinators out-of-the-box and lets you write your owns when you need it. Here's a quick overview of all the combinators that servant comes with.

## Combinators

### Static strings

### `Delete`, `Get`, `Patch`, `Post` and `Put`

### `Capture` and `MatrixParam`

### `QueryParam`, `QueryParams` and `QueryFlag`

### `ReqBody`

### Request `Header`s

### Content types

### Response `Headers`

### Interoperability with other WAI `Application`s: `Raw`

# Serving an API

# Deriving Haskell functions to query an API

# Deriving Javascript functions to query an API

# API docs generation

# Links, community and more
