---
title: Pulling a mock server for your APIs out of thin air
author: Alp Mestanogullari
date: 2015-07-22 22:00
toc: true
---

# Summary 

A couple of days ago, *marcushg* mentionned on the **#servant** IRC channel that one could probably easily use the information available from API types to "derive" a mock implementation of your request handlers that just generates random values of whatever the return type of the handlers are. Julian and I discussed this a bit today and I just went ahead and wrote down our thoughts in a new branch. The result will be explained in this post and lets us taking a type describing a web API, such as:

``` haskell
type API = "user" :> Get '[JSON] User
```

and generate request handlers that just respond with random values of the appropriate type, `User` in our case. In *servant*/*wai* terms, this means we get a `mock` function with the type:

``` haskell
mock :: HasMock api => Proxy api -> Server api
```

i.e "given an API type, please generate a mock server for such an API". This effectively means "please pull a mock server out of thing air for me".

Out of thing air, really? Not exactly. But let's start by clearly stating the problem.

# The Problem

*servant* lets you describe web applications with a Haskell type using the combinators from servant's type-level EDSL. Such a type would be:

``` haskell
-- In English:
-- the API has one endpoint, under /user, which returns
-- response bodies in JSON that describe values of type User
type API = "user" :> Get '[JSON] User
```

where `User` could be defined as:

``` haskell
newtype User = User { username :: String }
```

The goal would be to "automagically" derive a request handler of the right type that we could use as a placeholder until we properly implement a handler that talks to the database and responds with "the real data".

For anyone not familiar with *servant* already, you just need to know that it means we need to somehow automatically implement a function with the type:

``` haskell
getUser :: EitherT ServantErr IO User
```

possibly by constraining the user to provide an instance for some random generation class.

# The Plan

Just like *servant-server*, *servant-client* and others, we need a class whose instances will define the way we interpret each combinator, in a way very specific to this task: we will produce what *servant-server* takes as input, i.e request handlers! This all means we are basically looking at a class like:

``` haskell
class HasServer api => HasMock api where
  mock :: Proxy api -> Server api
```

where `Server api` just computes the types of the all the request handlers of an *API type*. In our case, `Server api` is computed as follows:

``` haskell
-- api = the API type from above in our case
Server API = Server ("user" :> Get '[JSON] User)
           -- static strings in the path do not influence
           -- the type of a handler
           = Server (Get '[JSON] User)
           -- EitherT ServantErr IO is the default monad
           -- in which handlers run
           = Either ServantErr IO User
```

So we have to implement at least support for static string fragments in the path and the `Get` combinator (i.e handlers for HTTP GET requests).

# `HasMock` instances

Let's start with the one for static path fragments, it's the simplest one: we ignore the string bit and move on to what comes after.

``` haskell
instance (KnownSymbol path, HasMock rest) => HasMock (path :> rest) where
  mock _ = mock (Proxy :: Proxy rest)
```

Don't be scared by `KnownSymbol`, it basically just means "`path` is a type-level string", that is, a string that appears in a type.

Next comes the one for `Get`. This one is trickier: this is the combinator that says what type the handler returns. The returned value then gets encoded into JSON, HTML, CSV or any format of your choice. In our case, the handler returns a `User` and can only encode it in the JSON format.

Now the heart of the matter really is: we know we need to return an `User` and our `EitherT ServantErr IO` monad mentions `IO`, couldn't we randomly generate an `User`? Yes, we can! For the purpose of a mock server, we will simply use [QuickCheck](http://hackage.haskell.org/package/QuickCheck)'s `Arbitrary` class, which represents types for which we can generate random values, given a random number generator.

``` haskell
class Arbitrary a where
  arbitrary :: Gen a
  -- and another method, but optional
```

The `Gen` type provides instances for the `Functor`, `Applicative` and `Monad` classes and `Arbitrary` comes with instances for many of the types in *base*.

This essentially means writing an `Arbitrary` instance for `User` is as simple as:

``` haskell
instance Arbitrary User where
  -- we just rely on the arbitrary instance for lists of
  -- chars, i.e Strings, and use the Functor instance for Gen
  arbitrary = fmap User arbitrary
```

If you have multiple fields, you can use the usual combo of `<$>` (i.e `fmap`) and `<*>` (comes with `Applicative`).

``` haskell
-- a point: x, y coordinates
data Point = Point Double Double

instance Arbitrary Point where
  arbitrary = Point <$> arbitrary <*> arbitrary
```

Once you have an `Arbitrary` instance, in order to generate a random value using your instance, you have to call a function called... `generate`!

``` haskell
generate :: Gen a -> IO a
```

Putting the two together, we get:

``` haskell
generate arbitrary :: Arbitrary a => IO a
```

All we need to do is just "lift" that up into our `EitherT ServantErr IO` monad, which is exactly what `Control.Monad.IO.Class.liftIO` is about in the [transformers](http://hackage.haskell.org/package/transformers) package.

``` haskell
liftIO (generate arbitrary) :: Arbitrary a => EitherT ServantErr IO a
-- actually, since `liftIO` comes from the `MonadIO` class,
-- the most general type for this expression is:
liftIO (generate arbitrary) :: (MonadIO m, Arbitrary a) => m a
```

In order to automatically "fill" request handlers with this expression we just need to write the `HasMock` instance for `Get`, shown below.

``` haskell
instance (Arbitrary a, AllCTRender ctypes a) => HasMock (Get ctypes a) where
  mock _ = liftIO (generate arbitrary)
```

The `AllCTRender` constraint just says "we know how to encode values of type `a` in the formats listed in the `Get` combinator".

And that's it! You can now actually use all of this to put together a mock server for our little API.

# Using `mock`

All we need to do to run the mock server is call *servant-server*'s `serve` function. It is illustrated below, along with **all** of the code you'd have to write if you were to use this mock-generation feature (aside from language pragmas and imports).

``` haskell
-- 1/ define our user type, deriving the Arbitrary instance
--    since it's just a newtype and we can use the
--    GeneralizedNewtypeDeriving extension. We also
--    derive the Generic class to get our JSON encoding
--    functions for free.
newtype User = User { username :: String }
  deriving (Arbitrary, Generic)

-- 2/ we get the JSON encoding for free
instance ToJSON Generic

-- 3/ recall our API type
type API = "user" :> Get '[JSON] User

-- 4/ define this simple Proxy.
-- for any given type 'a', there's only one value of type 'Proxy a'
-- that is not equivalent to error "foo" and the likes, a real honest value.
-- The way to build this value is to use the Proxy constructor.
-- In other words, this value lets us target one precise type. Servant
-- uses this to tie the type-level information with value-level data.
api :: Proxy API
api = Proxy

-- 5/ we magically "derive" the mock server.
-- the run function comes from the warp webserver,
-- http://hackage.haskell.org/package/warp
-- 'mock' is the method of the `HasMock` class we've
-- developed in this post.
-- This will run a mock web server with an endpoint at
-- http://localhost:8080/user that generates random values
-- of type User
main :: IO ()
main = run 8080 (serve api $ mock api)
```

Our little program in action:

``` bash
$ curl localhost:8080/user
{"username":"Yv\rG\u0014±Ssv\u001e>\u001aVF\u001e\u000fM5ø\u000ctefÚJ\u0001K4"}
# yes, a truly original username.
```

In practice, this is really, honestly all you have to do to put together a mock server for an API type. You can find the complete code for this in the work-in-progress [servant-mock](https://github.com/haskell-servant/servant/tree/servant-mock/servant-mock) package on github. The example can be found under `example/main.hs` there.

There are many more `HasMock` instances than the ones I have shown here of course -- there's one for all the combinators provided by the *servant* package! So you can take any API type out there and just create a mock server for it, as long as you provide `Arbitrary` instances for your data types. Nothing too interesting though, but feel free to take a look at `src/Servant/Mock.hs` in the repository if you want to read the other instances.

# Other news

- I mentionned in [a previous post](https://haskell-servant.github.io/posts/2015-05-25-servant-paper-wgp-2015.html) that we had submitted a paper for the *Workshop on Generic Programming*, co-located with ICFP'15, in Vancouver this year. Well, the paper has been accepted!
- Therefore, Julian Arni and/or Andres Löh will be giving a talk about servant there.
- In addition to this, Julian recently gave a talk at [Curry-On!](http://www.curry-on.org/). The video will be uploadedd on [their Youtube channel](https://www.youtube.com/channel/UC-WICcSW1k3HsScuXxDrp0w) in the upcoming days.
- I submitted a very hands-on talk proposal for [the Haskell eXchange 2015](https://skillsmatter.com/conferences/7069-haskell-exchange-2015) in London. The programme hasn't been decided yet, but this is a very nice event nonetheless, so if you're not too far, consider attending!

