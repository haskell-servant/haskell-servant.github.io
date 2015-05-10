---
title: Home
---

# Webservice API combinators

*servant* is a set of libraries that makes building Haskell webservices a breeze. With only the thinnest sliver of boilerplate, you get a type-safe service that is capable of generating client-side code (Haskell or Javascript) and documentation for free. See for yourself.

## A webservice API is a type

``` haskell
             -- GET /date
type MyAPI = "date" :> Get '[JSON] Date
             -- GET /time/:tz
        :<|> "time" :> Capture "tz" Timezone :> Get '[JSON] Time
```

This represents an API with two endpoints, both of which accept only GET requests, and which return a (JSON representation of) a `Date` and `Time` object, respectively.

## ... that can be served ...

``` haskell
server :: Server MyAPI
server = getDate :<|> getTimeForTZ

  where getDate = liftIO getCurrentDate
        getTimeForTZ tz = liftIO $ getTimeAtTZ tz

-- assuming we have
getCurrentDate :: IO Date
getTimeAtTZ :: Timezone -> IO Time

main :: IO ()
main = run 8000 $ serve myAPI server
    where myAPI :: Proxy MyAPI
          myAPI = Proxy
```

One handler per endpoint. The handlers' types must match those described in the endpoint. URL captures, GET parameters, headers or request body must be explicitly mentioned and get automatically turned into arguments to the handlers.

### ... and more!

This representation lets us generate client-side querying functions automatically, in Haskell and Javascript. API docs too!

To find out more, please check our [tutorial](/tutorial).

# Documentation and community

## Tutorials

- [Tutorial](/tutorial) (it's recommended to start here)
- [Extending servant](/extending.html)
- [Hackage API client in 5 minutes](/client-in-5-minutes.html)
- [Developing a servant application with Halcyon](https://halcyon.sh/tutorial/)
- See [the talks](/talks.html)

## Examples

*servant* comes with quite a few examples, among which the ones studied in the [tutorial](/tutorial). You can find them [here](https://github.com/haskell-servant/servant/tree/master/servant-examples).

## Haddocks

- [servant](http://hackage.haskell.org/package/servant), API types and combinators
- [servant-server](http://hackage.haskell.org/package/servant-server), for creating a server
- [servant-client](http://hackage.haskell.org/package/servant-client), for generating haskell functions to query APIs automatically
- [servant-jquery](http://hackage.haskell.org/package/servant-jquery), for generating javascript functions to query APIs automatically
- [servant-docs](http://hackage.haskell.org/package/servant-docs), for assistance in API docs generation

## Mailing list and IRC

- [Mailing list](https://groups.google.com/forum/#!forum/haskell-servant)
- **#servant** on the freenode IRC network
