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

## Tutorials and Blog Posts

- [Tutorial](/tutorial) (it's recommended to start here)
- [Paper](http://www.andres-loeh.de/Servant/servant-wgp.pdf) for a more technical introduction
- [Extending servant](/extending.html)
- [Hackage API client in 5 minutes](/client-in-5-minutes.html)
- [Developing a servant application with Halcyon](https://halcyon.sh/tutorial/)
- [Type-Safe Microservices in Haskell with Servant](https://github.com/k-bx/owlcloud)
- [Type-safe web services in Haskell with servant](http://taylor.fausak.me/2015/08/23/type-safe-web-services-in-haskell-with-servant/)
- [Servant, Type Families, and Type-level Everything](http://www.arow.info/blog/posts/2015-07-10-servant-intro.html)
- [Combining Servant With Persistent](http://www.parsonsmatt.org/programming/2015/06/07/servant-persistent.html)
- See [the talks](/talks.html)

## Examples

*servant* comes with quite a few examples, among which the ones studied in the [tutorial](/tutorial). You can find them [here](https://github.com/haskell-servant/servant/tree/master/servant-examples).

## Haddocks

- [servant](http://hackage.haskell.org/package/servant), API types and combinators
- [servant-server](http://hackage.haskell.org/package/servant-server), for creating a server
- [servant-client](http://hackage.haskell.org/package/servant-client), for generating Haskell functions to query APIs
- [servant-jquery](http://hackage.haskell.org/package/servant-jquery), for generating Javascript functions to query APIs
- [lackey](https://hackage.haskell.org/package/lackey), for generating Ruby functions to query APIs
- [servant-docs](http://hackage.haskell.org/package/servant-docs), for assistance in API docs generation
- [servant-mock](http://hackage.haskell.org/package/servant-mock), for generating conformant servers automatically
- [servant-blaze](http://hackage.haskell.org/package/servant-blaze), for HTML with blaze-html
- [servant-lucid](http://hackage.haskell.org/package/servant-lucid), for HTML with lucid
- [servant-ede](https://hackage.haskell.org/package/servant-ede), for EDE templates
- [servant-JuicyPixels](https://hackage.haskell.org/package/servant-JuicyPixels), for using JuicyPixels-backend content-types
- [servant-pandoc](https://hackage.haskell.org/package/servant-pandoc), for rendering API documentation with Pandoc.

## Mailing list and IRC

- [Mailing list](https://groups.google.com/forum/#!forum/haskell-servant)
- **#servant** on the freenode IRC network
