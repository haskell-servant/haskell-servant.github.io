---
title: Extending servant
toc: true
---

One of the best features of servant is that it is easily extensible. Extensions and plugins, such as client-library generation and support for authentication or cookies, generally happen along two axes: through the introduction of new combinators, and through new 'interpretations' for the combinators. You can think of the combinators as a little "API DSL", which is a deep-embedding in that multiple interpretations for it are possible.

Note that the two axes don't quite exhaust the ways in which you can extend servant, but they are the most common.

# New combinators

Let's suppose our objective was to add a `Post`-like combinator that returns a response with an HTTP [Location](http://en.wikipedia.org/wiki/HTTP_location) header with the location of a newly-created resource.

First we define a datatype:

``` haskell
data PostWithLocation a
    deriving Typeable
```

Next, we need to describe how this ought to be interpreted. Interpretations are defined via instances of classes. In particular, when we want to define how the server should behave, we instantiate the `HasServer` class:

``` haskell
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import Data.String.Conversions
import Data.Typeable
import Network.HTTP.Types
import Network.Wai
import Servant.Server

data Link = Link Ascii

instance ToJSON a => HasServer (PostWithLocation a) where
  type Server (PostWithLocation a) = EitherT (Int, String) IO (Link, a)

  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodPost = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right (Link link, out) ->
            responseLBS status201 [
                ("Content-Type", "application/json")
                ("Location", link)
                ] (encode out)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodPost =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound
```

If you compare this with the implementation of [Post](http://haskell-servant.github.io/servant/src/Servant-API-Post.html#Post), you'll see that very little changed. We've changed the type of the associated `Server` type to be a `EitherT (Int, String) IO (Link, a)` instead of `EitherT (Int, String) IO a`. This means that the function that ultimately implements this endpoint must return a tuple of the link and the return value, and not just the return value. In the definition of the `route` method, we also changed the code to add the link to the `Location` header. Note how in the definition of the instance, we have access to the details (e.g., headers) of the request and response, whereas the code that implements the endpoint doesn't (or at least not directly - we could, if we so desired, pass all of the details of the request to the function, creating a new combinator).

If we look at the original definition of `HasServer`, we see that the second parameter of `route` -- in this case, `action` has the type of the `Server` associated type synonym instance. In this case, that is our `EitherT (Int, String) IO (Link, a)`. This is just what we wanted in this case, because `PostWithLocation` should always be the last element of any route type. But if we were defining a combinator that wasn't at the end, we would likely need to delegate some decision-making to combinators further on down the line. Look at the `HasServer` instance for `(:>)` if you're curious how that works.

We can now use our combinator:

``` haskell
type MyAPI = "user" :> ReqBody User :> PostWithLocation ()

myAPI :: Proxy MyAPI
myAPI = Proxy

server :: Server MyAPI
server = mkNewUser
    where
      mkNewUser :: User -> EitherT (Int, String) IO (Link, a)
      mkNewUser = ...
```

Depending on your use case, you may also want to define `HasClient` and `HasDocs` instances for your combinator, so that you (and other people) can benefit from code and documentation generation.

# New Interpreters

If you've come this far, you should already have a sense for what defining new 'interpreters' for the API consists of. You write a new class, akin to `HasServer`, and instances of that class for existing combinators.

The most obvious use of a new interpreter is code generation. I highly recommend taking a look at [servant-jquery](http://hackage.haskell.org/package/servant-jquery) for inspiration. As you'll see, one approach is to have a record type that represents all the information you need to write a client for a particular endpoint, and then pass that record along, from instance to instance, filling in the details until you reach an end combinator (`Get`, `Post`, etc.).

# Other Directions

In rare cases, extensions to servant may involve something that doesn't quite belong in either of these categories. For instance, a distant dream of mine in getting *HATEOAS* for free in servant. The idea is that given an API:

``` haskell
type MyAPI = "user" :> ReqBody User :> Post ()
        :<|> "names" :> Capture "name" String :> Get User
```

And a server `MyServer` for it, we would *automatically* create a type:

``` haskell
type MyAPIResty = Get HATEOASData
            :<|> "user" :> Get HATEOASData
            :<|> "user" :> ReqBody User :> Post ()
            :<|> "names" :> Get HATEOASData
            :<|> "names" :> Capture "name" String :> Get User
```

And a server for it, that behaves just like `MyServer` insofar as their endpoints coincide, but would return information about the server's layout beneath the current endpoint for all other endpoints.

This involves considerable trickery at the type-level. In particular, it involves writing a class that rewrites types and servers hand-in-hand, to generate a new server.

Similarly, an interesting extension to servant would be a rewrite system that makes a trie out of the API type, and correspondingly changing the data-level implementation of a server, so that route lookups can be faster than linear.

If this, or related more advanced projects, sounds interesting to you, get in touch!
