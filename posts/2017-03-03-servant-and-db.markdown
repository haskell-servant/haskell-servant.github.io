---
title: Passing a DB connection to handlers in Servant
author: Oleg Grenrus
tags: servant
---

This post is originally published in http://oleg.fi/gists/posts/2017-03-03-servant-and-db.html. This version is updated to use `hoistServer`.

This write-up is motivated by discussion in
[servant/#704 issue](https://github.com/haskell-servant/servant/issues/704).
I try to summarize the main points.


As this is a literate haskell file, we'll need to do a small prelude dance:
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control
import Database.PostgreSQL.Simple (Connection)
import Log
import Servant
import qualified Control.Category

-- | Needed for 'MonadLog (LogT Handler)' instance
instance MonadTime Handler where
    currentTime = liftIO currentTime
```

The problem
-----------

The issue started as instance XY-problem:

- **Y**: Docs explaining how to actually create a full combinator (ex. one to create/store a DB connection)
- **X**: How to pass a db connection to the handlers.

I won't answer to the **Y**, how to write combinators is different topic (have to write about that later).
Let's see how to deal with **X**, by implementing a small Cat CR(UD) API:
```haskell
-- we should have proper data/newtypes, but then we'll need to write instances.
-- we'll try to keep a boilerplate at the minimum in this example.
type Cat = Text
type CatName = Text

type API = "cat" :> Capture "name" CatName :> Put '[JSON] Cat  -- create
      :<|> "cat" :> Capture "name" CatName :> Get '[JSON] Cat  -- read

api :: Proxy API
api = Proxy
```

Now we'll need to implement the api, we'll write a basic Haskell functions,
which we would write anyway, we could reuse them in a console application, for example.
```haskell
createCat :: MonadIO m => Connection -> CatName -> m Cat
createCat = error "not implemented"

readCat :: MonadIO m => Connection -> CatName -> m Cat
readCat = error "not implemented"
```

And the problem is that if we try to do
```foo
-- THIS DOESN'T WORK
app :: Application
app = serve api $ createCat :<|> readCat
```
it will fail with a type-error message from GHC. Obviously, GHC cannot conjure
`Connection` for us. We need to pass it in somehow.

Partial application
-------------------

*Partial application* is a simple tool. We can partially apply the
implementation to fit into type required by `serve`. We'll make a situation a
bit more interesting by using a connection pool:
```haskell
app :: Pool Connection -> Application
app pool = serve api $
    withResource1 pool createCat :<|> withResource1 pool readCat
  where
    withResource1 :: MonadBaseControl IO m => Pool a -> (a -> b -> m c) -> b -> m c
    withResource1 pool f b = withResource pool $ \a -> f a b
```

As you can see we'd need to wrap every handler in `withResource1`.
It's not very elegant, but **it works**. And is very **simple** to understand.

hoistServer
-----------

`servant` offers the
[`hoistServer`](http://hackage.haskell.org/package/servant-server-0.14/docs/Servant-Server.html#v:hoistServer)
helper function.
which let's you to remove this kind of
boilerplate.  We'll rewrite our handlers in MTL-style, with a `MonadDB` type
class. For the sake of example let's also add a `MonadLog` from
[`log-base`](http://hackage.haskell.org/package/log-base) to the first endpoint.
```haskell
class MonadDB m where
    withConnection :: (Connection -> m a) -> m a

createCat' :: (MonadDB m, MonadLog m) => CatName -> m Cat
createCat' = error "not implemented"

readCat' :: (MonadDB m) => CatName -> m Cat
readCat' = error "not implemented"
```

Looks good, but how we'll pass a connection (and a logger)? The answer is
obvious, when you know it: we'll need to use a concrete monad implementation, for example:

``` haskell
newtype H a = H { runH :: ReaderT (Pool Connection) (LogT Handler) a }
   deriving (Functor, Applicative, Monad, MonadTime, MonadLog)

instance MonadDB H where
    withConnection f = H $ do
        pool <- ask
        withResource pool $ \conn -> runH (f conn)
```

And now `hoistServer` will do the magic:
```haskell
app' :: Pool Connection -> Logger -> Application
app' pool logger = serve api $ hoistServer api nt $ createCat' :<|> readCat'
  where
    nt :: H x -> Handler x
    nt m -> runLogT "api" logger (runReaderT (runH m) pool)
```

The `nt` (for natural transformation) tells how to transform the concrete monad
`H` into servant's `Handler`. The `hoistServer` machinery walks through `ServerT H`
value and applies that transformation, resulting into `ServerT Handler` value.
If `api` has `HasServer` instance, you can `hoistServer` it.

The `hoistServer` is most useful when you have polymorphic handlers defined with
mtl-like monad type-classes, so you can instantiate them all with the same concrete
monad at then end. Note: that if we had concrete `LogT Handler` in some
handler, and `ReaderT (Pool Connection) Handler` in some other one, `hoistServer`
won't help!

So to conclude:

- start with *partial application* to pass arguments into handlers
- later you may transfer to use fancier `hoistServer`.

[Alp Mestanogullari summarised it well](https://github.com/haskell-servant/servant/issues/704#issuecomment-283396827):
*gradually reach for fancier things as your needs grow, never when it's not required*.
