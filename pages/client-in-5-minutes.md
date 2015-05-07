---
title: Write a client library for any web API in 5 minutes
toc: true
---

*servant* lets us write request handlers for webservices in a quite straighforward way, without polluting your logic with encoding/decoding of all sorts. What may be less obvious is that you also get a somehow symetric benefit too by being able to *derive* (without *actually writing them*) functions to query an API described by some servant API type. Here's an example.

# The Hackage API

Let's write some functions to query a couple of endpoints of [Hackage's API](http://hackage.haskell.org/api). Let's just consider the following ones:

```
/users/
GET: json -- list of users

/user/:username
GET: json -- user id info

/packages/
GET: json -- List of all packages
```

Let's see what the output looks like by using *curl*:

``` bash
$ curl -H "Accept: application/json" http://hackage.haskell.org/users/
[{"username":"admin","userid":0}, ...]
$ curl -H "Accept: application/json" http://hackage.haskell.org/user/AlpMestanogullari
{"groups":["/package/gloss-juicy/maintainers","/package/hnn/maintainers","/package/hspec-attoparsec/maintainers","/package/kmeans-vector/maintainers","/package/pastis/maintainers","/package/probable/maintainers","/package/servant-client/maintainers","/package/servant-docs/maintainers","/package/servant-jquery/maintainers","/package/servant-pool/maintainers","/package/servant-postgresql/maintainers","/package/servant-response/maintainers","/package/servant-scotty/maintainers","/package/servant-server/maintainers","/package/servant/maintainers","/package/sitemap/maintainers","/package/statistics-linreg/maintainers","/package/taggy-lens/maintainers","/package/taggy/maintainers","/packages/uploaders"],"username":"AlpMestanogullari","userid":75}
$ curl -H "Accept: application/json" http://hackage.haskell.org/packages/
[{"packageName":"3d-graphics-examples"},{"packageName":"3dmodels"}, ...]
```

This is enough to get us started.

# Describing Hackage's API as a type

First, some pragmas and imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client

import qualified Data.Text    as T
import qualified Data.Text.IO as T
```

Now, let's write the API type that corresponds to those 3 endpoints we're interested in.

``` haskell
type HackageAPI =
       "users" :> Get '[JSON] [UserSummary]
  :<|> "user" :> Capture "username" Username :> Get '[JSON] UserDetailed
  :<|> "packages" :> Get '[JSON] [Package]
```

Nothing fancy here, except that we clearly specify
we are expecting the output to be in JSON (this will insert the appropriate `Accept` header).

# Data types and JSON serialization

We also need some types to go with that: `UserSummary`, `Username`, `UserDetailed`, `Package`. Here they are, along with JSON deserialization instances.

``` haskell
type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Eq, Show)

instance FromJSON UserSummary where
  parseJSON (Object o) =
    UserSummary <$> o .: "username"
                <*> o .: "userid"

  parseJSON _ = mzero

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Eq, Show, Generic)

instance FromJSON UserDetailed

newtype Package = Package { packageName :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON Package
```

# Deriving functions to query hackage

Finally, we can automatically derive our client functions:

``` haskell
hackageAPI :: Proxy HackageAPI
hackageAPI = Proxy

getUsers :: EitherT ServantError IO [UserSummary] 
getUser :: Username -> EitherT ServantError IO UserDetailed
getPackages :: EitherT ServantError IO [Package]
getUsers :<|> getUser :<|> getPackages = client hackageAPI (BaseUrl Http "hackage.haskell.org" 80)
```

And here's some runnable code to actually check that everything works as expected:

``` haskell
main :: IO ()
main = print =<< uselessNumbers

uselessNumbers :: IO (Either ServantError ())
uselessNumbers = runEitherT $ do
  users <- getUsers
  liftIO . putStrLn $ show (length users) ++ " users"

  user <- liftIO $ do
    putStrLn "Enter a valid hackage username"
    T.getLine
  userDetailed <- run (getUser user)
  liftIO . T.putStrLn $ user <> " maintains " <> T.pack (show (length $ groups userDetailed)) <> " packages"
  
  packages <- run getPackages 
  let monadPackages = filter (isMonadPackage . packageName) packages
  liftIO . putStrLn $ show (length monadPackages) ++ " monad packages"

  where isMonadPackage = T.isInfixOf "monad"
```

Here's a sample run:

```
$ cabal run hackage
Preprocessing executable hackage for servant-examples-0.3...
Running hackage...
2460 users
Enter a valid hackage username
AlpMestanogullari
AlpMestanogullari maintains 20 packages
130 monad packages
Right ()
```

# Code

The whole code is available in [servant's repo](http://github.com/haskell-servant/servant), under the `servant-examples/hackage` directory.
