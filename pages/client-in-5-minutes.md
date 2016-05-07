---
title: Write a client library for any web API in 5 minutes
toc: true
---

*servant* を使うと、すごく直接的な方法でウェブサービスのリクエストハンドラを書けます。
各種のエンコードやデコードでロジックを汚すことはありません。あまり明らかでないこととしては、
servant の API type で書かれた API にクエリを投げる関数を導出(実際には記述しない)できる
ことによって、それに釣り合う利益も得ることです。以下に例を示します。

# The Hackage API

[Hackage's API](http://hackage.haskell.org/api) の2つのエンドポイントにクエリを投げる関数を書いてみましょう。

```
/users/
GET: json -- list of users

/user/:username
GET: json -- user id info

/packages/
GET: json -- List of all packages
```

*curl* を使って何が出力されているか見てみましょう。

``` bash
$ curl -H "Accept: application/json" http://hackage.haskell.org/users/
[{"username":"admin","userid":0}, ...]
$ curl -H "Accept: application/json" http://hackage.haskell.org/user/AlpMestanogullari
{"groups":["/package/gloss-juicy/maintainers","/package/hnn/maintainers","/package/hspec-attoparsec/maintainers","/package/kmeans-vector/maintainers","/package/pastis/maintainers","/package/probable/maintainers","/package/servant-client/maintainers","/package/servant-docs/maintainers","/package/servant-jquery/maintainers","/package/servant-pool/maintainers","/package/servant-postgresql/maintainers","/package/servant-response/maintainers","/package/servant-scotty/maintainers","/package/servant-server/maintainers","/package/servant/maintainers","/package/sitemap/maintainers","/package/statistics-linreg/maintainers","/package/taggy-lens/maintainers","/package/taggy/maintainers","/packages/uploaders"],"username":"AlpMestanogullari","userid":75}
$ curl -H "Accept: application/json" http://hackage.haskell.org/packages/
[{"packageName":"3d-graphics-examples"},{"packageName":"3dmodels"}, ...]
```

初めてとしては十分でしょう。

# Describing Hackage's API as a type

初めに 言語拡張と import を書きます。

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

興味のある3つのエンドポイントに対応する API type を書いてみましょう。

``` haskell
type HackageAPI =
       "users" :> Get '[JSON] [UserSummary]
  :<|> "user" :> Capture "username" Username :> Get '[JSON] UserDetailed
  :<|> "packages" :> Get '[JSON] [Package]
```

JSON で出力されることを明らかに期待していることを除けば、何もおかしいことはありません。
(これは適切な `Accept` header を挿入します)

# Data types and JSON serialization

いくつかの型も定義します。JSON のデシリアライズインスタンスも合わせて定義します。

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

最後に、クライアント関数は自動的に導出されます。

``` haskell
hackageAPI :: Proxy HackageAPI
hackageAPI = Proxy

getUsers :: EitherT ServantError IO [UserSummary]
getUser :: Username -> EitherT ServantError IO UserDetailed
getPackages :: EitherT ServantError IO [Package]
getUsers :<|> getUser :<|> getPackages = client hackageAPI (BaseUrl Http "hackage.haskell.org" 80)
```

期待通りにすべてが動くことを実際に確認するコードは以下の通りです。

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

コードを動かしてみましょう。

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

すべてのコードは [servant's repo](http://github.com/haskell-servant/servant) で利用できます。`servant-examples/hackage` ディレクトリ以下にあります。
