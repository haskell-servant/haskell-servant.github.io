---
title: Deriving Haskell functions to query an API
toc: true
---

API をサーブするハンドラを定義するには数多くのことをやらなければならないが、APIにクエリを投げるのがよりシンプルで、
ウェブサーバの中で何が起こっているのかを気にしません。Webサーバとやり取りする方法とレスポンスの受け取り型を知っている
だけで良いのです。ただし、APIの構造がリッチではなく大量のクライアント側の関数を生成するために調査させることはないので、普通は手でクエリ関数を書かなければならないことはあります。

しかし *servant* は API を調査する方法を持っています。API はただの Haskell 型で Haskell は型でできることがたくさんあるからです。ハンドラが持つべき型を推測するような API を見つけるのと同様の方法で、
`Capture`, `ReqBody`, `QueryParam` などのように1つの引数を持つ Haskell の関数を *derive* する API の構造を推測できます。*derive* によって複雑なコード生成は不要になり、関数は API type の構造だけで定義されます。

この章のソースは literate haskell file として書かれています。
まず、言語拡張と import が必要です。

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE TypeOperators #-}
>
> module Client where
>
> import Control.Monad.Trans.Either
> import Data.Aeson
> import Data.Proxy
> import GHC.Generics
> import Servant.API
> import Servant.Client

次はドメインを特定するデータ型です。

> data Position = Position
>   { x :: Int
>   , y :: Int
>   } deriving (Show, Generic)
>
> instance FromJSON Position
>
> newtype HelloMessage = HelloMessage { msg :: String }
>   deriving (Show, Generic)
>
> instance FromJSON HelloMessage
>
> data ClientInfo = ClientInfo
>   { clientName :: String
>   , clientEmail :: String
>   , clientAge :: Int
>   , clientInterestedIn :: [String]
>   } deriving Generic
>
> instance ToJSON ClientInfo
>
> data Email = Email
>   { from :: String
>   , to :: String
>   , subject :: String
>   , body :: String
>   } deriving (Show, Generic)
>
> instance FromJSON Email

本題に入ります。前章をふまえて次のような API type を考えてみましょう。

> type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
>       :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
>       :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

*servant-client* で得ようとしている3つの関数を示します。
それぞれがエンドポイントにクエリを投げます。

> position :: Int -- ^ value for "x"
>          -> Int -- ^ value for "y"
>          -> EitherT ServantError IO Position
>
> hello :: Maybe String -- ^ an optional value for "name"
>       -> EitherT ServantError IO HelloMessage
>
> marketing :: ClientInfo -- ^ value for the request body
>           -> EitherT ServantError IO Email

関数はそれぞれ、レスポンスが依存する任意の値の引数として、API type を確かにするものとして使うことができます。
それではどのようにしてこれらの関数を書けばよいのでしょうか。実は `Proxy` を API とリクエストを送るホストに
与えれば良いのです。

> api :: Proxy API
> api = Proxy
>
> position :<|> hello :<|> marketing = client api (BaseUrl Http "localhost" 8081)

上記のコードを見ると、これらの関数のパターンマッチになっていることが分かります。もしAPI内にあるエンドポイントよりも
多いまたは少ない関数を導こうとすると、明らかにエラーになります。`BaseUrl` の値は次のようになります。

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

できました。クライアントの関数を使うコードを書いてみましょう。

> queries :: EitherT ServantError IO (Position, HelloMessage, Email)
> queries = do
>   pos <- position 10 10
>   msg <- hello (Just "servant")
>   em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
>   return (pos, msg, em)
>
> run :: IO ()
> run = do
>   res <- runEitherT queries
>   case res of
>     Left err -> putStrLn $ "Error: " ++ show err
>     Right (pos, msg, em) -> do
>       print pos
>       print msg
>       print em


`dist/build/tutorial/tutorial 8` でサーバを動かせます。
クライアントは `dist/build/t8-main/t8-main` で動きます。

``` bash
 $ dist/build/tutorial/tutorial 8
 # and in another terminal:
 $ dist/build/t8-main/t8-main
 Position {x = 10, y = 10}
 HelloMessage {msg = "Hello, servant"}
 Email {from = "great@company.com", to = "alp@foo.com", subject = "Hey Alp, we miss you!", body = "Hi Alp,\n\nSince you've recently turned 26, have you checked out our latest haskell, mathematics products? Give us a visit!"}
```

関数の引数の型は(サーバ側の)リクエストハンドラと同じです。以上が *servant-client* の使い方です！

<div style="text-align: center;">
  <p><a href="/tutorial/server.html">Previous page: Serving an API</a></p>
  <p><a href="/tutorial/javascript.html">Next page: Generating javascript functions to query an API</a></p>
</div>
