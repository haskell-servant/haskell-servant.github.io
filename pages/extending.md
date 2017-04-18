---
title: Extending servant
toc: true
---

servant の最高な機能の1つは容易に拡張可能なことです。クライアントライブラリを生成したり認証や
クッキーを扱うようなエクステンションやプラグインは一般に2つの軸に沿っています。1つは新しい結合子
を出すもの、もう1つは結合子を変換するものです。結合子をちょっとした "API DSL" として考えて
みましょう。複数の変換が可能であることは深く織り込み済みです。

2つの軸は servant を拡張する方法を駆使するというわけではなく一般的なやり方です。

# New combinators

`Post`何とかのような結合子を加えることが目的だと仮定しましょう。この結合子は新規作成された
リソースの場所を持つHTTP [Location](http://en.wikipedia.org/wiki/HTTP_location) ヘッダでレスポンスを返します。

初めにデータ型を定義します。

``` haskell
data PostWithLocation a
    deriving Typeable
```

次に、これをどのように変換すべきでしょうか。変換はクラスのインスタンスによって定義されます。
例えば、サーバがどのように振る舞うかを定義したい時、`HasServer` クラスをインスタンス化します。

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

[Post](http://haskell-servant.github.io/servant/src/Servant-API-Post.html#Post) 
の実装と上記を比較すると、ほんの少し違っていることが分かります。サーバの型が `EitherT (Int, String) IO a` 
から `EitherT (Int, String) IO (Link, a)` に変わっています。それはこのエンドポイントを実装する
関数が単純な値を返すわけではなく、リンクと戻り値のタプルを返すということを意味しています。`route`
メソッドの定義の中で、`Location`ヘッダへのリンクを加えるコードも異なっています。そのインスタンスの
定義内で、リクエストやレスポンスの詳細(ヘッダなど)にアクセスしますが、エンドポイントを実装するコードは
アクセスしません。

`HasServer` の元の定義を見ると、`route` の第2引数である `action` が型シノニムインスタンスで関連した
`Server` の型を持っていることが分かります。これは `EitherT (Int, String) IO (Link, a)` に相当します。
`PostWithLocation` は常に任意の route type の最後の要素であるべきなので、これがまさに目的と一致します。
末尾に来る結合子は定義していないので、結合子に決定権を委譲する必要があります。`(:>)` のための `HasServer` 
インスタンスを見てみましょう。

自作の結合子を使ってみましょう。

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

ユースケースによって、自作の結合子のために `HasClient`, `HasDocs` インスタンスも定義したいと思うかも
しれません。その時はコード生成やドキュメント生成の恩恵を受けられます。

# New Interpreters

APIが属している新しい「インタプリタ」を定義してみましょう。`HasServer` と同類の新しい
クラスと、既存の結合子のクラスのインスタンスを書きます。

この新しいインタプリタのもっとも一般的な使い方はコード生成です。[servant-jquery](http://hackage.haskell.org/package/servant-jquery) に特に注目してみましょう。この後見ていく
ものとしては、1つの手法は特定のエンドポイントのためのクライアントを書くのに必要なすべての
情報を表現する record type を持っていることです。インスタンスからインスタンスへその 
record を渡し、最後に結合子(`Get`など)が来るまで詳細を伝えていきます。

# Other Directions

まれなケースとして、servant の拡張がこれらのカテゴリのいずれにも属さないことがあります。
例えば、servant で無料で *HATEOAS* を手に入れたいとしましょう。次のAPIで実現します。

``` haskell
type MyAPI = "user" :> ReqBody User :> Post ()
        :<|> "names" :> Capture "name" String :> Get User
```

`MyServer` サーバは自動的に型を生成します。

``` haskell
type MyAPIResty = Get HATEOASData
            :<|> "user" :> Get HATEOASData
            :<|> "user" :> ReqBody User :> Post ()
            :<|> "names" :> Get HATEOASData
            :<|> "names" :> Capture "name" String :> Get User
```

そのためのサーバはエンドポイントに一致するする限りにおいて `MyServer` のように振る舞い
ます。しかし、他のすべてのエンドポイントを現在のエンドポイントの下にサーバを配置する
ような情報を返すべきです。

型レベルで重要な策略です。特に新しいサーバを生成するために型とサーバを同時に書き換える
クラスを書く場合に用います。

同様に、servant の拡張で興味深いことは API type の外でトライを作るシステムを書き換える
ことで、それに応じてサーバのデータレベル実装を変えるので、ルート参照が線形より速くなります。

もしこれ以上発展的なプロジェクトに関わったり興味があるのでしたら、ぜひ試してみてください。
