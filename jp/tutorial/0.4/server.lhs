---
title: Serving an API
toc: true
---

型レベル結合子とAPIの種類についての説明はこのくらいにして、いよいよウェブサービスに進みましょう。

このガイドに書いてあることを試してみるには、下記をコマンドを入力します。

``` bash
cabal get servant-examples
cd servant-examples-<VERSION>
cabal sandbox init
cabal install --dependencies-only
cabal configure && cabal build
```

上記のコマンドは `tutorial` 実行ファイルを `dist/build/tutorial` というパスに生成します。
コマンドライン引数に対応するサンプル番号を入れて実行します。

``` bash
$ dist/build/tutorial/tutorial
Usage:   tutorial N
        where N is the number of the example you want to run.
```

A first example
===============

APIを作る基礎知識は身につけたので、最初のウェブサービスを書いてみましょう。

この章のソースはliterate haskell fileで書いてあります。
いくつかの言語拡張とライブラリインポートが必要です。

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeOperators #-}
>
> module Server where
>
> import Control.Monad.IO.Class
> import Control.Monad.Reader
> import Control.Monad.Trans.Either
> import Data.Aeson
> import Data.Aeson.Types
> import Data.Attoparsec.ByteString
> import Data.ByteString (ByteString)
> import Data.Int
> import Data.List
> import Data.String.Conversions
> import Data.Time.Calendar
> import GHC.Generics
> import Lucid
> import Network.HTTP.Media ((//), (/:))
> import Network.Wai
> import Network.Wai.Handler.Warp
> import Servant
> import System.Directory
> import Text.Blaze
> import Text.Blaze.Html.Renderer.Utf8
> import qualified Data.Aeson.Parser
> import qualified Text.Blaze.Html

``` haskell
{-# LANGUAGE TypeFamilies #-}
```

**重要**: `Servant` モジュールは *servant-server* パッケージにあります。
ウェブサーバを実行したり幾つかのAPIを実装しています。
`Servant` モジュールは *servant* パッケージからすべての型をエクスポートしなおしています。
リクエストハンドラをウェブサービスにするために必要なすべてのAPIが定義されています。
つまり、依存パッケージに *servant-server* を追加して `Servant` をインポートするだけで
うまくいくので他の心配は要らないということです。

次のAPIからサーバを書きます。

> type UserAPI1 = "users" :> Get '[JSON] [User]

これは `/users` へ GET リクエストを送ると何が見えるかを示しています。

``` javascript
[ {"name": "Isaac Newton", "age": 372, "email": "isaac@newton.co.uk", "registration_date": "1683-03-01"}
, {"name": "Albert Einstein", "age": 136, "email": "ae@mc2.org", "registration_date": "1905-12-01"}
]
```

`User` データ型を定義して、インスタンスを書いてみましょう。

> data User = User
>   { name :: String
>   , age :: Int
>   , email :: String
>   , registration_date :: Day
>   } deriving (Eq, Show, Generic)
>
> instance ToJSON User

何も楽しくはありませんが、2人のユーザのリストを定義しましょう。

> users1 :: [User]
> users1 =
>   [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
>   , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
>   ]

APIの型も書きましょう。

``` haskell
type UserAPI1 = "users" :> Get '[JSON] [User]
```

このようなAPIに対するリクエストを処理するウェブサービスを実際に書いてみましょう。このAPIは
非常に単純で、1つのエンドポイントに絞られています。ウェブアプリケーションの型は `Server`
という *type family* を通して API の型から決まります。(type family は入力と戻り値の型を
型として持つ関数です。)  `Server` type family は大量のリスエストハンドラが持つ正しい型を
対応するAPIの型から判別してくれます。

`Server` type family について知るべきことは、それが裏でひっそりとルーティングを制御
しているので、ビジネスロジックに専念できるということです。また、個々のエンドポイントごと
にハンドラがデフォルトでは `EitherT ServantErr IO` モナド内で動きます。モナド内で返る
値の型は、対応するエンドポイント使われるHTTPメソッド結合子の二番目の引数と同じでなければ
なりません。前述のAPIの場合、`EitherT ServantErr IO [User]` 型のハンドラを用意しなけ
ればなりません。モナドが手に入ったので、ユーザ一覧を返してみましょう。

> server1 :: Server UserAPI1
> server1 = return users1

よくできました。
[wai](http://hackage.haskell.org/package/wai) と [warp](http://hackage.haskell.org/package/warp)
を使えば、`server` を実際のウェブサーバにすることができます。

> userAPI :: Proxy UserAPI1
> userAPI = Proxy
>
> -- 'serve' は servant から与えられたもので WAI アプリケーションと接続します。
> -- "抽象的な" ウェブアプリケーションであってウェブサーバのことではありません。
> app1 :: Application
> app1 = serve userAPI server1

`userAPI` は悲しいかな、ボイラープレートです。(型推論の助けが必要です)
しかし、これ以外にボイラープレートはありません。

これで 8081 ポートで動くウェブサービスができました！

> main :: IO ()
> main = run 8081 app1

これを1つのファイルに書くか、[servant's repo](http://github.com/haskell-servant/servant)
を取ってきて *servant-examples* ディレクトリを見てみましょう。コードは *tutorial/T1.hs* に
あります。`dist/build/tutorial/tutorial 1` コマンドで動作します。

もし動かせたら、ウェブブラウザか curl で `http://localhost:8081/users` を見てみましょう。

``` bash
$ curl http://localhost:8081/users
[{"email":"isaac@newton.co.uk","registration_date":"1683-03-01","age":372,"name":"Isaac Newton"},{"email":"ae@mc2.org","registration_date":"1905-12-01","age":136,"name":"Albert Einstein"}]
```

More endpoints
==============

1つ以上のエンドポイントが必要な時は次のようにしましょう。JSONエンコードされたユーザ情報を見るために `/albert` と `/isaac` を追加します。

> type UserAPI2 = "users" :> Get '[JSON] [User]
>            :<|> "albert" :> Get '[JSON] User
>            :<|> "isaac" :> Get '[JSON] User

それに合わせて、コードも少し直します。

> isaac :: User
> isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
>
> albert :: User
> albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
>
> users2 :: [User]
> users2 = [isaac, albert]

`UserAPI` の中で `:<|>` を使ってエンドポイントを分けたように、ハンドラも `:<|>` を使って分けます。
ただし、API の type で宣言したのと同じ順番で並べなければなりません。

> server2 :: Server UserAPI2
> server2 = return users2
>      :<|> return albert
>      :<|> return isaac

これで完成です！ この例題は `dist/build/tutorial/tutorial 2` で実行できます。
`/users`, `/albert`, `/isaac` で確認してみましょう。

From combinators to handler arguments
=====================================

それで、簡単にちょっとしたウェブサービスを書けるようになったが、上記の2つはどちらも servant にある
"変な"結合子は使っていない。とにかく `QueryParam`, `Capture`, `ReqBody` を使ってみよう。
この項目では、エンドポイントにあるこれらの結合子が、どのようにして対応するハンドラに適切な型を持つ引数を
自動的に受け取らせるかが書いてあります。URLキャプチャやクエリストリングパラメータ、JSONへのデータ変換
などを調べる手間を心配するは必要はありません。心配無用です。

以下のデータ型と関数を使ってサーバの `API` を実装しましょう。

> type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
>       :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
>       :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
>
> data Position = Position
>   { x :: Int
>   , y :: Int
>   } deriving Generic
>
> instance ToJSON Position
>
> newtype HelloMessage = HelloMessage { msg :: String }
>   deriving Generic
>
> instance ToJSON HelloMessage
>
> data ClientInfo = ClientInfo
>   { clientName :: String
>   , clientEmail :: String
>   , clientAge :: Int
>   , clientInterestedIn :: [String]
>   } deriving Generic
>
> instance FromJSON ClientInfo
> instance ToJSON ClientInfo
>
> data Email = Email
>   { from :: String
>   , to :: String
>   , subject :: String
>   , body :: String
>   } deriving Generic
>
> instance ToJSON Email
>
> emailForClient :: ClientInfo -> Email
> emailForClient c = Email from' to' subject' body'
>
>   where from'    = "great@company.com"
>         to'      = clientEmail c
>         subject' = "Hey " ++ clientName c ++ ", we miss you!"
>         body'    = "Hi " ++ clientName c ++ ",\n\n"
>                 ++ "Since you've recently turned " ++ show (clientAge c)
>                 ++ ", have you checked out our latest "
>                 ++ intercalate ", " (clientInterestedIn c)
>                 ++ " products? Give us a visit!"

ハンドラの3つのエンドポイントを実装します。

> server3 :: Server API
> server3 = position
>      :<|> hello
>      :<|> marketing
>
>   where position :: Int -> Int -> EitherT ServantErr IO Position
>         position x y = return (Position x y)
>
>         hello :: Maybe String -> EitherT ServantErr IO HelloMessage
>         hello mname = return . HelloMessage $ case mname of
>           Nothing -> "Hello, anonymous coward"
>           Just n  -> "Hello, " ++ n
>
>         marketing :: ClientInfo -> EitherT ServantErr IO Email
>         marketing clientinfo = return (emailForClient clientinfo)

上記を見てください。ハンドラの型は必要な形に変わっています。
特に注目すべきは以下の箇所です。

  - `Capture "something" a` は 型 `a` の引数を持ちます (`position` の箇所です)
  - `QueryParam "something" a` 型 `Maybe a` の引数を持ちます (エンドポイントは
  クエリ文字列パラメータなしでアクセスできるので、ハンドラにパラメータが必ずしも必要
  ではないことを明示できます。
  - `ReqBody contentTypeList a` は 型 `a` の引数を持ちます。

以上です。
この例題は `dist/build/tutorial/tutorial 3` で実行できます。

``` bash
$ curl http://localhost:8081/position/1/2
{"x":1,"y":2}
$ curl http://localhost:8081/hello
{"msg":"Hello, anonymous coward"}
$ curl http://localhost:8081/hello?name=Alp
{"msg":"Hello, Alp"}
$ curl -X POST -d '{"name":"Alp Mestanogullari", "email" : "alp@foo.com", "age": 25, "interested_in": ["haskell", "mathematics"]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/marketing
{"subject":"Hey Alp Mestanogullari, we miss you!","body":"Hi Alp Mestanogullari,\n\nSince you've recently turned 25, have you checked out our latest haskell, mathematics products? Give us a visit!","to":"alp@foo.com","from":"great@company.com"}
```

参考までに、*servant* の結合子の一覧、引数からハンドラへの変換、引数の型についてまとめました。

 > - `Delete`, `Get`, `Patch`, `Post`, `Put`: これらは引数にはならず、ハンドラの戻り値の型を示します。通常は `EitherT ServantErr IO <something>` のように表されます。
 > - `Capture "something" a` は `a` 型の引数となります.
 > - `QueryParam "something" a`, `MatrixParam "something" a`, `Header "something" a` はすべて `Maybe a` 型の引数となります。クライアントから渡される値がなくても大丈夫です。
 > - `QueryFlag "something"` と `MatrixFlag "something"` は `Bool` 型の引数になります.
 > - `QueryParams "something" a` と `MatrixParams "something" a` は `[a]` 型の引数となります。
 > - `ReqBody contentTypes a` は `a` 型の引数となります.

The `FromText`/`ToText` classes
===============================

servant は URL から `Int` にデコードしたり、リクエストボディから `ClientInfo` の値をデコードしたりする
方法をどうやって知るのでしょうか？ この章と次の章でそれが明らかになります。

`Capture` と `QueryParam` は URL 内にテキストで書かれています。`Header` はリクエストの
メタデータ内にヘッダ名と値が同じようにテキストで書かれています。servant で `FromText` と `ToText`
という2つの型クラスが提供されている理由は、これら各々のテキストから値または値からテキストへと変換できる
ようにするためです。定義は以下のとおりです：

``` haskell
class FromText a where
  fromText :: Text -> Maybe a

class ToText a where
  toText :: a -> Text
```

`Capture` や `QueryParam`, `Header` などは `FromText` インスタンスを提供してあげれば、
正しく動作します。*servant* は十分な数のインスタンスを提供していますが、自分で定義する場合の
例を以下に示します。

> -- 典型的な enumeration
> data Direction
>   = Up
>   | Down
>   | Left
>   | Right
>
> instance FromText Direction where
>   -- {-# LANGUAGE OverloadedStrings #-} が必要
>   fromText "up"    = Just Up
>   fromText "down"  = Just Down
>   fromText "left"  = Just Server.Left
>   fromText "right" = Just Server.Right
>   fromText       _ = Nothing
>
> instance ToText Direction where
>   toText Up           = "up"
>   toText Down         = "down"
>   toText Server.Left  = "left"
>   toText Server.Right = "right"
>
> newtype UserId = UserId Int64
>   deriving (FromText, ToText)

もしくは以下のようにも書けます。

``` haskell
instance FromText UserId where
  fromText = fmap UserId fromText

instance ToText UserId where
  toText (UserId i) = toText i
```

これらの class について言うことは他に何もありません。`Capture`, `QueryParam`, `QueryParams`, 
`MatrixParam`, `MatrixParams`, `Header` そして自作の型を使う時にはそれらのインスタンスが必要です。
`FromText` インスタンスはサーバサイドで、`ToText` インスタンスは *servant-client* でのみ使います。
servant-client については [section about deriving haskell functions to query an API](/tutorial/client.html)
 に書きます。

Using content-types with your data types
========================================

JSON からリクエストボディをデコードするまたはレスポンスから JSON をエンコードするときにも
同じ原則が使われます (JSON は実行可能な例題で、任意の content-type で使うことができます)

この章では *servant* が提供する2つの型クラスを紹介します。

The truth behind `JSON`
-----------------------

`JSON` とは一体何でしょうか？ JSON は *servant* が提供する他の2つの content-type と同じように
ダンプデータ型です。

``` haskell
data JSON
data PlainText
data FormUrlEncoded
data OctetStream
```

明らかにこれは `JSON` になるためのすべてではないし、全く要領を得ないとも言える。*servant* 内の
多くのデータ型と同様に、`JSON` は JSON フォーマットへエンコードする特別なシンボルでもあります。
この関係をつなぐ方法は2つのステップに分けられます。

初めのステップは `JSON` または自前の content-type を表すのに適した 
[`MediaType`](https://hackage.haskell.org/package/http-media-0.6.2/docs/Network-HTTP-Media.html)
を提供することです。このリンクから haddock を生成するには、適切な関数を使って `application/json` を
指定しなければなりません。この場合には、`(//) :: ByteString -> ByteString -> MediaType` を
使いましょう。正しく `MediaType` を指定するには、`Accept` クラスのインスタンスを書きましょう。

``` haskell
-- for reference:
class Accept ctype where
    contentType   :: Proxy ctype -> MediaType

instance Accept JSON where
    contentType _ = "application" // "json"
```

2番目のステップは `MimeRender` と `MimeUnrender` クラスを中心としたことです。
これらのクラスは値を自前の content-type に変換する方法を示します。

``` haskell
class Accept ctype => MimeRender ctype a where
    mimeRender  :: Proxy ctype -> a -> ByteString
    -- alternatively readable as:
    mimeRender  :: Proxy ctype -> (a -> ByteString)
```

content-type と user type を与えると、`MimeRender`は`a`型の値を遅延`ByteString`型
にエンコードする機能を提供します。

`JSON` の場合は簡単です。任意の `a` 型に対する `ToJSON` インスタンスにおいて、
`Data.Aeson.encode` を使うとその型の値を JSON に変換できます。

``` haskell
instance ToJSON a => MimeRender JSON a where
  mimeRender _ = encode
```


`MimeUnrender` クラスを使うと、遅延`ByteString`から値を抽出できる、もしくは
エラー文字列を出して失敗します。

``` haskell
class Accept ctype => MimeUnrender ctype a where
    mimeUnrender :: Proxy ctype -> ByteString -> Either String a
    -- alternatively:
    mimeUnrender :: Proxy ctype -> (ByteString -> Either String a)
```

`Data.Aeson.eitherDecode` を使えば either についてすることはほとんどありません。
しかし、トップレベル JSON の値として配列かオブジェクトしか許されておらず、"JSONドキュメント"
のトップレベルで任意の型のJSONの値を使うことができる *aeson* や *attoparsec* を使って
ちょっとした関数を書くほうがよく使われています。以下に定義を示します。

> eitherDecodeLenient :: FromJSON a => ByteString -> Either String a
> eitherDecodeLenient input = do
>     v :: Value <- parseOnly (Data.Aeson.Parser.value <* endOfInput) (cs input)
>     parseEither parseJSON v

以下の関数はまさに `MimeUnrender` インスタンスに必要なものです。

``` haskell
instance FromJSON a => MimeUnrender JSON a where
    mimeUnrender _ = eitherDecodeLenient
```

`ReqBody`, `Get`, `Post` そして友達のために `JSON` を使うためのすべてのコードができました。 
どのくらい理解が進んだかを確認するために `HTML` content-type に対応する実装をしてみましょう。
そして、そのウェブサービスのユーザが HTML データにアクセスできて、HTMLドキュメントをインクルード
できる準備ができているかは、例えば [jQuery's `load` function](https://api.jquery.com/load/)
を使ったり、リクエストヘッダに `Accept: text/html` を追加すれば確認できます。

Case-studies: *servant-blaze* and *servant-lucid*
-------------------------------------------------

Haskell から直接 HTML UI を書くには、最近では [blaze-html](http://hackage.haskell.org/package/blaze-html)
や [lucid](http://hackage.haskell.org/package/lucid) が使われています。
*servant* ではどちらもサポートされています。

> data HTMLLucid

改めて書きますが、データ型は関数を変換する記法です。*blaze-html* と *lucid* は HTML から
データを抽出する方法を提供しないので、今回はエンコードについて心配しないことにします。

両方のパッケージともに同じく `HTMLLucid` 型の `Accept` インスタンスを持ちます。

> instance Accept HTMLLucid where
>     contentType _ = "text" // "html" /: ("charset", "utf-8")

注目すべきは、このインスタンスは *http-media* にある `(/:)` 演算子を使っていることです。
この演算子は、上記の charset のような content-type についての追加情報を示します。

両方のパッケージでレンダリングインスタンスは似たような関数を呼び出す。その関数は適切な
インスタンスで型を"抽象的な"HTML表現に変え、`ByteString`で書き出す。

*lucid* の場合:

> instance ToHtml a => MimeRender HTMLLucid a where
>     mimeRender _ = renderBS . toHtml
>
> -- let's also provide an instance for lucid's
> -- 'Html' wrapper.
> instance MimeRender HTMLLucid (Html a) where
>     mimeRender _ = renderBS

*blaze-html* の場合:

> -- For this tutorial to compile 'HTMLLucid' and 'HTMLBlaze' have to be
> -- distinct. Usually you would stick to one html rendering library and then
> -- you can go with one 'HTML' type.
> data HTMLBlaze
>
> instance Accept HTMLBlaze where
>     contentType _ = "text" // "html" /: ("charset", "utf-8")
>
> instance ToMarkup a => MimeRender HTMLBlaze a where
>     mimeRender _ = renderHtml . Text.Blaze.Html.toHtml
>
> -- while we're at it, just like for lucid we can
> -- provide an instance for rendering blaze's 'Html' type
> instance MimeRender HTMLBlaze Text.Blaze.Html.Html where
>     mimeRender _ = renderHtml

適切なクラス(*blaze-html* では `ToMarkup`、 *lucid* では `ToHtml`)のインスタンスを
用意できる限り、[servant-blaze](http://hackage.haskell.org/package/servant-blaze) と
[servant-lucid](http://hackage.haskell.org/package/servant-lucid) の両方ともが
content-type リストで `HTMLLucid` を使えます。

`HTMLLucid` を表示するために *servant-lucid* を使うウェブサービスを書いてみましょう。
まずは import や pragma などを書きます。

次の API を用意します。

> type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]

`Person` は以下のように定義します。

> data Person = Person
>   { firstName :: String
>   , lastName  :: String
>   } deriving Generic -- for the JSON instance
>
> instance ToJSON Person

*lucid* に `Person` をテーブルの列として描画する方法を示しましょう。そして、`Person`
のリストは person ごとの列として扱います。

> -- HTML serialization of a single person
> instance ToHtml Person where
>   toHtml person =
>     tr_ $ do
>       td_ (toHtml $ firstName person)
>       td_ (toHtml $ lastName person)
>
>   -- do not worry too much about this
>   toHtmlRaw = toHtml
>
> -- HTML serialization of a list of persons
> instance ToHtml [Person] where
>   toHtml persons = table_ $ do
>     tr_ $ do
>       th_ "first name"
>       th_ "last name"
>
>     -- this just calls toHtml on each person of the list
>     -- and concatenates the resulting pieces of HTML together
>     foldMap toHtml persons
>
>   toHtmlRaw = toHtml

`Person` の値を作り、その一覧をサーバで扱いましょう。

> persons :: [Person]
> persons =
>   [ Person "Isaac"  "Newton"
>   , Person "Albert" "Einstein"
>   ]
>
> personAPI :: Proxy PersonAPI
> personAPI = Proxy
>
> server4 :: Server PersonAPI
> server4 = return persons
>
> app2 :: Application
> app2 = serve personAPI server4

これで大丈夫です。
この例題は `dist/build/tutorial/tutorial 4` で実行できます。

``` bash
 $ curl http://localhost:8081/persons
 [{"lastName":"Newton","firstName":"Isaac"},{"lastName":"Einstein","firstName":"Albert"}]
 $ curl -H 'Accept: text/html' http://localhost:8081/persons
 <table><tr><td>first name</td><td>last name</td></tr><tr><td>Isaac</td><td>Newton</td></tr><tr><td>Albert</td><td>Einstein</td></tr></table>
 # or just point your browser to http://localhost:8081/persons
```

The `EitherT ServantErr IO` monad
=================================

ハンドラの中心には `EitherT ServantErr IO` モナドがあります。1つ不思議な事は、どうしてこのモナド
なのでしょうか？ 答えは、以下の性質を持つ最も単純なモナドだから、です。

- 成功した場合の結果または失敗した場合のエラーを返します。
- IO を取り扱えます。多くのウェブサービスは `IO` で扱うデータベースのインターフェースとして存在します。

定義を思い出してみましょう。

``` haskell
-- from the Prelude
data Either e a = Left e | Right a

-- from the 'either' package at
-- http://hackage.haskell.org/package/either-4.3.3.2/docs/Control-Monad-Trans-Either.html
newtype EitherT e m a
  = EitherT { runEitherT :: m (Either e a) }
```

要するに、`EitherT ServantErr IO a` 型のハンドラは `IO (Either ServantErr a)` 型を操作したもの
と単に等価であることを意味しています。これはつまりエラーか結果を返す IO アクションです。

前述の `either` パッケージは見る価値があります。以下も大事なことです。

``` haskell
left :: Monad m => e -> EitherT e m a
```

これを使うとハンドラからエラーを返せます。

ハンドラで処理しようとしていることの多くは IO を動かすことで、その結果によってエラーを返したり、
処理を中断したりしたいと思うかもしれません。次の2つの章ではこの方法を示します。

Performing IO
-------------

上述のうちのもう1つの大事なインスタンスは、`MonadIO m => MonadIO (EitherT e m)` です。
[`MonadIO`](http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-IO-Class.html) 
は、*transformers* パッケージにあるクラスで、以下のように定義されています。

``` haskell
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
```

明らかに、IO モナドは `MonadIO` インスタンスを提供しています。
したがって、任意の `e` 型において、`EitherT e IO` は `MonadIO` を持っています。
ハンドラ内で IO 操作する任意の kind を実行したければ、`liftIO` を使いましょう。

> type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent
>
> newtype FileContent = FileContent
>   { content :: String }
>   deriving Generic
>
> instance ToJSON FileContent
>
> server5 :: Server IOAPI1
> server5 = do
>   filecontent <- liftIO (readFile "myfile.txt")
>   return (FileContent filecontent)

Failing, through `ServantErr`
-----------------------------

もし適切なHTTPステータスコードとエラーメッセージを使ってエンドポイントで決められた結果を
明示的に失敗させたいのであれば、上述の `left` 関数を使うか、`ServantErr` 型の適切な値を
生成しましょう。ServantErr は以下のように定義されています。

``` haskell
data ServantErr = ServantErr
    { errHTTPCode     :: Int
    , errReasonPhrase :: String
    , errBody         :: ByteString -- lazy bytestring
    , errHeaders      :: [Header]
    }
```

多くの標準的な値は、`Servant.Server` モジュールで提供されています。もしこれらの値を使うけど
一部を変更したい場合には、以下の record update syntax を使いましょう。

> failingHandler :: EitherT ServantErr IO ()
> failingHandler = left myerr
>
>   where myerr :: ServantErr
>         myerr = err503 { errBody = "Sorry dear user." }

以下の例は、"myfile.txt" が存在しなかった場合にレスポンスボディに含まれるカスタマイズされた 
404-Not-Found エラーメッセージです。

> server6 :: Server IOAPI1
> server6 = do
>   exists <- liftIO (doesFileExist "myfile.txt")
>   if exists
>     then liftIO (readFile "myfile.txt") >>= return . FileContent
>     else left custom404Err
>
>   where custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }

`dist/build/tutorial/tutorial 5` でこのサーバを動かせます。
初めはファイルが存在しない状態でクエリを送ってみましょう。

``` bash
 $ curl --verbose http://localhost:8081/myfile.txt
 [snip]
 * Connected to localhost (127.0.0.1) port 8081 (#0)
 > GET /myfile.txt HTTP/1.1
 > User-Agent: curl/7.30.0
 > Host: localhost:8081
 > Accept: */*
 >
 < HTTP/1.1 404 Not Found
 [snip]
 myfile.txt just isnt there, please leave this server alone.

 $ echo Hello > myfile.txt

 $ curl --verbose http://localhost:8081/myfile.txt
 [snip]
 * Connected to localhost (127.0.0.1) port 8081 (#0)
 > GET /myfile.txt HTTP/1.1
 > User-Agent: curl/7.30.0
 > Host: localhost:8081
 > Accept: */*
 >
 < HTTP/1.1 200 OK
 [snip]
 < Content-Type: application/json
 [snip]
 {"content":"Hello\n"}
```

Response headers
================

レスポンスにヘッダを加えるためには [addHeader](http://hackage.haskell.org/package/servant-0.4.4/docs/Servant-API-ResponseHeaders.html)
を使います。以下の例のように API の型が変わることに注意しましょう。

> type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)
>
> myHandler :: Server MyHandler
> myHandler = return $ addHeader 1797 albert

Serving static files
====================

*servant-server* は Web API 内でのあるパス以下のディレクトリのコンテンツをサーブする方法も提供します。
`Raw` 結合子は任意の WAI アプリケーションをつなぐ API で使われます。servant-server は WAI アプリを
サーブするのにファイルやディレクトリを取得する機能を提供すます。

``` haskell
-- exported by Servant and Servant.Server
serveDirectory :: FilePath -> Server Raw
```

`serveDirectory` の引数は有効なディレクトリへのパスでなければなりません。
以下に例を示します。これは `dist/build/tutorial/tutorial 6` でこのサーバを動かせます。
(必ず *servant-examples/* ディレクトリで動かさなければなりません)
このチュートリアルで網羅されている大量のコードを動かすウェブサーバです。

API の定義は次のようになります。

> type CodeAPI = "code" :> Raw

サーバ

> codeAPI :: Proxy CodeAPI
> codeAPI = Proxy

> server7 :: Server CodeAPI
> server7 = serveDirectory "tutorial"
>
> app3 :: Application
> app3 = serve codeAPI server7

このサーバは `/code` でパスが始まる任意のリクエストにマッチします。
そしてこのプログラムを動かしているパスの *tutorial/* ディレクトリの中で、
残りのリクエストパスで指定されるパスでファイルを探します。

これは以下のようにも言えます。

- もしクライアントが `/code/foo.txt` をリクエストした場合、サーバは `./tutorial/foo.txt` を探します。(これは失敗します。)
- もしクライアントが `/code/T1.hs` をリクエストした場合、サーバは `./tutorial/T1.hs` を探します。(これは成功します。)
- もしクライアントが `/code/foo/bar/baz/movie.mp4` をリクエストした場合、サーバは `./tutorial/foo/bar/baz/movie.mp4` を探します。(これは失敗します。)

これが実際に動くサーバです。

``` haskell
$ curl http://localhost:8081/code/T1.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module T1 where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Servant

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

-- orphan ToJSON instance for Day. necessary to derive one for User
instance ToJSON Day where
  -- display a day in YYYY-mm-dd format
  toJSON d = toJSON (showGregorian d)

instance ToJSON User

type UserAPI = "users" :> Get '[JSON] [User]

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = return users

app :: Application
app = serve userAPI server
$ curl http://localhost:8081/code/tutorial.hs
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment

import qualified T1
import qualified T2
import qualified T3
import qualified T4
import qualified T5
import qualified T6
import qualified T7
import qualified T9
import qualified T10

app :: String -> (Application -> IO ()) -> IO ()
app n f = case n of
  "1" -> f T1.app
  "2" -> f T2.app
  "3" -> f T3.app
  "4" -> f T4.app
  "5" -> f T5.app
  "6" -> f T6.app
  "7" -> f T7.app
  "8" -> f T3.app
  "9" -> T9.writeJSFiles >> f T9.app
  "10" -> f T10.app
  _   -> usage

main :: IO ()
main = do
  args <- getArgs
  case args of
    [n] -> app n (run 8081)
    _   -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:\t tutorial N"
  putStrLn "\t\twhere N is the number of the example you want to run."

$ curl http://localhost:8081/foo
not found
```

Nested APIs
===========

重複を避けて、組み立て式の方法でどうやってAPIを定義することができるか見てみましょう。
シンプルな例を考えます。

> type UserAPI3 = -- view the user with given userid, in JSON
>                 Capture "userid" Int :> Get '[JSON] User
>
>            :<|> -- delete the user with given userid. empty response
>                 Capture "userid" Int :> Delete '[] ()

むしろ `userid` をくくりだすことができます。

> type UserAPI4 = Capture "userid" Int :>
>   (    Get '[JSON] User
>   :<|> Delete '[] ()
>   )

しかし、対応する `Server` の型に影響することには注意しなければなりません。

``` haskell
Server UserAPI3 = (Int -> EitherT ServantErr IO User)
             :<|> (Int -> EitherT ServantErr IO ())

Server UserAPI4 = Int -> (    EitherT ServantErr IO User
                         :<|> EitherT ServantErr IO ()
                         )
```

前者の場合には、ハンドラごとに *userid* 引数を取ります。
後者の場合には、`Server` 全体が *userid* を取り、引数無しで `EitherT` を処理するハンドラを持ちます。
以下のようにも書けます。

> server8 :: Server UserAPI3
> server8 = getUser :<|> deleteUser
>
>   where getUser :: Int -> EitherT ServantErr IO User
>         getUser _userid = error "..."
>
>         deleteUser :: Int -> EitherT ServantErr IO ()
>         deleteUser _userid = error "..."
>
> -- notice how getUser and deleteUser
> -- have a different type! no argument anymore,
> -- the argument directly goes to the whole Server
> server9 :: Server UserAPI4
> server9 userid = getUser userid :<|> deleteUser userid
>
>   where getUser :: Int -> EitherT ServantErr IO User
>         getUser = error "..."
>
>         deleteUser :: Int -> EitherT ServantErr IO ()
>         deleteUser = error "..."

くくりだせる `Capture` は何も特別でなく、任意の結合子に対して可能であることに注目しましょう。
くくりだせる結合子を使ったいくつかの API の例を示します。
これを使えば完全に有効に機能するサーバを書くことができます。

> -- we just factor out the "users" path fragment
> type API1 = "users" :>
>   (    Get '[JSON] [User] -- user listing
>   :<|> Capture "userid" Int :> Get '[JSON] User -- view a particular user
>   )
>
> -- we factor out the Request Body
> type API2 = ReqBody '[JSON] User :>
>   (    Get '[JSON] User -- just display the same user back, don't register it
>   :<|> Post '[JSON] ()  -- register the user. empty response
>   )
>
> -- we factor out a Header
> type API3 = Header "Authorization" Token :>
>   (    Get '[JSON] SecretData -- get some secret data, if authorized
>   :<|> ReqBody '[JSON] SecretData :> Post '[] () -- add some secret data, if authorized
>   )
>
> newtype Token = Token ByteString
> newtype SecretData = SecretData ByteString

このアプローチでモジュール構造のAPIを定義できます。
最終的に巨大な API type として組み立てることができます。

> type UsersAPI =
>        Get '[JSON] [User] -- list users
>   :<|> ReqBody '[JSON] User :> Post '[] () -- add a user
>   :<|> Capture "userid" Int :>
>          ( Get '[JSON] User -- view a user
>       :<|> ReqBody '[JSON] User :> Put '[] () -- update a user
>       :<|> Delete '[] () -- delete a user
>          )
>
> usersServer :: Server UsersAPI
> usersServer = getUsers :<|> newUser :<|> userOperations
>
>   where getUsers :: EitherT ServantErr IO [User]
>         getUsers = error "..."
>
>         newUser :: User -> EitherT ServantErr IO ()
>         newUser = error "..."
>
>         userOperations userid =
>           viewUser userid :<|> updateUser userid :<|> deleteUser userid
>
>           where
>             viewUser :: Int -> EitherT ServantErr IO User
>             viewUser = error "..."
>
>             updateUser :: Int -> User -> EitherT ServantErr IO ()
>             updateUser = error "..."
>
>             deleteUser :: Int -> EitherT ServantErr IO ()
>             deleteUser = error "..."

> type ProductsAPI =
>        Get '[JSON] [Product] -- list products
>   :<|> ReqBody '[JSON] Product :> Post '[] () -- add a product
>   :<|> Capture "productid" Int :>
>          ( Get '[JSON] Product -- view a product
>       :<|> ReqBody '[JSON] Product :> Put '[] () -- update a product
>       :<|> Delete '[] () -- delete a product
>          )
>
> data Product = Product { productId :: Int }
>
> productsServer :: Server ProductsAPI
> productsServer = getProducts :<|> newProduct :<|> productOperations
>
>   where getProducts :: EitherT ServantErr IO [Product]
>         getProducts = error "..."
>
>         newProduct :: Product -> EitherT ServantErr IO ()
>         newProduct = error "..."
>
>         productOperations productid =
>           viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid
>
>           where
>             viewProduct :: Int -> EitherT ServantErr IO Product
>             viewProduct = error "..."
>
>             updateProduct :: Int -> Product -> EitherT ServantErr IO ()
>             updateProduct = error "..."
>
>             deleteProduct :: Int -> EitherT ServantErr IO ()
>             deleteProduct = error "..."

> type CombinedAPI = "users" :> UsersAPI
>               :<|> "products" :> ProductsAPI
>
> server10 :: Server CombinedAPI
> server10 = usersServer :<|> productsServer

最後に、user と product API がほとんど同じなので、1つにまとめることができます。

> -- API for values of type 'a'
> -- indexed by values of type 'i'
> type APIFor a i =
>        Get '[JSON] [a] -- list 'a's
>   :<|> ReqBody '[JSON] a :> Post '[] () -- add an 'a'
>   :<|> Capture "id" i :>
>          ( Get '[JSON] a -- view an 'a' given its "identifier" of type 'i'
>       :<|> ReqBody '[JSON] a :> Put '[] () -- update an 'a'
>       :<|> Delete '[] () -- delete an 'a'
>          )
>
> -- Build the appropriate 'Server'
> -- given the handlers of the right type.
> serverFor :: EitherT ServantErr IO [a] -- handler for listing of 'a's
>           -> (a -> EitherT ServantErr IO ()) -- handler for adding an 'a'
>           -> (i -> EitherT ServantErr IO a) -- handler for viewing an 'a' given its identifier of type 'i'
>           -> (i -> a -> EitherT ServantErr IO ()) -- updating an 'a' with given id
>           -> (i -> EitherT ServantErr IO ()) -- deleting an 'a' given its id
>           -> Server (APIFor a i)
> serverFor = error "..."
> -- implementation left as an exercise. contact us on IRC
> -- or the mailing list if you get stuck!

Using another monad for your handlers
=====================================

`Server` が HTTP メソッドの結合子をどのように `EitherT ServantErr IO` に変えたのかを思い出してみましょう。
`Server` は実際には単純な type synonym に過ぎません。

``` haskell
type Server api = ServerT api (EitherT ServantErr IO)
```

`ServerT` は `HasServer` クラスの一部のハンドラが必要な型を算出する type family です。これは3番目のパラメータ
を取ることを除けば `Server` のようなものです。このパラメータはハンドラを差し込みたいモナドもしくはハンドラが返す型
です。3番目のパラメータはエンドポイントのハンドラが返す型を特定するのに使います。
`ServerT (Get '[JSON] Person) SomeMonad` を処理すると、`SomeMonad Person` を返します。

最初の、そして主な疑問はどうしてこれが必要なのでしょうか。他のモナドに差し込むハンドラをどのように書けばいいのでしょうか。

Natural transformations
-----------------------

もし任意の `a` において `m a` から `n a` へ変換できる関数があったとすると、その関数はどういうもの
になるのでしょうか。

``` haskell
newtype m :~> n = Nat { unNat :: forall a. m a -> n a}

-- For example
-- listToMaybeNat ::`[] :~> Maybe`
-- listToMaybeNat = Nat listToMaybe  -- from Data.Maybe
```
(`Nat` は "natural transformation" のことで、今回取り扱うものです。)

もし `EitherT ServantErr IO` とは別のモナド(`Reader String` モナド)または型を使ってハンドラを
書きたいのであれば、最初に用意すべきなのは以下の関数です。

``` haskell
readerToEither :: Reader String :~> EitherT ServantErr IO
```

`readerToEither'` から始めましょう。例えば "hi" のような `String` をそれに供給する処理を
`Reader` にさせなければならないことは明らかです。`a` を取り出して `EitherT` の中で `return`
しましょう。変わった型を持たせるために `Nat` コンストラクタを使った関数でラップします。

> readerToEither' :: forall a. Reader String a -> EitherT ServantErr IO a
> readerToEither' r = return (runReader r "hi")
>
> readerToEither :: Reader String :~> EitherT ServantErr IO
> readerToEither = Nat readerToEither'

`Reader String` を差し込みハンドラを使って単純なウェブサービスを書いてみましょう。

> type ReaderAPI = "a" :> Get '[JSON] Int
>             :<|> "b" :> Get '[JSON] String
>
> readerAPI :: Proxy ReaderAPI
> readerAPI = Proxy
>
> readerServerT :: ServerT ReaderAPI (Reader String)
> readerServerT = a :<|> b
>
>   where a :: Reader String Int
>         a = return 1797
>
>         b :: Reader String String
>         b = ask

運の悪いことに `serve` の引数として `readerServerT` を使うことはできません。`serve` の
引数として使えるのは `Server ReaderAPI` です。これは `EitherT ServantErr IO` を差し込む
ハンドラで使います。それでも上記は単純な解決方法の1つです。

Enter `enter`
-------------

上で書いてきた `readerToEither` ですが、`serve` の正しい型を持つハンドラを作るために
すべてのハンドラの戻り値にこれを正確に適用する必要があります。これを手動でやるのは面倒なので、
`enter` 関数が用意されています。これは `m` と `n` という2つのパラメータの型と `ServerT someapi m`
の間の natural transformation を行い、`ServerT someapi n` を返します。

具体的には、ハンドラ上で `enter readerToEither` を使ってウェブサービスをラップします。

> readerServer :: Server ReaderAPI
> readerServer = enter readerToEither readerServerT
>
> app4 :: Application
> app4 = serve readerAPI readerServer

`dist/build/tutorial/tutorial 7` でこのウェブサービスを実行できます。

``` bash
$ curl http://localhost:8081/a
1797
$ curl http://localhost:8081/b
"hi"
```

Conclusion
==========

*servant* を使ってウェブサービス・ウェブアプリケーションを実装できるようになりました。
ここでは唯一、自分の結合子を組み込むのを取り扱いませんでしたが、このウェブサイトの別ページで扱っています。
残りの文章では *servant-client* と *servant-jquery*、 *servant-docs* について取り扱います。

<div style="text-align: center;">
  <p><a href="/tutorial/api-type.html">Previous page: A web API as a type</a></p>
  <p><a href="/tutorial/client.html">Next page: Deriving Haskell functions to query an API</a></p>
</div>
