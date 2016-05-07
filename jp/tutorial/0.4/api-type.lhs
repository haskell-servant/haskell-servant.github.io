---
title: A web API as a type
toc: true
---

この章のソースは literate haskell file です。
まずいくつかの言語拡張と import を導入します。

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeOperators #-}
>
> module ApiType where
>
> import Data.Text
> import Servant.API

Consider the following informal specification of an API:

以下のように大雑把にAPIの仕様を考えてみましょう。

 > `/users` というエンドポイントは `age` や `name` などの値を持つ `sortby`
 > クエリ文字列を受け取り、`age`, `name`, `email`, `registration_date` といった
 > ユーザ情報を持つJSONオブジェクトの一覧を返します。

これを形式化してみましょう。形式化されたAPIからウェブアプリを書くための多くの手段を得られます。
他にもクライアントライブラリやドキュメントを書く手段にもなります。

それでは sevant を使ってどのようにAPIを記述すれば良いのでしょうか？
前述のとおりエンドポイントを書くには古き良き Haskell の **型** を使います。

> type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
>
> data SortBy = Age | Name
>
> data User = User {
>   name :: String,
>   age :: Int
> }

上記を掘り下げてみましょう:

- `"users"` は `/users` でアクセスできるエンドポイントを表しています。
- `QueryParam "sortby" SortBy` は `sortby` クエリ文字列パラメータを持つ
エンドポイントであり、`SortBy` 型の値を持つことが期待されます。
`SortBy` は `data SortBy = Age | Name` のように定義されます。
- `Get '[JSON] [User]` は HTTP GET リクエストを通じてアクセスできる、
JSONとしてUserのリストを返すようなエンドポイントであることを示しています。
異なるフォーマットでデータを使えるようにする方法は後ほど登場します。それは
クライアントのリクエスト内でどの [Accept header](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html)
を選ぶかで決まります。
- `:>` 演算子は様々な「結合子」を分離します。static path や URL capture など。
static path や URL capture の場合だけ、その順序に意味があります。
`"users" :> "list-all" :> Get '[JSON] [User]` は `/users/list-all` と同じで、
`"list-all" :> "users" :> Get '[JSON] [User]` とは異なります。
`:>` は `/` と等価な場合もありますが、必ずしもそうではないこともあります。

複数のエンドポイントを持つAPIを `:<|>` 結合子を使って記述できます。
以下に一例を示します：

> type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
>            :<|> "list-all" :> "users" :> Get '[JSON] [User]

*servant* は多数の（out-of-the-box?）結合子を取り扱えますが、必要なだけ自分で書かなければ
なりません。servant で扱えるすべての結合子の概要を以下にまとめました。

Combinators
===========

Static strings
--------------

これまでに見てきた通り、static path を記述するのに型レベル文字列を使用できます。
(ただし、`DataKinds` 言語拡張を導入する必要があります。)
URL を書くには文字列を `/` で句切れば良いのです。

> type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]
>               -- これでアクセスできるエンドポイントは以下のようになります:
>               -- /users/list-all/now

`Delete`, `Get`, `Patch`, `Post` and `Put`
------------------------------------------

これら5つの結合子は非常に似ていますが、HTTPメソッドが異なります。
以下のように定義されています。

``` haskell
data Delete (contentTypes :: [*]) a
data Get (contentTypes :: [*]) a
data Patch (contentTypes :: [*]) a
data Post (contentTypes :: [*]) a
data Put (contentTypes :: [*]) a
```

エンドポイントは(自作しないかぎり)上記の5つの結合子のうちの1つで終わります。
例: 

> type UserAPI4 = "users" :> Get '[JSON] [User]
>            :<|> "admins" :> Get '[JSON] [User]

`Capture`
---------

URLの一部であるURLキャプチャは変数で、その実際の値は取得されてからリクエストハンドラに渡されます。
多くのウェブフレームワークでは `/users/:userid` のように書かれ、`:` のついた `userid` が変数名
またはプレースホルダです。例えば、もし `userid` が1以上の整数の範囲に収まるの場合には、そのエンド
ポイントは `/users/1` とか `/users/143` とかになります。

Servant における `Capture` 結合子は変数名と型で表される(型レベル)文字列で、取得したい値の
型を示しています。

``` haskell
data Capture (s :: Symbol) a
-- s :: シンボル 's' は型レベル文字列
```

キャプチャに正規表現を使っているウェブフレームワークもあります。
Servant は [`FromText`](https://hackage.haskell.org/package/servant/docs/Servant-Common-Text.html#t:FromText)
クラスを使っていて、取得された値はそのインスタンスになっていなければなりません。

例:

> type UserAPI5 = "user" :> Capture "userid" Integer :> Get '[JSON] User
>                 -- 'GET /user/:userid' と等価
>                 -- ただし servant では "userid" が Integer であることを明示している
>
>            :<|> "user" :> Capture "userid" Integer :> Delete '[] ()
>                 -- 'DELETE /user/:userid' と等価

`QueryParam`, `QueryParams`, `QueryFlag`, `MatrixParam`, `MatrixParams` and `MatrixFlag`
----------------------------------------------------------------------------------------

`QueryParam` や `QueryParams`, `QueryFlag` はクエリ文字列パラメータです。
これらのパラメータは URL 内の (`?`) の後に置かれます。
例えば、`/users?sortby=age` の `sortby` のようなもので `age` という値を取ります。
`QueryParams` はクエリパラメータが値のリストであることを表していて、
`?param[]=value1&param[]=value2` のように書かれます。 
`QueryFlag` は論理値のようなクエリパラメータに用いられ、値を持つ必要はありません。
クエリ文字列の中でパラメータ名があるかないかで、パラメータの値が `True` か `False`
であるかを決定します。例えば、`/users?active` はすべての `/users` のうち、
アクティブなユーザの一覧のみを表します。

以下に対応するデータ型の定義を示します:

``` haskell
data QueryParam (sym :: Symbol) a
data QueryParams (sym :: Symbol) a
data QueryFlag (sym :: Symbol)
```

[Matrix parameters](http://www.w3.org/DesignIssues/MatrixURIs.html)
はクエリ文字列パラメータに近いものですが、パスの至るところに出てきます。
matrixパラメータのURLは `/users;sortby=age` のようになり、
クエリ文字列パラメータでは `/users?sortby=age` のように書けます。
このパラメータのいいところは、URLの最後に持ってくる必要がないことです。
2015/01/01 以降に登録したアクティブユーザの位置データを取得するには、
`/users;active=true;registered_after=2005-01-01/locations` 
のように書きます。

対応するデータ型定義は以下のとおりです:

``` haskell
data MatrixParam (sym :: Symbol) a
data MatrixParams (sym :: Symbol) a
data MatrixFlag (sym :: Symbol)
```

例:

> type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
>                 -- 'GET /users?sortby={age, name}' と同じ
>
>            :<|> "users" :> MatrixParam "sortby" SortBy :> Get '[JSON] [User]
>                 -- 'GET /users;sortby={age, name}' と同じ

繰り返しになりますが、ハンドラが `SortBy` のようなものを処理する必要はありません。
*Servant* が面倒を見てくれます。

`ReqBody`
---------

個々のHTTPリクエストは使用可能なサーバについてのデータを *body* に持つことができ、
そのデータは(サーバが理解する長さで)任意のフォーマットにエンコードできます。
それは新しいユーザを作成するのに使うエンドポイントなどで使われます。
分離したクエリ文字列パラメータみたいな汚い書式でユーザの個々のフィールドを得る
代わりに、すべてのデータをJSONオブジェクトに入れてしまうことができます。
この利点はネストされたオブジェクトを扱えることです。

*Servant* の `ReqBody` 結合子は content-type のリストを持ち、エンコードされた
そのデータとデータ型はリクエストボディに書かれます。気づいたかもしれませんが、
content-type ヘッダーをチェックする必要はなく、自前で処理する必要もありません。
servant が面倒をみます。そして必要に応じて `Bad Request` や `Unsupported Content Type`
を返します。

以下はデータ型の定義です:

``` haskell
data ReqBody (contentTypes :: [*]) a
```

例:

> type UserAPI7 = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
>                 -- - equivalent to 'POST /users' with a JSON object
>                 --   describing a User in the request body
>                 -- - returns a User encoded in JSON
>
>            :<|> "users" :> Capture "userid" Integer
>                         :> ReqBody '[JSON] User
>                         :> Put '[JSON] User
>                 -- - equivalent to 'PUT /users/:userid' with a JSON
>                 --   object describing a User in the request body
>                 -- - returns a User encoded in JSON

Request `Header`s
-----------------

リクエストヘッダはキャッシュから認証データの受け渡しまで様々な用途に使われます。リクエスト
ヘッダはヘッダ名とその値を持ちます。例えば、`Accept: application/json` のように書き
ます。

Servant の `Header` 結合子はヘッダ名とその型を型レベル文字列で表します。型は(何らかの
文字表現から変換される)ヘッダ値の型で、以下のように書けます。

``` haskell
data Header (sym :: Symbol) a
```

以下の例は `User-Agent` を使ったエンドポイントを表しています。リクエストを送信する
クライアントが使うソフトウェアまたはライブラリの名前を示します。

> type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]

Content types
-------------

これまでは content-type のリストを扱う結合子として常に `'[JSON]` を使ってきました。
しかし *servant* は他にも content-type を使えますし、自前の content-type を定義する
こともできます。

*Servant* パッケージはすぐに使える4つの content-type を用意しています。
`JSON`, `PlainText`, `FormUrlEncoded`, `OctetStream` です。
もし良く分からない理由で4つのフォーマットを使ってユーザデータを扱えるエンドポイントが必要に
なった場合には、以下のように API を書きます。

> type UserAPI9 = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]

HTML の content-type も提供されていますが、誰もが使う1つのライブラリとしては提供して
いません。データを HTML にエンコードするために *servant-lucid* と *servant-blaze* 
という2つのパッケージが用意されています。

content-type と独自のデータ型を一緒に扱う方法は [section about serving an API](/tutorial/server.html)
で紹介します。

Response `Headers`
------------------

HTTPリクエストと同じく、ウェブサーバに作られたレスポンスもヘッダを載せることができます。
*servant* は `Header` のリストを渡す `Headers` 結合子を持ち、"return type" を
単純にラップするエンドポイントによって使われます

``` haskell
data Headers (ls :: [*]) a
```

レスポンスが "User-Count" ヘッダを返すようなエンドポイントは、以下のように書けます。

> type UserAPI10 = "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])

Interoperability with other WAI `Application`s: `Raw`
-----------------------------------------------------

最後に紹介するのは `Raw` 結合子で、以下の2つの理由で使用されます。

- 静的なファイルを使いたい場合には、以下のように書きます。

> type UserAPI11 = "users" :> Get '[JSON] [User]
>                  -- /users エンドポイント
>
>             :<|> Raw
>                  -- /users 以外のリクエストはここにきます。
>                  -- 正しいパス名とファイル名を指定すれば表示されます。

- You more generally want to plug a [WAI `Application`](http://hackage.haskell.org/package/wai)
into your webservice. Static file serving is a specific example of that. The API type would look the
same as above though. (You can even combine *servant* with other web frameworks
this way!)

- より一般的な使い方としてはウェブサービスに [WAI `Application`](http://hackage.haskell.org/package/wai)
を取り入れる方法があります。静的なファイルを表示する方法が書いてあります。APIの場合と同じように見えます。
(この方法で *servant* と他のウェブフレームワークを組み合わせることもできます。)

<div style="text-align: center;">
  <a href="/tutorial/server.html">Next page: Serving an API</a>
</div>
