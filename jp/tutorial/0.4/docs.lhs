---
title: Generating documentation from API types
toc: true
---

この章のソースは literate haskell file として書かれています。
まずは言語拡張と import が必要です。

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -fno-warn-orphans #-}
>
> module Docs where
>
> import Data.ByteString.Lazy (ByteString)
> import Data.Proxy
> import Data.Text.Lazy.Encoding (encodeUtf8)
> import Data.Text.Lazy (pack)
> import Network.HTTP.Types
> import Network.Wai
> import Servant.API
> import Servant.Docs
> import Servant.Server

[Serving an API](/tutorial/server.html) モジュールの1つからいくつかを import します。

> import Server (Email(..), ClientInfo(..), Position(..), HelloMessage(..),
>   server3, emailForClient)

クライアントの関数生成のように、ドキュメント生成とは API type の推論と、APIのユーザへのフォーマットとして
示す必要がある全データの抽出を意味しています。

しかし今回は *servant* を補助しなければなりません。APIについてたくさんのことを推定できるので、
ユーザフレンドリーなAPIの様々な点を記述するのに偶然気づいたり、"ビジネスロジックレベル"で何であるのか
説明できません。
ドキュメント生成のための勉強となるいい例が `/position`, `/hello`, `/marketing` エンドポイントを
使ったウェブサービスです。

> type ExampleAPI = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
>       :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
>       :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
>
> exampleAPI :: Proxy ExampleAPI
> exampleAPI = Proxy

*servant* の例では3つのエンドポイントとそのレスポンスボディがJSONで出力されるので、ウェブサービス上の
キャプチャ、パラメータリクエストボディやその他の結合子がどのような影響を与えるかを知ることはできません。
これには手動での操作が必要になります。

キャプチャ、リクエストボディ、レスポンスボディ、クエリパラメータによって、レスポンスにどのような影響を
あたえ、どんな値を取りうるかの説明をしなければなりません。上述の例は以下のようになります。

> instance ToCapture (Capture "x" Int) where
>   toCapture _ =
>     DocCapture "x"                                -- name
>                "(integer) position on the x axis" -- description
>
> instance ToCapture (Capture "y" Int) where
>   toCapture _ =
>     DocCapture "y"                                -- name
>                "(integer) position on the y axis" -- description
>
> instance ToSample Position Position where
>   toSample _ = Just (Position 3 14) -- example of output
>
> instance ToParam (QueryParam "name" String) where
>   toParam _ =
>     DocQueryParam "name"                     -- name
>                   ["Alp", "John Doe", "..."] -- example of values (not necessarily exhaustive)
>                   "Name of the person to say hello to." -- description
>                   Normal -- Normal, List or Flag
>
> instance ToSample HelloMessage HelloMessage where
>   toSamples _ =
>     [ ("When a value is provided for 'name'", HelloMessage "Hello, Alp")
>     , ("When 'name' is not specified", HelloMessage "Hello, anonymous coward")
>     ]
>     -- mutliple examples to display this time
>
> ci :: ClientInfo
> ci = ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]
>
> instance ToSample ClientInfo ClientInfo where
>   toSample _ = Just ci
>
> instance ToSample Email Email where
>   toSample _ = Just (emailForClient ci)

リクエストまたはレスポンス本体として使われる型は、1つ以上の値の例を示す `ToSample` 型クラスを
インスタンス化しなければなりません。`Capture` と `QueryParam` はそれぞれ `ToCapture` と 
`ToParam` クラスをインスタンス化しなければなりません。そして名前と上記のコードで書かれているような
引数の具体的な意味についての情報を与えなければなりません。

APIによってdocsを導出できます。

> apiDocs :: API
> apiDocs = docs exampleAPI

`API` は *servant-docs* によって導出される型です。servant-docs は同じフォーマットでドキュメントを
生成するために web API について必要な情報をすべて保持しています。
*servant-docs* は [Markdown](http://en.wikipedia.org/wiki/Markdown) 出力するドキュメント生成器
しか持ちませんが、[servant-pandoc](http://hackage.haskell.org/package/servant-pandoc) パッケージ
にはたくさんの便利なフォーマットを使う機能があります。

*servant* のマークダウン pretty printer は `markdown` という名前の関数です。

``` haskell
markdown :: API -> String
```

`maekdown apiDocs` を見ると、どのように markdown で書かれた API ドキュメントが生成されるかが分かります。

``` text
 ## Welcome

 This is our super webservice's API.

 Enjoy!

 ## GET /hello

 #### GET Parameters:

 - name
      - **Values**: *Alp, John Doe, ...*
      - **Description**: Name of the person to say hello to.


 #### Response:

 - Status code 200
 - Headers: []

 - Supported content types are:

     - `application/json`

 - When a value is provided for 'name'

   ```javascript
   {"msg":"Hello, Alp"}
   ```

 - When 'name' is not specified

   ```javascript
   {"msg":"Hello, anonymous coward"}
   ```

 ## POST /marketing

 #### Request:

 - Supported content types are:

     - `application/json`

 - Example: `application/json`

   ```javascript
   {"email":"alp@foo.com","interested_in":["haskell","mathematics"],"age":26,"name":"Alp"}
   ```

 #### Response:

 - Status code 201
 - Headers: []

 - Supported content types are:

     - `application/json`

 - Response body as below.

   ```javascript
   {"subject":"Hey Alp, we miss you!","body":"Hi Alp,\n\nSince you've recently turned 26, have you checked out our latest haskell, mathematics products? Give us a visit!","to":"alp@foo.com","from":"great@company.com"}
   ```

 ## GET /position/:x/:y

 #### Captures:

 - *x*: (integer) position on the x axis
 - *y*: (integer) position on the y axis

 #### Response:

 - Status code 200
 - Headers: []

 - Supported content types are:

     - `application/json`

 - Response body as below.

   ```javascript
   {"x":3,"y":14}
   ```

```

さらに、ドキュメントについて紹介する章を追加します。`apiDocs` を生成する方法を微調整するだけです。
`wai` が求めてるのは `Raw` エンドポイントなので、文章内容を遅延`ByteString`型に変えます

> docsBS :: ByteString
> docsBS = encodeUtf8
>        . pack
>        . markdown
>        $ docsWithIntros [intro] exampleAPI
>
>   where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

`docsWithIntros` は `DocIntro` のリストという追加のパラメータを取ります。
これはどのエンドポイントのドキュメントよりも先に表示されなければなりません。

これで API と API ドキュメントが単純なサーバで動かすことができるようになりました。

> type DocsAPI = ExampleAPI :<|> Raw
>
> api :: Proxy DocsAPI
> api = Proxy
>
> server :: Server DocsAPI
> server = Server.server3 :<|> serveDocs
>
>   where serveDocs _ respond =
>           respond $ responseLBS ok200 [plain] docsBS
>
>         plain = ("Content-Type", "text/plain")
>
> app :: Application
> app = serve api server

このサーバは `dist/build/tutorial/tutorial 10` で動かすことができます。
`/position`, `/hello`, `/marketing` 以外で API ドキュメントを見ることが出来ます。
`serverDocs` は3つ以外のエンドポイントがマッチしなくてシステム的に成功するかどうかを試されるからです。
成功の定義は `text/plain` コンテントタイプと決められた bytestring を返すことです。

<div style="text-align: center;">
  <a href="/tutorial/javascript.html">Previous page: Generating javascript functions to query an API</a>
</div>
