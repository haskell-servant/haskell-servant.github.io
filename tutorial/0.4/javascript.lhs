---
title: Deriving Javascript functions to query an API
toc: true
---
 
*servant* が API type を javascript の関数に変える方法を見てみましょう。その関数は
ウェブサービスにクエリを送ることができます。導出されたコードは *jQuery* を使わせると仮定します
が、純粋なjavascriptや他の*jQuery*ライブラリに基づくajaxリクエストを生成するコードを大変
簡単に適用することができます。

このために2つに分割されたシンプルなシンプルなページを考えます。一番上には検索ボックスがあり、
著者や題名でHaskell本の一覧、サイトに入った時間やキャラクタを削除する時間で更新
された結果の一覧から検索できます。一番下にはランダムな点をえるためのウェブサービスを使って
古典的な[probabilistic method to approximate pi](http://en.wikipedia.org/wiki/Approximations_of_%CF%80#Summing_a_circle.27s_area)
を見ることができます。最終的には2つのJavascriptファイルとともにHTMLファイルをサーブます。1つは
API typeから自動的に生成され、もう1つはAPIにクエリを投げる関数ですぐに使える状態で提供されます。

この章のソースは literate haskell file として書かれています。
まずは言語拡張と import から始めましょう。

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TypeOperators #-}
>
> module Javascript where
>
> import Control.Monad.IO.Class
> import Data.Aeson
> import Data.Proxy
> import Data.Text (Text)
> import qualified Data.Text as T
> import GHC.Generics
> import Language.Javascript.JQuery
> import Network.Wai
> import Servant
> import Servant.JQuery
> import System.Random

以下は API type と付随のデータタイプです。

> type API = "point" :> Get '[JSON] Point
>       :<|> "books" :> QueryParam "q" Text :> Get '[JSON] (Search Book)
>
> type API' = API :<|> Raw
>
> data Point = Point
>   { x :: Double
>   , y :: Double
>   } deriving Generic
>
> instance ToJSON Point
>
> data Search a = Search
>   { query   :: Text
>   , results :: [a]
>   } deriving Generic
>
> mkSearch :: Text -> [a] -> Search a
> mkSearch = Search
>
> instance ToJSON a => ToJSON (Search a)
>
> data Book = Book
>   { author :: Text
>   , title  :: Text
>   , year   :: Int
>   } deriving Generic
>
> instance ToJSON Book
>
> book :: Text -> Text -> Int -> Book
> book = Book

"書籍データベース" が必要です。
このガイドの目的は、データベースを以下の本に制限することです。

> books :: [Book]
> books =
>   [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
>   , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
>   , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
>   , book "Graham Hutton" "Programming in Haskell" 2007
>   , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
>   , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
>   ]

任意の検索文字 `q` を与えるとすると、本の一覧で大文字小文字を区別しない検索を実施しようとする。明らかに
最も適したアルゴリズムを試したり実装したりしているわけではありません。それはこのチュートリアルの対象外です。
もし一覧が小さければ、以下の単純な線形探索の例で十分です。

> searchBook :: Monad m => Maybe Text -> m (Search Book)
> searchBook Nothing  = return (mkSearch "" books)
> searchBook (Just q) = return (mkSearch q books')
>
>   where books' = filter (\b -> q' `T.isInfixOf` T.toLower (author b)
>                             || q' `T.isInfixOf` T.toLower (title b)
>                         )
>                         books
>         q' = T.toLower q

`-1 <= x,y <= 1` の範囲の乱数 `(x, y)` を生成するエンドポイントも必要です。以下のコードには、
`System.Random` の [random](http://hackage.haskell.org/package/random) を使っています。

> randomPoint :: MonadIO m => m Point
> randomPoint = liftIO . getStdRandom $ \g ->
>   let (rx, g')  = randomR (-1, 1) g
>       (ry, g'') = randomR (-1, 1) g'
>   in (Point rx ry, g'')

もし静的ファイルをサーブしたいなら、以下のようにすれば完成です。

> api :: Proxy API
> api = Proxy
>
> api' :: Proxy API'
> api' = Proxy
>
> server :: Server API
> server = randomPoint
>     :<|> searchBook
>
> server' :: Server API'
> server' = server
>      :<|> serveDirectory "tutorial/t9"
>
> app :: Application
> app = serve api' server'

なぜ2つの異なる API type が proxy と server をするのでしょうか。
単純に API type の `Raw` の部分のために javascript 関数を生成したくないので、`Raw` エンドポイント
なしで `API` API type のために `Proxy` が必要です。

haskell 関数を導出する方法に非常に同じように、`Servant.JQuery` から `jsForAPI` を呼び出す単純な
関数で javascript を導出できる。

> apiJS :: String
> apiJS = jsForAPI api

この `String` は2つの javascript 関数を持っている。

``` javascript

function getpoint(onSuccess, onError)
{
  $.ajax(
    { url: '/point'
    , success: onSuccess
    , error: onError
    , method: 'GET'
    });
}

function getbooks(q, onSuccess, onError)
{
  $.ajax(
    { url: '/books' + '?q=' + encodeURIComponent(q)
    , success: onSuccess
    , error: onError
    , method: 'GET'
    });
}
```

サーバを始める前に、ファイルへこの `String` を書く必要があります。`api.js` は *jQuery* ライブラリ
のコピーとともに [js-jquery](http://hackage.haskell.org/package/js-jquery) によって提供されて
います。

> writeJSFiles :: IO ()
> writeJSFiles = do
>   writeFile "getting-started/gs9/api.js" apiJS
>   jq <- readFile =<< Language.Javascript.JQuery.file
>   writeFile "getting-started/gs9/jq.js" jq

以上です。`dist/build/tutorial/tutorial 9` でサーバを動かせます。
`http://localhost:8081/` で確認できます。
データベースの著者の1人の名前または本の題名の一部を入力すると、上記のメソッドを使って、
pi に近づくことになります。

<div style="text-align: center;">
  <p><a href="/tutorial/client.html">Previous page: Deriving Haskell functions to query an API</a></p>
  <p><a href="/tutorial/docs.html">Next page: Generating documentation for APIs</a></p>
</div>
