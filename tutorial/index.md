---
title: Tutorial
author: Alp Mestanogullari
toc: true
---

<p class="notice">**注意: このチュートリアルは `servant-0.4` 向けです。最新版のチュートリアルは以下にあります: [haskell-servant.readthedocs.org](http://haskell-servant.readthedocs.org/).**
</p>

これは *servant* **0.4** 対応の初心者向けチュートリアルです。コメントや Issue は以下で受け付けます [this website's issue tracker](http://github.com/haskell-servant/haskell-servant.github.io/issues).

## Github

- リポジトリ: [haskell-servant/servant](https://github.com/haskell-servant/servant)
- ウェブサイト (このチュートリアルも含む): [haskell-servant/haskell-servant.github.io](https://github.com/haskell-servant/haskell-servant.github.io/)
- このチュートリアルについての質問や意見は Issue もしくは PR はこのサイトのリポジトリに気軽に送ってください！

# Introduction

*servant* は以下のような基本的性質を持っています。

- 簡潔さ (concision)

   これは幅広く適用できる原則です。重複な記述無しであなたのウェブサーバやクライアントライブラリの素敵なドキュメントを書けるべきです。あなたのリソースを手動で変換することなく、種類ごとに一度だけ変換する方法を示すべきです。もしあなたの大量のハンドラが同じクエリパラメータを渡すようになっているなら、ハンドラごとにロジックを繰り返すのではなく、一度だけそれをすべてのハンドラに適用してください。ハンドラがまとめて死んでしまわないように。その他にもいろいろあります。

- 柔軟性 (flexibility)

   これは個々のユースケースを考えなければ、容易に実現できます。あるライブラリのテンプレート機能を使いたければ、お好きにどうぞ。ただしFormも難しいことはなしで使いたいというのであれば、自分の意見を曲げないといけなくなります。

- 関心の分離 (separation of concerns)

   ハンドラとHTTPロジックは分離すべきです。*servant* におけるHTTPとRESTの哲学の信念では、ハンドラが一般的なデータ型、つまりそのリソースを返すことです。APIでは *servant* は、Content-Type のような *体裁* を扱います。これは一例に過ぎません。

- 型安全性 (type safety)

   あなたのAPIが仕様を満たしていることを確かめたいのですか？ それはコンパイラがやってくれます。リンク先が存在していることも確認したいのですか？ それもうまくやってくれます。

これらの原則を突き通すためには、あなたの予想よりも少し異なる手法を取ります。基本的な考え方は *APIの記述を具体化する* ということです。一度具体化すれば、すべてがついてきます。servant は拡張可能な方法でAPIの記述を具体化する、おそらく最初のウェブフレームワークになると思います。*型* としてAPIの記述を具体化する最初のウェブフレームワークであることは確信しています。

servant でウェブサービスを書けるようになるためには初めの2章を読めばいいですが、この文章の目的である servant を拡張するための2つの方法まで網羅しています。

# Tutorial

1. [型として扱うWeb API (A web API as a type)](/tutorial/0.4/api-type.html)
2. [APIをサーバとして動かす (Serving an API)](/tutorial/0.4/server.html)
3. [APIにクエリを投げるHaskellの関数を扱う (Deriving Haskell functions to query an API)](/tutorial/0.4/client.html)
4. [APIにクエリを投げるjavascript関数を生成する (Generating javascript functions to query an API)](/tutorial/0.4/javascript.html)
5. [APIのドキュメントを生成する (Generating documentation for APIs)](/tutorial/0.4/docs.html)
