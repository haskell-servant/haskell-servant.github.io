---
title: servant paper submitted to WGP 2015
author: David Johnson, Nickolay Kudasov, Julian Arni
date: 2015-05-25 12:00
---

# Swagger

`Servant` is not the first project to provide a unified way of documenting APIs.
There is `API Blueprint`, `RAML`, `Apiary`, and finally `swagger`. While these
Web API Description languages don't do much in the way of helping you build web
services in a type-safe way, they are generally very mature, and have some
amazing tooling. For example, take a look at what `swagger-ui`, a client-side
HTML, CSS, and JS bundle, does with your `swagger` API description:

...

This is a pretty nifty way of documenting your API! But it doesn't end there.
If you have a `swagger` specification of you API, you can also take advantage
of the large variety of [languages](https://github.com/swagger-api/swagger-codegen/blob/master/README.md#customizing-the-generator) for which you can generate a client
library automatically. You don't even need to build the Java code - you can
just use the "Generate Client" button in the beautiful
[swagger editor](http://editor.swagger.io/#/).

There are a wide array of other [tools](http://swagger.io/open-source-integrations/)
that support `swagger`. Obviously, having access to them would be a great boon.
The problem so far has been that writing and maintaining a `swagger`
specification, that you are sure matches your service, isn't fun.

# Swagger-servant

Thankfully David Johnson and Nickolay Kudasanov have written two wonderful Haskell
libraries, [swagger2](https://hackage.haskell.org/package/swagger2) and
[servant-swagger](https://hackage.haskell.org/package/servant-swagger), that
automate nearly all of that process for `servant` APIs.
