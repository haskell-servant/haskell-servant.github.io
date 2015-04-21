---
title: Versioning your API
toc: true
---

The *servant* package contains everything needed to describe an API, and little else. In particular, we decided to keep servant-server as a separate package. This enables an easy and effective way of versioning your API that avails itself of familiar tools - Hackage and cabal - in what we think is a novel way.

The basic idea is that, when writing a service, you can keep your API as a separate package, containing only the type that (with the servant combinator) describes your API, and the types that your endpoints accept (via query parameters, captures, or request bodies) and return.

``` haskell
-- The entire contents of your API package
module MyAPI where

import Servant

type MyAPI = "date" :> Get Date
        :<|> "time" :> Capture "tz" Timezone :> Get Time

data Date = ...
data Timezone = ...
data Time = ...
```

This package is a perfect encapsulation of your service's interface - everything that's needed to interact with it, but nothing of the internals. The types that you include are only those that client code needs to have access to. You can upload this to Hackage without exposing code that should only be private.

Your server can then depend on the package. Other users (or you yourself) can write packages that interact with that API simply by importing (a) servant-client and (b) your API package. They ought not place version bounds on your API package, however (for reasons that might already be clear).

In essence, any change to the code (i.e., not comments or documentation) of your API package will constitute a change to your API, and as such indicates that client code may break. If it does break, a build from scratch (such as you get from travis) will automatically indicate this, since assumptions about your API will be reflected as type-level incompatibilities with the latest Hackage version of your package.

In this manner, you can develop both server and client code in a way that feels natural, without much overhead, and guarantee that API changes are safe. You can't update your server in a way that necessitates an API change without changing your API package version. And you can't write client code that relies on an API other than the one deployed, unless you develop against stale versions. (The only exception is the always-tricky case of an endpoint whose name and types don't change, but whose behavior does in ways the client may not expect. Such changes should, in any service, be avoided as much as possible.) This assumes there are no version bound placed in your API package - otherwise, there's no guarantee that server and client agree on the interface.

You can also have regular builds of your client code so that you are automatically notified of any API changes in a server your interface with that break your code!

Of course, you might still choose to unpack your API into client code yourself (which has the added benefit of allowing you to use `DEPRECATE` pragmas for deprecated endpoints). But even then, separating your API into its own package, belonging neither to the client code nor the server code, but being a dependency of both, maintains the clear boundary between server and client.
