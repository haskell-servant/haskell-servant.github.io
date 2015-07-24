---
title: New JavaScript Generators
author: Freezeboy
date: 2015-07-25 00:00
toc: true
---

# Summary

In the upcoming Servant version, it will be way easier to build a javascript
connector to your webservice. The chosen API is *as usual with Servant*
putting the hard work away from your eyes to let you concentrate on what you are
working on in most situations.

# The Problem

Until recently, we had a javascript generator using exclusively
JQuery, but depending on the framework you might want to use in your project,
it can be overkill to fetch [JQuery](http://jquery.org), or else you are using
a library that prefers doing things differently, such as 
[AngularJS](http://angular.io), which contains its own solution to handle
Ajax calls. On the other hand, sometimes your need is so simple that you don't even want JQuery.

One solution could have been to have multiple packages `servant-jquery`,
`servant-angular`, and so on... But we chose to put them all in the same
package because the generators share lots of *problems & solutions*.

We have to provide a function for each endpoint defined in the API, with as many
parameter as needed. In most cases the client application will simply call
these functions throughout its flow.

# How do I use it?

A complete example is present in the [tutorial](/tutorial/javascript.html).

Basically you can define your API as [usual](/tutorial/api-type.html),
``` haskell
data ServerAPI = realAPI    -- ^ This is the real api 
		 :<|> Raw   -- ^ As a helper, we will provide the
			    -- js file at the root of the API

serverApi :: Proxy ServerAPI
serverApi = Proxy
```

And now, we just have to generate the client code
``` haskell
generateApi :: IO ()
generateApi = do
	-- we generate different versions of the API
	writeJsForApi realAPI angular "angular.js"
	writeJsForApi realAPI vanilla "vanilla.js"
```

And we [serve](/tutorial/server.html) the bundle to the world
``` haskell
main :: IO ()
main = do
	generateApi
	-- And now let serve it
	run (serve serverApi) 8000
```

# What changed

## API simplification

Since `0.4` branch, we drastically simplified the API so that it is easier
to use and to extend (if you want to use the *next new javascript framework*.

## More options

Yes it may sound counter-intuitive with previous paragraph... but It's not!

A simple "configuration" mecanism lets you

- Rename *success* and *error* callbacks
- prefix your function definitions (to put them in a namespace, or simply
change the name)
- even customize the naming heuristics

Some generators have even more options (Namely Angular).

## Simpler interface to deal with more environments

A simple raw XMLHttpRequest-based backend has been included, it can therefore
be used without even requiring an external library for the most simple applications.

It is also an opportunity for people writing [Qt5/QML](http://doc.qt.io/qt-5/qmlapplications.html)
web clients to test Servant. This solution does not rely on a DOM structure.
You can write a graphical client to your web service with QML and JS.

If you want to use a Servant API as a backend to your node.js application, it is
also possible to connect them using
[this node package](https://www.npmjs.com/package/xmlhttprequest).

## Advanced integration with Angular

If you play with Angular applications, you are used to the *$http* service from
the core library. By default, our `angular` generator creates top level functions
that depend on this service (using Angular dependency mecanism).

There is an alternative solution if you simply want an _Angular service_ mapping
exactly your _Servant service_, in this case, generated code will depend on
*$service*.

This extension helped us to define new needs and demonstrate how a more complexe
generator can be included.

# Future

There are still room for improvements to make the generators even richer in
features, and we are open to suggestions. Please play with this tool and tell
us if you have any idea.

One particular aspect is about the generated function names. Currently,
they are derived from url, but we need to deal with potential conflicts.
