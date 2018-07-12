---
title: Why is servant a type-level DSL?
author: Alp Mestanogullari
date: 2018-07-12 20:00
toc: true
---

---

This post is an attempt at explaining servant's design as an embedded domain
specific language, and particularly why it _had to_ be a _type-level_ domain
specific language, given our requirements. Along the way, we will discuss
approaches for designing extensible EDSLs in Haskell and see why other simpler
approaches just don't cut it.

# It all started with a problem

Back in 2014, Sönke Hahn, Julian Arni and myself were working together in "the
Haskell team" at Zalora on all sorts of projects. Many of them involved serving
web applications, querying external APIs or our own services from Haskell,
PHP, JS and probably a few other languages. At the time, we were using
a few of the well established "web frameworks", among which `scotty`,
whenever we had to offer some service over HTTP.

However, writing all those functions for hitting our own webservices was a lot
of manual, error-prone, tedious work. The bigger web applications got, the more
tedious it became. And it had to be done once per language in which we wanted to
hit the application. This could not continue.

For reference, this is what a simple scotty application looks like:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (split)
import Web.Scotty

main :: IO ()
main = scotty 8000 $
  get "/repeat/:n" $ do
    n <- param "n"
    json (replicate n n)

  post "/message" $ do
    msg <- jsonData
    json (split "\n" msg)
```

How could we somewhat automate the creation of one client function per
endpoint of the web application? In an ideal world, we would just show this
application to some program or library and it would collect all the data it
needs about the overall structure of the application from the code itself, in
order to produce 2 client functions:

``` haskell
-- Client for the first endpoint.
--
-- The Int is the value you want to set ":n" to (/repeat/23, /repeat/10, ...).
getRepeat :: Int -> ClientMonad [Int]

-- Client for the second endpoint.
--
-- The JSON body (just a naked string) to send is the Text argument.
postMessage :: Text -> ClientMonad [Text]
```

which would do all the hard work of preparing an HTTP request for us, even
taking care of JSON encoding and decoding for us. But... the entire structure
of the application is just hidden in the `do` block and we just cannot
programmatically access it.

So... this is not realistically doable. We clearly _need_ to change (a little bit?
a lot?) the way we write our applications, making sure we get a description
of the web's application structure (the endpoints, the part of the request they
use or depend on, what they return) that we could then hand over to _something_,
which would get us our client functions.

We will now try implementating such a web application description DSL in the
most straightforward way possible.

# A first, non-modular attempt

We want to produce client functions that look like the ones above, that
prepare and send HTTP requests for us by taking some pieces of data
given as arguments to those functions and encoding then storing
them in the right places of the request (request path for
URL captures, request body, headers, etc). Let's perhaps start
simple with a data type for describing an endpoint that can be
served under some path (which can contain static string fragments
and captures), for a given http method, ignoring everything else for now.

It could look like this:

``` haskell
data Method = Get | Post

data Endpoint = Static String Endpoint
              | Capture Endpoint
              | Verb Method

-- GET /hello/:name
getHello :: Endpoint
getHello = Static "hello" (Capture (Verb Get))
```

and, if we want it to look a little more "servant-y", we can define:

``` haskell
infixr 5 :>
(:>) :: (Endpoint -> Endpoint) -> Endpoint -> Endpoint
f :> x = f x

getHelloNew :: Endpoint
getHelloNew = Static "hello" :> Capture :> Verb Get
```

Unlike servant though, as you can see with the type of `getHello` and
`getHelloNew`, our descriptions are good old Haskell values, both of the
`Endpoint` type.

Given those few definitions, how could we go about, say, generating links
to endpoints? Well, here is a straightforward attempt.

``` haskell
-- a link here is just a list of path components
-- (we ignore query parameters in this post)
type Link = [String]

linkTo :: Endpoint -> Link
linkTo (Static str rest) = str : linkTo rest
linkTo (Verb _method)    = []
linkTo (Capture rest)    = ??? : linkTo rest
```

But... what should we put in place of those `???`, if anything?

Well, we definitely want to add _some_ path component, to fill the
`Capture` slot. However, by definition, a captured path fragment is
not fixed, it is allowed to vary. In other words, `Capture :> Verb Post`
matches both `POST /x` and `POST /y`. We cannot just pick one value
and hope that it is the one the user wanted. We need to take it as an
argument. But what about `Capture :> Capture :> Verb Post`? We would need
our `linkTo` function to take 2 arguments for that case. And zero additional
argument for `Static "hello" :> Verb Post`. This is quite problematic.

Indeed, we would like the type of `linkTo` to be `Endpoint -> Link`,
`Endpoint -> String -> Link`, `Endpoint -> String -> String -> Link` and
so on depending on what the `Endpoint` argument is. In other words,
we want the return type of `linkTo` (when really seen as a function of
one argument, which it is anyway) to depend on the value of type `Endpoint`
it gets as input. That is, we want a type that depends on a value, i.e
dependent types.

Fortunately, GADTs can help here. We could turn `Endpoint` into
a GADT that tracks captures and use some type-level computations
to get the type of the link-making function from our list of captures, as well
as define the link making function through typeclass instances that would
go through the captures and add an argument for each of them. Request bodies,
query parameters, headers? We could probably track them too, in a similar way.
Or we could unify it all by basically building up and tracking API types
through a `GADT` version of Endpoint's type argument, and do some of what
servant does at the type-level, with everything else at the value-level.

However, all those approaches have a big problem. Once you've made a decision,
it is set in stone, in a way. You cannot explore two
different directions simultaneously without breaking code, you cannot add new
constructs you hadn't thought of before. Extensibility and modularity
were central requirements as we had been bitten by the lack of them
in libraries that we were using at the time.

So... how do people build extensible/modular DSLs in Haskell? The next section
talks about the general problem behind this and a solution that I read about
that gets us halfway to servant.

# The Expression Problem

To quote [Phil Wadler](http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt):
"_the expression problem is a new name for an old problem. The goal is to define
a datatype by cases, where one can add new cases to the datatype and new
functions over the datatype, without recompiling existing code, and while
retaining static type safety (e.g., no casts)_".

In Haskell, the standard approach to representing some domain is
to define an algebraic data type for it. For a simple type of expressions
with additions and integers, we usually do:

``` haskell
data Expr = I Integer | Add Expr Expr
```

and proceed to write what we call "interpreters", which in this case are just
functions that take expressions as input and do something interesting with them.

``` haskell
eval :: Expr -> Integer
eval (I n)     = n
eval (Add a b) = eval a + eval b

prettyPrint :: Expr -> String
prettyPrint (I n) = show n
prettyPrint (Add a b) = unwords [prettyPrint a, "+", prettyPrint b]
```

So, given an expression type, we can easily "add new functions over the
data type", to reuse Phil's wording. We just write a new function.
However, when the time comes to "add new cases to the data type", this approach
becomes painful. A "new case" here means a new constructor for our `Expr` data
type. Let's say we want to support multiplications too:

``` haskell
data Expr = I Integer | Add Expr Expr | Mul Expr Expr
```

Now, we have to modify _every single function that patterns matches on an
`Expr`_ to handle the `Mul` constructor, including our `eval` and `prettyPrint`
"interpreters". For any non-trivial domain, this becomes _very_ painful,
_very_ quickly. Fine, so what other options are there?

[Ralf Lämmel's slides](https://userpages.uni-koblenz.de/~laemmel/TheEagle/resources/pdf/xproblem1.pdf)
on the topic have been of a great help for me, back when we were looking for
a solution suitable to our needs. With Oleg Kiselyov, they show how we can
reasonably easily (that is, in Haskell 98) achieve full extensibility in both
directions (constructors and interpretations) in Haskell. It boils down to:

- Turn what would be a constructor into its own little data type.
- Turn what would be a simple function that operates on the data type into
  a typeclass with a method.
- Write instances of those typeclasses for the data types representing the DSL's
  constructs.

This effectively means that we won't have a single type to represent all the
vald "endpoint descriptions". Instead, with this approach, we will be able to
process any "reasonable" combination of "endpoint components". The `Expr` typeclass
below is exactly what lets us say what is a valid endpoint description and what
isn't. Using their approach for our expressions would look like this:

``` haskell
-- our expression constructs, one data type per
-- constructor we had previously.

	-- integer constants
data I = I Integer

-- Since we don't have an 'Expr' type anymore, to use as a type for
-- the fields of Add, we just make them type parameters. Sometimes
-- 'l' and 'r' might be I, some other times they might be 'Add I I',
-- or 'Add (Add I I) I', and so on. The type reflects the recursive
-- structure.
data Add l r = Add l r

-- an "open union" to be able to describe all the
-- valid expression types.
class Expr a
instance Expr I
instance (Expr l, Expr r) => Expr (Add l r)

-- our first interpretation, evaluation
class Expr a => Eval a where
  eval :: a -> Integer

-- evaluating a constant amounts to returning it
instance Eval I where
  eval (I n) = n

-- if we know how to evaluate two things, we know how to evaluate
-- their addition
instance (Eval l, Eval r) => Eval (Add l r) where
  eval (Add a b) = eval a + eval b

-- our second interpretation, pretty printing
class Expr a => Pretty a where
  pretty :: a -> String

instance Pretty I where
  pretty (I n) = show n

instance (Pretty l, Pretty r) => Pretty (Add l r) where
  pretty (Add a b) = unwords [pretty a, "+", pretty b]
```

Every constructor that we had in our previous `Expr` data type is now
turned into its own little type, and every interpretation becomes a type
class that all those little types are then free to provide an instance for.
In fact, we do not necessarily have to supply an instance of each interpretation
for all of our constructs. If we try to interpret an expression that uses a
construct not supported by this interpretation, we get a type error! This is
much better than calling `error` in some corner cases that should in theory
not be reached... In theory. Right.

Anyway, if we now want to add support for multiplications, we can simply do:

``` haskell
data Mul l r = Mul l r
instance (Expr l, Expr r) => Expr (Mul l r)

instance (Eval l, Eval r) => Eval (Mul l r) where
  eval (Mul a b) = eval a * eval b

instance (Pretty l, Pretty r) => Pretty (Mul l r) where
  pretty (Mul a b) = unwords [autoParens a, "*", autoParens b]
    where autoParens a@(Add _ _) = "(" ++ pretty a ++ ")"
          autoParens           a = pretty a
```

We didn't have to change any existing function, that's great! Let's apply this
approach to a very simplified web application description "language" that we
could make out of tiny building blocks (static path fragments, captures, etc).

# A first modular attempt

Adapting the approach from the previous section to our domain, we can give a
shot at decomposing the kind of information we want to represent into
a few different "constructs" (i.e data types).

``` haskell
-- static path fragments
data Static = Static String

-- variable path fragments ("captures")
data Capture = Capture

-- HTTP method
data Method = Get | Post
-- Leaf of a chain of :>'s, specifies the HTTP method
data Verb = Verb Method

-- chain a few "endpoint components" with this operator,
-- all chains must be terminated with a 'Verb' component.
infixr :> 5
data a :> b = a :> b

-- a class to specify all the valid endpoint descriptions
class Endpoint a
instance Endpoint (Verb a)
instance Endpoint rest => Endpoint (Static :> rest)
instance Endpoint rest => Endpoint (Capture :> rest)

-- GET /hello
endpoint1 = Static "hello" :> Verb Get
```

OK, why not. Let's now try to write an interpretation for generating links
to endpoints like the one above. This is a lot simpler and self-contained than
investigating client generation or server-side routing, while retaining many of
the difficulties. The main one is that depending on what we find in the
description of the endpoint, we need the type of the link-generating function
to change: indeed, if we encounter `Capture`s, then the user has to supply
values for them. We will let the user do that through one additional
argument per Capture we encounter.

Let's start with something really simple.

``` haskell
type Link = [String]

-- @renderLink ["hello", "world"] == "/hello/world"@
renderLink :: Link -> String
renderLink xs = '/' : intercalate '/' xs

class HasLink endpoint where
  -- return the path components
  link :: endpoint -> [String]

instance HasLink api => HasLink (Static :> api) where
  link (Static s :> api) = s : link api

instance HasLink api => HasLink (Capture :> api) where
  link (Capture :> api) = ??? : link api

instance HasLink Verb where
  link _ = []
```

We should be appending something in place of those `???` there.
But since `Capture` represents variable path fragments (like `:userid` in
`/user/:userid`, in many web frameworks), we do not want to pick a fixed string,
we would like for this string to be supplied by the caller of `link`, as stated
above. Let's introduce a slightly fancier `HasLink` class to make it seemingly
"variadic".

``` haskell
class HasLink endpoint where
  type LinkType endpoint :: *
  link :: endpoint -> LinkType

instance HasLink Verb where
  type LinkType Verb = Link
  link _ = []

instance HasLink api => HasLink (Static :> api) where
  type LinkType (Static :> api) = LinkType api
  link (Static s :> api) = s : link api

instance HasLink api => HasLink (Capture :> api) where
  -- HERE! we introduce a String argument
  type LinkType (Capture :> api) = String -> LinkType api

  -- we expand the type of link:
  -- link :: (Capture :> api) -> String -> LinkType api
  -- we see that our little `LinkType` trick there allows
  -- link to receive arguments when appropriate
  link (Capture :> api) captureValue = captureValue : link api

-- examples:

-- "/hello"
simpleEndpointLink = renderLink (link endpoint1)

endpoint2 = Capture :> Verb Post
linkFun2 :: String -> Link
linkFun2 = link endpoint2

link2a = renderLink (linkFun2 "foo") -- "/foo"
link2b = renderLink (linkFun2 "bar") -- "/bar"

endpoint3 = Static "hello" :> Capture :> Capture :> Verb Get
link3 = renderLink (link endpoint3 "x" "y") -- "/hello/x/y"
```

This looks promising. Let's now try to introduce some more types here,
by allowing captures to not be specified just as simple strings, but any
`Show`able type (this is terrible, but simple enough for this post). We need
to modify `Capture` to track that `Show`able type we will use to specify
the value of that path fragment.

``` haskell
data Capture a = Capture

instance (Show a, HasLink api) => HasLink (Capture a :> api) where
  -- HERE! we introduce an argument of type 'a'
  type LinkType (Capture :> api) = a -> LinkType api

  -- we expand the type of link:
  -- link :: (Capture a :> api) -> a -> LinkType api
  -- we see that our little `LinkType` trick there allows
  -- link to receive the argument of type 'a' at the right time, just
  -- when we need to stick it at the top of the list
  link (Capture :> api) captureValue = show captureValue : link api
```

We unfortunately cannot just "track" some type by storing it in a field
(which is different from storing _a value of that type_). Instead we make
`Capture` a clone of `Proxy` (from `Data.Proxy`) and just carry around a phantom
type parameter. This is a little inconvenient as we will _have to_ type annotate
_all_ `Capture`s (or use the `TypeApplications` language extension), but let's
roll with this approach for now.

Let's now see an endpoint description using this variant of `Capture`.

``` haskell
endpoint4  = Static "hello" :> (Capture :: Capture Int) :> Verb Post
-- or, with TypeApplications:
endpoint4' = Static "hello" :> (Capture @ Int) :> Verb Post
```

OK, interesting, why not. It does look a little bit ugly. It would look
even uglier if we included the response type in `Verb`, turning it into
`data Verb a = Verb Method` which would require the same kind of type
annotations. And the same problem would manifest itself if we were to add
all the similar types from servant (`ReqBody`, `QueryParam`, `Header`, etc).
This is quite disappointing.

Unrelatedly, have you noticed that I have not given the type of any of our
endpoint descriptions so far? This is on purpose, because those types are
a little bit fancy. Fortunately, they should look familiar:

``` haskell
endpoint1 :: Static :> Verb
endpoint2 :: Capture String :> Verb
endpoint3 :: Static :> Capture String :> Capture String :> Verb
endpoint4 :: Static :> Capture Int :> Verb
```

That's right, not only do the descriptions (which are good old haskell values)
look like servant's API types, but their types too! We can see that we are only
"hiding" the strings (in static path fragments) and the HTTP method (in verbs)
from the type-level.

Most of the other bits of information we would want to see
in API descriptions will also have to be reflected at the type-level.
When we consider content types for example, we have no choice
but to keep track of them at the type level too, even with this design. Because
we need to make sure suitable encoding/decoding instances are available for the
types that will be represented with those MIME types, and this cannot be done when
discovering `"application/json"` in a list somewhere, at runtime.

All in all, there is no value in keeping anything at the value level at this point.
And we are already traversing a bunch of types mixed together with funny symbols and
computing the type of a link making function as we go,
as evidenced by the `HasLink` instances from above, so we've already got one
foot in type-level land.

An important tradeoff that we are making here is that while putting more
information at the type-level indeed makes things more complex, it does
however give a chance to our descriptions to influence more things, including
other types. This is noticeable in the last `HasLink` instance we wrote, where
making `Capture` track the type the url fragment is going to be decoded to
allowed us to directly make the link-making function take a value of that type,
instead of a string. This is strictly more powerful and will allow us to work
in a very strongly typed world and where the typechecker "writes" a whole lot of
code for us.

Let's bite the bullet and finally take a quick look at what servant's type-level
approach looks like.

# Servant's approach (simplified)

First, let me emphasize that any of the designs we have considered so far
are interesting on their own and are fruitful in different ways. They simply
were not quite good enough to meet our requirements which were, again, dictated
by the projects and needs we had at work. This whole project started because
we were sick of getting things wrong when manually constructing (client) or
deconstructing (server) HTTP requests and so on.

Now, let's write our type-level DSL. If you want a longer version of just this
section, with more explanations, you may want to read
[Implementing a minimal version of servant](https://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/).

``` haskell
-- GHC-flavoured Haskell supports type-level strings, they are the only type-level
-- entity to have kind Symbol. We will therefore just use those,
-- wrapped with 'Static'. See the first link in the "Going further"
-- section if you're not familiar with type-level strings, kinds, etc.
data Static (str :: Symbol)
data Capture a

-- GHC-flavoured Haskell (with the DataKinds extension) supports promoting ordinary data types
-- to kinds and their constructors to types of those kinds. See again the
-- first link in the "Going further" section if this is new to you.
-- In our example, this lets us parametrize Verb not by an ordinary type but by
-- a constructor of 'Method'. Indeed, 'method' can only be instantiated to
-- Get and Post.
data Method = Get | Post
data Verb (method :: Method)

infixr 5 :>
data a :> b
```

As you can see, there isn't a single constructor in sight, all the types (but Method)
are empty. And now, we proceed with the `HasLink` class. Since we don't have any
value to give to the `link` method, given that the description is now a type,
we will use `data Proxy a = Proxy` to act as an intermediate between the value level,
where the calls to `link` will happen, and the type level, where the descriptions
live and drive the link interpretation through our typeclass instances.

``` haskell
class HasLink api where
  type LinkType api :: *

  link :: Proxy api -> LinkType api

instance HasLink (Verb method) where
  type LinkType (Verb method) = Link

  link _ = []

instance (KnownSymbol str, HasLink api) => HasLink (Static str :> api) where
  type LinkType (Static str :> api) = LinkType api

  -- we call some "magic" GHC function, symbolVal, to turn type-level
  -- strings to good old value level strings.
  link api = symbolVal (Proxy :: Proxy str) : link (apiTail api)

instance (Show a, HasLink api) => HasLink (Capture a :> api) where
  type LinkType (Capture a :> api) = a -> LinkType api

  link api a = show a : link (apiTail api)

-- we're just specifying a very handy type for a function
-- that's in fact much more general (forall a b. Proxy a -> Proxy b).
-- no magic going on, we just decide that this function takes endpoint description
-- shaped types and drops the first component.
apiTail :: Proxy (a :> b) -> Proxy b
apiTail Proxy = Proxy
```

It is not all that different from the code in the previous section.
We can use this as follows:

``` haskell
type Foo = Static "hello" :> Capture Int :> Capture Text :> Verb 'Get

linkFoo :: Int -> Text -> Link
linkFoo = link (Proxy :: Proxy Foo)

link1, link2 :: Link
link1 = linkFoo 40 "abc"
link2 = linkFoo 2987 "cba"
```

# Conclusion

I hope this little tour of some of the designs we explored on our way to writing
servant was useful and informative, whether from a Haskell EDSL writer
perspective or for any Haskeller who has ever wondered about why
the descriptions live at the type-level. The real servant libraries of course
have a much richer vocabulary for describing endpoints and entire APIs, and
offer many interpretations in addition to the type-safe links. But the core
ideas behind the design and implementation are the same ones we progressively
arrived at in this post.

# Going further

- [Servant, Type Families, and Type-level Everything - A look at advanced GHC features used in Servant](https://arow.info/blog/posts/2015-07-10-servant-intro.html)

  I suspect this is a rather useful resource for Haskellers who haven't yet
  encountered type-level programming in (GHC) Haskell.

- [Implementing a minimal version of servant](https://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/)

  A more approchable and more narrowly focused alternative to the servant paper,
  which consists in implementing a very simplified version of servant, using
  however the same "API type" based approach for the EDSL as the real servant.

- [the servant paper](https://alpmestan.com/servant/servant-wgp.pdf), published at
  the Workshop on Generic Programming, 2015.

- [Software extensions and Integration with Type Classes](https://www.informatik.uni-marburg.de/~kos/papers/gpce06.pdf)

  by Ralf Lämmel and Klaus Ostermann talks in greater depth than the slides
  about the highly modular approach to embedded domain specific languages in
  Haskell and uses it on several examples.

- [serv](https://github.com/tel/serv) and [solga](https://github.com/chpatrick/solga)
  are smaller, younger and (I think) humbler relatives of servant which make
  slightly different choices for the DSL.

  Somewhat relatedly, there is [servant-0.1](https://github.com/alpmestan/servant/tree/master#servant),
  which wasn't anything like the servant most people know.
