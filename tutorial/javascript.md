---
title: Deriving Javascript functions to query an API
toc: true
---

We will now see how *servant* lets you turn an API type into javascript functions that you can call to query a webservice. The derived code assumes you use *jQuery* but you could very easily adapt the code to generate ajax requests based on vanilla javascript or another library than *jQuery*.

For this, we will consider a simple page divided in two parts. At the top, we will have a search box that lets us search in a list of Haskell books by author/title with a list of results that gets updated every time we enter or remove a character, while at the bottom we will be able to see the classical [probabilistic method to approximate &pi;](http://en.wikipedia.org/wiki/Approximations_of_%CF%80#Summing_a_circle.27s_area), using a webservice to get random points. Finally, we will serve an HTML file along with a couple of Javascript files, among which one that's automatically generated from the API type and which will provide ready-to-use functions to query your API.

Let's start with the API type(s) and the accompanying datatypes.

``` haskell
type API = "point" :> Get '[JSON] Point
      :<|> "books" :> QueryParam "q" Text :> Get '[JSON] (Search Book)

type API' = API :<|> Raw

data Point = Point
  { x :: Double
  , y :: Double
  } deriving Generic

instance ToJSON Point

data Search a = Search
  { query   :: Text
  , results :: [a]
  } deriving Generic

mkSearch :: Text -> [a] -> Search a
mkSearch = Search

instance ToJSON a => ToJSON (Search a)

data Book = Book
  { author :: Text
  , title  :: Text
  , year   :: Int
  } deriving Generic

instance ToJSON Book

book :: Text -> Text -> Int -> Book
book = Book
```

We need a "book database". For the purpose of this guide, let's restrict ourselves to the following books.

``` haskell
books :: [Book]
books =
  [ book "Paul Hudak" "The Haskell School of Expression: Learning Functional Programming through Multimedia" 2000
  , book "Bryan O'Sullivan, Don Stewart, and John Goerzen" "Real World Haskell" 2008
  , book "Miran Lipovača" "Learn You a Haskell for Great Good!" 2011
  , book "Graham Hutton" "Programming in Haskell" 2007
  , book "Simon Marlow" "Parallel and Concurrent Programming in Haskell" 2013
  , book "Richard Bird" "Introduction to Functional Programming using Haskell" 1998
  ]
```

Now, given an optional search string `q`, we want to perform a case insensitive search in that list of books. We're obviously not going to try and implement the best possible algorithm, this is out of scope for this tutorial. The following simple linear scan will do, given how small our list is.

``` haskell
searchBook :: Monad m => Maybe Text -> m (Search Book)
searchBook Nothing  = return (mkSearch "" books)
searchBook (Just q) = return (mkSearch q books')

  where books' = filter (\b -> q' `T.isInfixOf` T.toLower (author b)
                            || q' `T.isInfixOf` T.toLower (title b)
                        )
                        books
        q' = T.toLower q
```

We also need an endpoint that generates random points `(x, y)` with `-1 <= x,y <= 1`. The code below uses [random](http://hackage.haskell.org/package/random)'s `System.Random`.

``` haskell
randomPoint :: MonadIO m => m Point
randomPoint = liftIO . getStdRandom $ \g ->
  let (rx, g')  = randomR (-1, 1) g
      (ry, g'') = randomR (-1, 1) g'
  in (Point rx ry, g'')
```

If we add static file serving, our server is now complete.

``` haskell
api :: Proxy API
api = Proxy

api' :: Proxy API'
api' = Proxy

server :: Server API
server = randomPoint
    :<|> searchBook

server' :: Server API'
server' = server
     :<|> serveDirectory "tutorial/t9"

app :: Application
app = serve api' server'
```

Why two different API types, proxies and servers though? Simply because we don't want to generate javascript functions for the `Raw` part of our API type, so we need a `Proxy` for our API type `API'` without its `Raw` endpoint.

Very similarly to how one can derive haskell functions, we can derive the javascript with just a simple function call to `jsForAPI` from `Servant.JS`.

``` haskell
apiJS :: String
apiJS = jsForAPI api jquery
```

This `String` contains 2 Javascript functions:

``` javascript

var getpoint = function (onSuccess, onError)
{
  $.ajax(
    { url: '/point'
    , success: onSuccess
    , error: onError
    , method: 'GET'
    });
};

var getbooks = function (q, onSuccess, onError)
{
  $.ajax(
    { url: '/books' + '?q=' + encodeURIComponent(q)
    , success: onSuccess
    , error: onError
    , method: 'GET'
    });
};
```

Right before starting up our server, we will need to write this `String` to a file, say `api.js`, along with a copy of the *jQuery* library, as provided by the [js-jquery](http://hackage.haskell.org/package/js-jquery) package.

``` haskell
writeJSFiles :: IO ()
writeJSFiles = do
  writeFile "getting-started/gs9/api.js" apiJS
  jq <- readFile =<< JQ.file
  writeFile "getting-started/gs9/jq.js" jq
```

And we're good to go. Start the server with `dist/build/tutorial/tutorial 9` and go to [http://localhost:8081/](http://localhost:8081/). Start typing in the name of one of the authors of our database or part of a book title and check out how long it takes to approximate &pi; using the method mentioned above.

## Customizations

Instead of calling `jquery`, you can call its variant `jqueryWith`.
Here are the type definitions

```haskell
jquery :: JavaScriptGenerator
jqueryWith :: CommonGeneratorOptions -> JavaScriptGenerator
```

The `CommonGeneratorOptions` will let you define different behaviors to
change how functions are generated. Here is the definition of currently
available options:

```haskell
data CommonGeneratorOptions = CommonGeneratorOptions
  { 
    -- | function transforming function names
    functionRenamer :: String -> String
    -- | name used when a user want to send the request body (to let you redefine it)
  , requestBody :: String
    -- | name of the callback parameter when the request was successful
  , successCallback :: String
    -- | name of the callback parameter when the request reported an error
  , errorCallback :: String
    -- ^ namespace on which we define the js function (empty mean local var)
  , moduleName :: String
  }
```

This pattern is available with all supported backends, and a default instance

## Vanilla support

If you don't require JQuery for your application, you can reduce your
dependencies to simply use `XMLHttpRequest` object from standard API.

Follow the same code as previous part, and simply replace the `apiJS`
definition to this one:

``` haskell
apiJS :: String
apiJS = jsForAPI api vanillaJS
```

The rest is *completely* unchanged.

The output file is a bit different, but it has the same parameters,

``` javascript

var getpoint = function (onSuccess, onError)
{
    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/point', true);

    xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
        if (xhr.status == 200 || xhr.status == 201) {
            onSuccess(value);
        } else {
            onError(value);
        }
        }
    }
    xhr.send(null);
};

var getbooks = function (q, onSuccess, onError)
{
    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/books' + '?q=' + encodeURIComponent(q), true);

    xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
        var value = JSON.parse(xhr.responseText);
        if (xhr.status == 200 || xhr.status == 201) {
            onSuccess(value);
        } else {
            onError(value);
        }
        }
    }
    xhr.send(null);
};
```

And that's all, your web service can of course be accessible from those
two clients at the same time!

## Angular support

### Simple version

You can apply the same procedure as with `vanillaJS` and `jquery`, and
generate top level functions.

The difference is that `angular` Generator always takes an argument.

``` haskell
apiJS :: String
apiJS = jsForAPI api $ angular defAngularOptions
```

The generated code will be a bit different than previous generators. An extra
argument `$http` will be added to let Angular magical Dependency Injector
operate.

``` javascript

var getpoint = function($http)
{
  return $http(
   { url: '/counter'
   , method: 'GET'
   });
}

var getbooks = function($http, q)
{
  return $http(
    { url: '/books' + '?q=' + encodeURIComponent(q), true);
    , method: 'GET'
    });
}
```

You can then build your controllers easily

``` javascript

app.controller("MyController", function($http) {
  this.getpoint = getpoint($http)
    .success(/* Do something */)
    .error(/* Report error */);
    
  this.getpoint = getbooks($http, q)
    .success(/* Do something */)
    .error(/* Report error */);
});
```

### Service generator

You can also generate automatically a service to wrap the whole API as
a single Angular service:

``` javascript
app.service('MyService', function($http) {
  return ({
  postcounter: function()
  {
   return $http(
     { url: '/counter'
     , method: 'POST'
      });
  },
  getcounter: function()
  {
   return $http(
     { url: '/books' + '?q=' + encodeURIComponent(q), true);
     , method: 'GET'
      });
  }
  });
});
```

To do so, you just have to use an alternate generator.

``` haskell
apiJS :: String
apiJS = jsForAPI api $ angularService defAngularOptions
```

Again, it is possible to customize some portions with the options.

``` haskell
data AngularOptions = AngularOptions
  { -- | When generating code with wrapInService, name of the service to generate, default is 'app'
    serviceName :: String
  , -- | beginning of the service definition
    prologue :: String -> String -> String
  , -- | end of the service definition
    epilogue :: String
  }
```

# Example

Part of the library, an [example](https://github.com/haskell-servant/servant/blob/new-js-codegen/servant-js/examples/counter.hs)
shows how you can use the module to generate all the variants and serves
them with a concurrent counter scenario.

Each part of the client uses a different connector and they all talk to the same
server implementation.