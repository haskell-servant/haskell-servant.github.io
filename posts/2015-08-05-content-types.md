---
title: Content Type Bliss
author: Julian K. Arni
date: 2015-08-05 12:00
---

Recently I came across Timo von Holtz's
[servant-JuicyPixels](https://hackage.haskell.org/package/servant-JuicyPixels)
package. It describes `servant`-compatible content-types for JuicyPixel's
`DynamicImage` data type, and clocks under 100 airy lines.

Timo and I realized there is a pretty neat demonstration of the advantage of
abstracting away content-type serialization and deserialization: the world's
most concise image-conversion web service. Essentially the same application is
available as the [`image conversion` example](https://github.com/tvh/servant-JuicyPixels/blob/master/examples/image-conversion.hs) in
Timo's package.

(If you want to know more about how content-types work in `servant`, the
 [content-type section of the tutorial](http://haskell-servant.github.io/tutorial/server.html#using-content-types-with-your-data-types)
 has more information.)

Our goal is to provide a service that converts images between formats based on
the `Content-Type` and `Accept` headers of the request:

```shell
$ curl localhost:8001 -H "Content-Type: image/png"  \
                      -H "Accept: image/jpeg"  \
                      --data-binary "@haskell-logo.png" \
                      > haskell-logo.jpeg
```

To get there, we need to do a couple of things. We need to of course run the
application:

```haskell
main :: IO ()
main = run 8001 conversion
```

And describe the API:

```haskell
type ConversionApi
     = ReqBody '[BMP, GIF, JPEG 50, PNG, TIFF, RADIANCE] DynamicImage
    :> Post '[BMP, GIF, JPEG 50, PNG, TIFF, RADIANCE] DynamicImage
```

As you can see, we state that we accept and can return a variety of image
formats.

The application is then:

```haskell
conversion :: Application
conversion = serve (Proxy :: Proxy ConversionApi) handler
```

And for the clincher, the handler:

```haskell
    where handler = return
```
