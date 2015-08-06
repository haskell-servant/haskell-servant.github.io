---
title: Content Type Bliss
author: Julian K. Arni
date: 2015-08-05 12:00
---

Recently I came across Timo von Holtz's
[servant-JuicyPixels](https://hackage.haskell.org/package/servant-JuicyPixels)
package. It describes `servant`-compatible content-types for JuicyPixel's
`DynamicImage` data type, and clocks under 100 airy lines.

Timo and I realized there is a pretty neat demonstration of abstracting away
content-type serialization and deserialization: the world's shortest
image-conversion web service.

To convert, we'll use `Content-Type` and `Accept` headers:

> $ curl localhost:8001 -H "Content-Type: image/png"  \
>                       -H "Accept: image/jpeg"  \
>                       --data-binary "@haskell-logo.png" \
>                       > haskell-logo.jpeg

We need to do a couple of things. Run the application:

> main :: IO ()
> main = run 8001 conversion

Describe the API:

> type ConversionApi
>      = ReqBody '[BMP, GIF, JPEG 50, PNG, TIFF, RADIANCE] DynamicImage
>     :> Post '[BMP, GIF, JPEG 50, PNG, TIFF, RADIANCE] DynamicImage

As you can see, we state that we accept and can return a variety of image
formats.

The application:

> conversion :: Application
> conversion = serve (Proxy :: Proxy ConversionApi) handler

And for the clincher, the handler:

>    where handler = return
