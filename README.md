Holidays2013
============

looking at 2weeks free time over the holidays this is where I hope to put some of my "reaserch" and toy samples

## Mandelbrot
I had a look around Hackage searching for a suitable library to help me generate pictures (I want come back and try to write a medicore raytracer in Haskell as advanced project) and [JuicyPixels](http://hackage.haskell.org/package/JuicyPixels) seemd just fine (I kindof liked the easieness of [`generateImage`](http://hackage.haskell.org/package/JuicyPixels-3.1.2/docs/Codec-Picture.html#v:generateImage))- so thank you Vincent for your great work!

Of course I need something to toy with and I always loved to write yet another mandelbrot-set drawing app ... so here we go!

Right now this will only produce a complete image of the well-known set - no zoom or anything. Like this: ![MandelbrotSetImage](./Mandelbrot/mandelbrot.png "Mandelbrot Set")

I modified the algorithm to test the [Repa](http://hackage.haskell.org/package/repa) package - that is I tried to compute the images pixel-array in parallel. So far the speedupt is not that great and I guess I have to really think this through a bit more.

### obvious-stupid-mistakes I made
Yes this is embarassing, but here are the most-stupid:

- toyed with JuicyPixels and imported `PixelRGB8` but forgot the constructors (`PixelRGB8(..)`) and then being dumbfolded for several minutes by the obvious error:

    Not in scope: data constructor `PixelRGB8'
