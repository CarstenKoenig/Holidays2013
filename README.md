Holidays2013
============

looking at 2weeks free time over the holidays this is where I hope to put some of my "reaserch" and toy samples

## Mandelbrot
I had a look around Hackage searching for a suitable library to help me generate pictures (I want come back and try to write a medicore raytracer in Haskell as advanced project) and [JuicyPixels](http://hackage.haskell.org/package/JuicyPixels) seemd just fine (I kindof liked the easieness of [`generateImage`](http://hackage.haskell.org/package/JuicyPixels-3.1.2/docs/Codec-Picture.html#v:generateImage))- so thank you Vincent for your great work!

Of course I need something to toy with and I always loved to write yet another mandelbrot-set drawing app ... so here we go!

Right now this will only produce a complete image of the well-known set - no zoom or anything. Like this: ![MandelbrotSetImage](./Mandelbrot/mandelbrot.png "Mandelbrot Set")

I modified the algorithm to test the [Repa](http://hackage.haskell.org/package/repa) package - that is I tried to compute the images pixel-array in parallel. So far the speedupt is not that great and I guess I have to really think this through a bit more.

### fiddling with [threepenny-gui](http://www.haskell.org/haskellwiki/Threepenny-gui)
After some timeout I decided to play a bit more with this mandelbrot-generator by trying to implement it using threepenny-gui.

As of now this works ... well ok - it's not very fast or very beautiful (just shows the image and some very basic "wait"-text).

I am not sure I really got the libraries spirit right but it was kindof fun (really nasty hacking and head-scratching, till I got it).

To get this running you have to run the executable and then go with your favorite browser to [localhost:10000](http://localhost:10000/).
It will take a while to create the starting image and after this is done you can click on a point on the image to zoom into that region (rinse and repeat / enjoy).

### obvious-stupid-mistakes I made
Yes this is embarassing, but here are the most-stupid:

- toyed with JuicyPixels and imported `PixelRGB8` but forgot the constructors (`PixelRGB8(..)`) and then being dumbfolded for several minutes by the obvious error:

    Not in scope: data constructor `PixelRGB8'

- tried to implement a widget into Threepenny-GUI to show my image. Needed some kind of accumulation to translate the clicks into new views (you see: i need the old-view to know "where" I clicked) into new, zoomed views ... the solution I found was to create a "mapper"-function between views for a click (at the image-coordinates) and then use this together with [accumulateB](http://hackage.haskell.org/package/threepenny-gui-0.4.0.1/docs/Reactive-Threepenny.html#v:accumB) interesting concept but a bit counterintuitive for me but logical in the end ... really love Haskell - always something new to figure out.
- 
## Thanks
Thanks to Heinrich Apfelmus (the creator of Threepenny-GUI), who had a look on my little toy-project and gave me lots of really kind feedback!
