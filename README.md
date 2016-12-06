# cppnx

A web ui for exploring [Compositional Pattern Producing Networks](http://eplex.cs.ucf.edu/publications/2007/stanley-gpem07).

The app is online at [floybix.github.io/cppnx](https://floybix.github.io/cppnx/).

A gallery is on twitter at [#cppnx](https://twitter.com/hashtag/cppnx).


## Compared to [picbreeder](http://picbreeder.org/) and [neurogram](http://otoro.net/neurogram/)

### History

Picbreeder is the original site. Started in 2007, it has a lot of history, with
thousands of evolved images. Check it out, the images are truly incredible.

Neurogram is a 2015 reimagining of picbreeder in Javascript with a sleek,
minimal design. (If I'd known about it, I probably wouldn't have built cppnx
from scratch).

Cppnx was written in 2016.

### User interface

The primary UI of both picbreeder and neurogram is a black box - the user simply
selects images. Both have a network view on a separate screen. Picbreeder
has a "DNA analysis" applet which can vary nodes and edges.

Cppnx has more controls and their effect is immediate. For instance you can
easily toggle off nodes and adjust the weights of edges.

Cppnx also has a weight-space tour: continuously varying the network weights
in a smooth animation, similar to a
[grand tour][grandtour] as
used in multi-variate data visualisation.

  [grandtour]:https://en.wikipedia.org/wiki/Grand_Tour_(data_visualisation)

### Crossover

Both picbreeder and neurogram include crossover (re-combining multiple networks)
using the NEAT algorithm. However, cppnx does not. In cppnx a single network
rather than a population is the focus at each step, and it is mutated
independently.

### Sharing

Both picbreeder and neurogram host their own database. Picbreeder has several
useful views of the collection including tags, a rating system, and recent work.

Cppnx does not host a database. The definition of a CPPN is stored in the
page location text (uri) and this can be posted to twitter for sharing, rating
etc. **This is actually kind of dumb.** In practice if the uri is over ~4000
characters it can't be posted to twitter. That works out to around 70 nodes.

### Front-end / code

* Picbreeder uses Java applets. Those don't work in Chrome or on iOS.
  Code is not public afaik.

* Neurogram uses Javascript and HTML5 Canvas which works in all modern browsers.

* Cppnx uses Javascript (via Clojurescript) and WebGL which works in all
  modern browsers as far as I know, and is fast enough for animation.



## Local setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.

## License

Copyright © 2016 Felix Andrews

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
