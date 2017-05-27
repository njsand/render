Render
======

## Introduction ##

This program draws a random lines into an image, and writes the result
as a PNG, intended as a demo of how to do mutable, C-style arrays in Haskell.

The size of the image, and number of lines drawn are controlled via
arguments.  An argument also controls what data structure is used to store the
image, as each random line is rasterized into it.  Currently, the choice is
between "array" and "list".  Type "list" means the program just uses the
standard Haskell list (actually a list of lists) to store the image.  Type
"array" means an IOUArray is used.

As you might expect, storing (and updating) an image as a list is very slow, and
the array version is much faster.

## Example Output ##

![10 white lines in a sea of black](/sample.png "10 white lines in a sea of black")

## Usage ##

The width and height of the image, and the number of lines to draw are
supplied on the command line.  As mentioned above, lines are created
randomly, so the program output will be different from run to run.

The choice of data structure for the image is also controlled via an argument.

Usage: `render {list|array} width height number-of-lines output-filename`

Example: `render list 800 600 42 myimage.png`.  This example draws 42 random,
straight lines into an 800 by 600 image and writes it out as a PNG.

NB: The arguments must be provided in the correct order, as above.  (Might
investigate a getopts style package to improve this with named parameters and so
on.)

Finally, a convenient way to run this thing after a `stack build` is something
like `stack exec render-exe array 800 600 10 test.png`.

## Running Stats ##

TODO: Put some sample execution times with various parameters here.
