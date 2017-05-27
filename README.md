Render
======

## Introduction ##

This program draws a bunch of random lines into an image, and outputs the result
as a JPEG, intended as a demo of how to do mutable, C-style arrays in Haskell.

The size of the image, and the number of lines drawn are controlled via
arguments.  An argument also controls what data structure is used to store the
image, as each random line is rasterized into it.  Currently, the choice is
between "array" and "list".  Type "list" means the program just uses the
standard Haskell list (actually a list of lists) to store the image.  Type
"array" means an IOUArray is used.

As you might expect, storing (and updating) an image as a list is very slow, and
the array version is much faster.

## Example Output ##

TODO: Add a sample image here.

## Usage ##

The width and height of the image, and the number of lines to draw are
supplied on the command line.  As mentioned above, lines are created
randomly, so the program output will be different from run to run.

The choice of data structure for the image is also controlled via an argument.

Usage: `render {list|array} width height number-of-lines > myimage.jpg`

Example: `render list 800 600 42 > myimage.jpg`.  This example draws 42 random,
straight lines into an 800 by 600 image and writes it out as a JPEG image.

NB: The arguments must be provided in the correct order, as above.  (Might
investigate a getopts style package to improve this with named parameters and so
on.)

## Running Stats ##

TODO: Put some sample execution times with various parameters here.
