Render
======

## Introduction ##

This program rasterizes randomly created lines into an image, and writes the
result to a file in PNG format, intended as a demo of how to do mutable, C-style
arrays in Haskell.

The size of the image, and number of lines drawn are controlled via
arguments.  An argument also controls what data structure is used to store the
image, as each random line is rasterized into it.  Currently, the choice is
between "array" and "list".  Type "list" means the program just uses the
standard Haskell list (actually a list of lists) to store the image.  Type
"array" means an IOUArray is used.

As you might expect, drawing pixels into an image as a list is very slow, and
the array version is much faster (and much less insane).

## Example Output ##

![10 white lines in a sea of black](/sample.png "10 white lines in a sea of black")

This picture was generated via the command:

    render array 400 300 10 sample.png

## Building and Running ##

This project uses [stack](https://docs.haskellstack.org/en/stable/README/).
Once stack is installed, run the following commands to build the project:

1. `stack setup`
2. `stack build`
3. `stack install` (Optional: This will copy the executable somewhere on your PATH.)

If you perfomed step 3, you can then simply run `render` to launch (or
`render.exe` for windows).

Otherwise, a convenient way to run this thing is something like `stack exec
render <args>`.

See below for an explanation of the expected arguments.

## Arguments / Usage ##

Usage: `render {list|array} width height number-of-lines output-filename`

The first argument is the internal data structure for the image the program will
use.  A value of `list` uses regular Haskell lists; a value of `array` uses an
IOUArray from the the Data.Array.IO package.

The next two args specify the image dimensions: width then height.

The fourth is the number of lines to draw.  As mentioned above, lines are created
randomly, so the image produced will look different from run to run.

The final argument is the name of the PNG file to write out.

Example: `render list 800 600 42 myimage.png`.  This draws 42 random, straight
lines into an 800 by 600 image and writes it out as a PNG file named
`myimage.png`.

NB: The arguments must be provided in the correct order, as above.  Named
parameters in different orders do not work currently.

## Running Stats ##

On one machine, running the list version takes about 24 seconds:

    PS C:\Users\njsand\Documents\render> Measure-Command {render list 1920 1200 150 list.png}
    
    Days              : 0
    Hours             : 0
    Minutes           : 0
    Seconds           : 24
    Milliseconds      : 820

On the same machine, the array version is about 360 milliseconds.  As expected
this is way faster.

    PS C:\Users\njsand\Documents\render> Measure-Command {render array 1920 1200 150 array.png}

    Days              : 0
    Hours             : 0
    Minutes           : 0
    Seconds           : 0
    Milliseconds      : 364
