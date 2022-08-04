## About

PDO tools extracts and converts data from Pepakura PDO format.

While the Pepakura Designer is a decent unfolding tool, the vector format exports provided by it are quite limited and not really suitable for further editing:

* every line segment is stored as a separate object - for example dashed lines are broken into multiple separate paths
* only the part outlines are exported, so you can't simply click a part and recolor it.
* the textures are not exported at all. If your model has textures, you're out of luck.
* vector exports are not available in Pepakura Viewer. Which makes things difficult if you want to customize some model not made by you.

In addition, Pepakura is only available on Windows.

PDO tools offers much more useful 2D vector export into SVG files (which are easily editable by a vector editor, for example Inkscape):

* part outlines are used to create a single filled object for each part. Easy part manipulation.
* the unfolded triangles and quads from the 3D objects are exported as well. You can rearrange them to modify how the parts were unfolded
* textures (and solid fills) are exported as clipped bitmaps. Each part has its own bitmap - redraw any individual part as you want.

It works not only on Windows, but on Linux too. It would probably work on MacOS X with some modifications, but I don't have a dev/testing environment for it.

Due to the PDO format not being open the file data interpretation is mostly made by guesswork. Therefore there are some issues:

* the tab shape is wrong in some cases
* the line types don't always match

Other limitations:

* edge numbers are not exported yet
* non-english texts in PDO files often don't have the proper locale set, so weird characters may appear in output. This is a limitation of the format itself


## Compilation

You will need recent stable Lazarus (>= 2.0) with Freepascal (>= 3.2.0) to compile the sources. Older versions may work, but are untested.
There are 2 Lazarus project files in solution:

* pdotools.lpi - pdo tools gui
* pdodump.lpi - command line pdo analyser / exporter

To compile from Lazarus, open the desired project and from Lazarus menu pick Run -> Build.
To compile from command line, use lazbuild:

* lazbuild --bm=Release pdotools.lpi

Windows and Linux targets are supported. MacOS 10.6 and higher should work as well, but it's untested.
Libharu is requied for PDF export, either as libhpdf.dll on Windows or libharu package on Linux (static linking is used by default, unless changed in hpdf.pas - remove USE_STATIC define).


## TODO

* replace textures for exporting
* optionally export edge numbers


## Issues

If your file doesn't work properly, create a new issue and attach a sample. Alternatively, there's a support thread at [Papermodelers.com](http://www.papermodelers.com/forum/software/24448-pdo-tools-format-discussion.html)