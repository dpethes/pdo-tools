## Changelog

### v0.10 (x.y.2022)

* improved support for Pepakura version 4 and higher
* option to merge tabs with outlines https://github.com/dpethes/pdo-tools-v2/issues/3

### v0.9 (25.1.2018)

* allow exporting each page to separate file
* SVG: set proper page size
* allow changing the text encoding (some pdo files use wrong encoding)

### v0.8 (24.7.2016)

* support new Pepakura version 4
* improved texture quality
* fixed reading of files without unfolded data

### v0.7 (3.1.2016)

* tab clipping - if any tab of a part overlaps its outline, it is downsized so no overlap remains
* OBJ: export groups
* PDF: respect pdo page orientation
* fixed outline shape for some parts
* faster PDO loading if PDO contains a lot of textures
* sligtly faster SVG export

### v0.6 (24.7.2015)

* extended fold line option: fold line is not drawn over the part's texture, but extends from the part's edges
* PDF: use dashed line for valley folds and solid line for mountain folds
* improved fold line handling: don't export hidden fold lines, hide almost flat lines
* much faster PDO loading
* improved SVG and PDF export speed and texture precision
* improved 'Save as...' file naming/filters
* warn if libhpdf cannot be found
* fixed minor pixel-sized artifacts in textures with white areas
* fixed non-default page margins in PDF export
* fixed rare texture bug

### v0.5 (10.1.2015)

* new PDF export option (uses Haru/libhpdf)
* change line and image attributes in SVG files for better compatibility with new versions of Inkscape
* fixed issue with some pdo files with imported images

### v0.4 (7.1.2015)

* DPI for each part is set individually according to the part's size and its texture resolution
* improved SVG export speed
* export fold lines. Hiding by angle is not yet supported
* fixed crash when unfolded face has more than 4 vertices
* fixed rare issue with broken textures in exported SVG files

### v0.3 (17.11.2014)

* SVG: improved texture precision
* SVG: much faster export, multiple CPU cores utilization
* change DPI scale, set 125 as default

### v0.2 (27.10.2014)

* tab export
* improved reading of material color

### v0.1 (10.8.2014)

* SVG: improved text export - better positioning, multiline text
* SVG: fixed opening of SVG files with japanese characters in Inkscape
* SVG: parts (junk) that are positioned out of pattern pages (left/top only) are not exported anymore
* improved texture mapping precision
* fixed issues with extremely small textures
* slightly faster SVG export

### r164 (12.6.2014)
Many SVG export improvements.

* SVG: improved part outlining
* SVG: added smooth texture filtering
* much faster SVG export
* fixed color of materials with no texture
* OBJ: normalize to unit size = 2

### r114 (24.2.2014)

* texture wrapping (repeated texture) support
* material color support (for untextured faces)
* SVG: fix unescaped 'less than'/'greater than' characters in text export

### r104 (22.2.2014)

* SVG: texture export. No texture filtering currently. Textures are stored in separate layer group.
* SVG: part outline export. Outlines are stored in separate layer group.
* SVG: initial Text export support. Multiline texts and font sizes aren't handled properly
* SVG: image export.
* new icon ;)

### r57 (19.5.2013)

* linux builds for QT and GTK frameworks
* Wavefront OBJ export
