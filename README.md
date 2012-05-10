WHO-maps
========

A few functions for easily creating maps in R that are aligned with official boundaries.

Here are a few quick notes on my methodolgy.

- There are a lot of ways to make maps, but I happen to make one type way more frequently than any other. These functions make it easy to make those maps and can be a springboard for making more custom versions.
- The maps shouldn't change too frequently, but they will and these scripts will need to be updated. Here has been my method for aligning disputed borders to the technical specifications of WHO legal:
  - Import the main country shapes, the additional polygons and the additional lines
  - Open up the polygons file to see how each polygon should be colored (e.g. lakes blue, Jammu Kashmir gray).
  - Plot the lines and try and figure out which line goes to which part of the map. When you figure out the IDs that go with each line type, you can plot them as solid, dashed, dotted, white, or black accordingly. These lines should be checked any time the shape files change as the IDs might be different
 
That's it! Hopefully I'll be able to maintain these for awhile to come, but if not, now you know how to update them.

Tom