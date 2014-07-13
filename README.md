calisson-hs
===========

Calissons with Diagrams and Yesod.

Torward a deeper understanding of the [Calisson Tiling theorem](http://gurmeet.net/puzzles/tiling-with-calissons/), and a deeper understanding of Haskell programming.

This program is a webapp that draws a hexagon with triangular grid, and then tiles the grid with diamond tiles (two triangles connected along an edge of each). The tiles are red, green, and blue, and all tiles of one color share the same orientation (angle). 

The Calisson Tiling theorem states that any such tiling must have the same number of red, green, and blue tiles. The image of a tiling (example below) hints at the reason. Look at the image with a 3-D perspective (remember Q*Bert?); see how a tiling resembles a neat pile of cube blocks. All the cubes have the same red color on top (similarly, they all have green left side and blue right side), and every visible red-top is the top of one "tower" of cubes. All tilings of the grid will show the same number of towers, with varying heights, and all will have a red tile atop each tower.

By symmetry (rotate to the left, or to the right), there are the same number of green "sideways" towers, and the same number of blue sideways towers.

The app serves the image as an SVG file.


![tiling-6-1](https://cloud.githubusercontent.com/assets/656964/3562699/dc28dd74-09ff-11e4-9ede-acaa34097f1e.png)

![tiling-6-1](https://cloud.githubusercontent.com/assets/656964/3564223/d87a5cda-0a76-11e4-8906-8d78c9a47857.png)

![tiling-15](https://cloud.githubusercontent.com/assets/656964/3564273/6d78c134-0a7a-11e4-8501-ecb9662f951f.png)
