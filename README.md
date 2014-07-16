calisson-hs
===========

Calissons with Diagrams and Yesod.

Torward a deeper understanding of the [Calisson Tiling theorem](http://gurmeet.net/puzzles/tiling-with-calissons/), and a deeper understanding of Haskell programming.

This program is a webapp that draws a hexagon with triangular grid, and then tiles the grid with diamond tiles (two triangles connected along an edge of each). The tiles are red, green, and blue, and all tiles of one color share the same orientation (angle). 

The Calisson Tiling theorem states that any such tiling must have the same number of red, green, and blue tiles. The image of a tiling (example below) hints at the reason. Look at the image with a 3-D perspective (remember Q*Bert?); see how a tiling resembles a neat pile of cube blocks. All the cubes have the same red color on top (similarly, they all have green left side and blue right side), and every visible red-top is the top of one "tower" of cubes. All tilings of the grid will show the same number of towers, with varying heights, and all will have a red tile atop each tower.

By symmetry (rotate to the left, or to the right), there are the same number of green "sideways" towers, and the same number of blue sideways towers.

The app serves the image as an SVG file.

![tiling-6-1](https://cloud.githubusercontent.com/assets/656964/3594894/1d8e2e80-0ca9-11e4-9178-346cbfc6de92.png)

![tiling-6-120](https://cloud.githubusercontent.com/assets/656964/3594892/1d5f4e8a-0ca9-11e4-9951-a6f4976fcb34.png)

![tiling-10-500](https://cloud.githubusercontent.com/assets/656964/3594893/1d8d6ab8-0ca9-11e4-8203-513c89095e23.png)
