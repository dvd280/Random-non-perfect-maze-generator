###Random non perfect Maze generator in R ###

This project features an algorithm I devised for creating random mazes of sizes up to 100X100 in R. 
The mazes generated by this code will be represented by a matrix, where:

-Wall locations get a negative value of -2 

-Potential paths get a value of 0

-The mazes will not be "tree mazes", this means that multiple paths might lead to the same point in the maze. 

-Any point with a 0 value is accessible from any other point which has a 0 value attached to it.

### some more information about this implementation ###

This algorithm implements a recursion with a "jumping" mechanism, meaning that nodes are pushed onto the stack only
when a 3 or 4 way fork is created in the maze path. This is done to limit the size of the stack by avoiding stacking
calls which include nodes that are essentially corridors in the maze ( connected to only 1 or 2 other path nodes).

Another optionality in this code is to save a snapshot of the generation process to the current directory.
To turn on this option, users will need to use setwd() to determine where they want images to be saved. And then remove the 
commented parts inside the first two lines of he main loop in the " traverse_maze " function.
*Warning* : Using the snapshot option will slow the process considerably as snapshots are taken for every step in the loop
 using the external packag  "plotly" and then exported using the "orca" package

### External packages used ###
dplyr
plotly
orca



