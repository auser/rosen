
ROSEN: the RObotic Simulation Erlang eNgine
!!!!!!  How to compile and test it  !!!!!!!
===========================================


1. Compiling
------------

First of all you need erlang and esdl.

Erlang can be found at http://www.erlang.org
ESDL can be found at http://esdl.sourceforge.net

After unpacking the tarball, type:

$ ./configure

and then

$ make

This will compile ROSEN and the examples. The binaries of ROSEN can
be found in the "ebin" dir, while, for the examples, you can find both
source code and binaries in "examples".


2. Testing the library
----------------------

Type:

$ erl -pa ebin -pa examples

and then, from the erlang shell:

1> test:objects().

If everything is ok, you will see some 3D objects (a cube, a cylinder
a cone, a shpere, etc.) of various colors rotating in the 3D space.
Press "q" to terminate the program and return to the shell.


3. Moving the scene
-------------------

While looking a 3D scene, you may use the following keys to move it:

"1"                 --> Zoom out
"2"                 --> Zoom in
"Left/Right Arrow"  --> Rotate left/right the scene
"Up/Down" Arrow"    --> Rotate up/down the scene
"c"                 --> Show camera position
"ESC" or "q"        --> Close the 3D window and terminate ROSEN


4. Problems?
------------

Please drop a mail to Corrado Santoro (csanto@diit.unict.it).

