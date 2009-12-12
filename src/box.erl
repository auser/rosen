%
% box.erl
%
% ----------------------------------------------------------------------
%
%  ROSEN, a RObotic Simulation Erlang eNgine
%  Copyright (C) 2007 Corrado Santoro (csanto@diit.unict.it)
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%
% $Id: box.erl,v 1.4 2008/02/27 15:29:13 corrado_santoro Exp $
%
%%
%% @doc The module implementing a 3D box.
%%
%% <p>A box is created using the
%% function <code>oject3d:new/1</code>, passing the proper
%% <code>#object3d{}</code>
%% record, whose fields have the following meaning:</p>
%% <ul>
%% <li><code>type</code>, must be set equal to atom <code>box</code>.</li>
%% <li><code>name</code>, a name assigned to the process handling
%%     this box (set it only if you want to register the process).</li>
%% <li><code>position</code>, a <code>#vector{} </code>representing the
%%     <i>(x,y,z)</i> position of the center of the box (use the
%%     <code>?VECTOR(X,Y,Z)</code> macro defined in <code>geometry.hrl</code>).
%%     </li>
%% <li><code>axis</code>, a <code>#vector{}</code> representing the orientation
%%     of the box axis. The default is along the <i>z</i> axis.</li>
%% <li><code>up</code>, a <code>#vector{}</code> representing the orientation
%%     of the up vector. The default is along the <i>y</i> axis.</li>
%% <li><code>color</code>, the box's color, expressed in RGB using
%%     macro <code>?RGB(R,G,B)</code> defined in <code>geometry.hrl</code>.
%%     </li>
%% <li><code>width</code>, box's width.</li>
%% <li><code>height</code>, box's height.</li>
%% <li><code>depth</code>, box's depth.</li>
%% </ul>
%%
%% <p>Example:</p>
%% <pre>
%%  object3d:new (#object3d { type = box,
%%                            name = mybox,
%%                            width = 0.1,
%%                            height = 0.75,
%%                            depth = 21.1,
%%                            position = ?VECTOR (0.0, -0.1, 0.6),
%%                            axis = ?VECTOR (0.0, 1.0, 1.0),
%%                            color = ?RGB(1.0, 1.0, 0.0)}).
%% </pre>
%%
%
-module (box).
-behaviour (object3d).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").

-include("geometry.hrl").

-export ([init/1,
          draw/1,
          terminate/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: draw/1
%% @private
%%====================================================================
draw (TheBox) ->
  W = TheBox#object3d.width / 2.0,
  H = TheBox#object3d.height / 2.0,
  D = TheBox#object3d.depth / 2.0,

  Box = {{ W,  H, -D},
         { W, -H, -D},
         {-W, -H, -D},
         {-W,  H, -D},
         {-W,  H,  D},
         { W,  H,  D},
         { W, -H,  D},
         {-W, -H,  D}},

%%   Colors = {{ 1.0,  1.0,  0.0},
%%             { 1.0,  0.0,  0.0},
%%             { 0.0,  0.0,  0.0},
%%             { 0.0,  1.0,  0.0},
%%             { 0.0,  1.0,  1.0},
%%             { 1.0,  1.0,  1.0},
%%             { 1.0,  0.0,  1.0},
%%             { 0.0,  0.0,  1.0}},

  Colors = { TheBox#object3d.color,
             TheBox#object3d.color,
             TheBox#object3d.color,
             TheBox#object3d.color,
             TheBox#object3d.color,
             TheBox#object3d.color,
             TheBox#object3d.color,
             TheBox#object3d.color },

  gl:glBegin(?GL_QUADS),

  rosen:color(element(1, Colors)),
  gl:vertex3fv(element(1, Box)),
  rosen:color(element(2, Colors)),
  gl:vertex3fv(element(2, Box)),
  rosen:color(element(3, Colors)),
  gl:vertex3fv(element(3, Box)),
  rosen:color(element(4, Colors)),
  gl:vertex3fv(element(4, Box)),

  rosen:color(element(4, Colors)),
  gl:vertex3fv(element(4, Box)),
  rosen:color(element(5, Colors)),
  gl:vertex3fv(element(5, Box)),
  rosen:color(element(8, Colors)),
  gl:vertex3fv(element(8, Box)),
  rosen:color(element(3, Colors)),
  gl:vertex3fv(element(3, Box)),

  rosen:color(element(1, Colors)),
  gl:vertex3fv(element(1, Box)),
  rosen:color(element(6, Colors)),
  gl:vertex3fv(element(6, Box)),
  rosen:color(element(7, Colors)),
  gl:vertex3fv(element(7, Box)),
  rosen:color(element(2, Colors)),
  gl:vertex3fv(element(2, Box)),

  rosen:color(element(6, Colors)),
  gl:vertex3fv(element(6, Box)),
  rosen:color(element(5, Colors)),
  gl:vertex3fv(element(5, Box)),
  rosen:color(element(8, Colors)),
  gl:vertex3fv(element(8, Box)),
  rosen:color(element(7, Colors)),
  gl:vertex3fv(element(7, Box)),

  rosen:color(element(6, Colors)),
  gl:vertex3fv(element(6, Box)),
  rosen:color(element(1, Colors)),
  gl:vertex3fv(element(1, Box)),
  rosen:color(element(4, Colors)),
  gl:vertex3fv(element(4, Box)),
  rosen:color(element(5, Colors)),
  gl:vertex3fv(element(5, Box)),

  rosen:color(element(7, Colors)),
  gl:vertex3fv(element(7, Box)),
  rosen:color(element(2, Colors)),
  gl:vertex3fv(element(2, Box)),
  rosen:color(element(3, Colors)),
  gl:vertex3fv(element(3, Box)),
  rosen:color(element(8, Colors)),
  gl:vertex3fv(element(8, Box)),

  gl:glEnd(),

  {ok, TheBox}.

%%====================================================================
%% Func: init/1
%% @private
%%====================================================================
init (Obj) ->
  O = object3d:copy_default_axis (
        Obj#object3d { default_axis = ?VECTOR (0.0, 0.0, 1.0) }),
  O2 = object3d:copy_default_up (
         O#object3d { default_up = ?VECTOR (0.0, 1.0, 0.0) }),
  {ok, O2}.


%%====================================================================
%% Func: terminate/2
%% @private
%%====================================================================
terminate (_, _) ->
  ok.

