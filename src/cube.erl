%
% cube.erl
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
%
% $Id: cube.erl,v 1.4 2007/11/04 17:28:49 corrado_santoro Exp $
%
%%
%% @doc The module implementing a 3D cube.
%%
%% <p>A cube is created using the
%% function <code>oject3d:new/1</code>, passing the proper
%% <code>#object3d{}</code>
%% record, whose fields have the following meaning:</p>
%% <ul>
%% <li><code>type</code>, must be set equal to atom <code>cube</code>.</li>
%% <li><code>name</code>, a name assigned to the process handling
%%     this cube (set it only if you want to register the process).</li>
%% <li><code>position</code>, a <code>#vector{} </code>representing the
%%     <i>(x,y,z)</i> position of the center of the cube (use the
%%     <code>?VECTOR(X,Y,Z)</code> macro defined in <code>geometry.hrl</code>).
%%     </li>
%% <li><code>axis</code>, a <code>#vector{}</code> representing the orientation
%%     of the cube axis. The default is along the <i>z</i> axis.</li>
%% <li><code>up</code>, a <code>#vector{}</code> representing the orientation
%%     of the up vector. The default is along the <i>y</i> axis.</li>
%% <li><code>color</code>, the cube's color, expressed in RGB using
%%     macro <code>?RGB(R,G,B)</code> defined in <code>geometry.hrl</code>.
%%     </li>
%% <li><code>size</code>, the size of the cube's edge.</li>
%% </ul>
%%
%% <p>Example:</p>
%% <pre>
%%  object3d:new (#object3d { type = cube,
%%                            name = mycube,
%%                            size = 2.0,
%%                            position = ?VECTOR (0.0, -0.1, 0.6),
%%                            axis = ?VECTOR (0.0, 1.0, 1.0),
%%                            color = ?RGB(1.0, 1.0, 0.0)}).
%% </pre>
%%
-module (cube).
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
draw (TheCube) ->
  HS = TheCube#object3d.size / 2.0,

  Cube = {{ HS,  HS, -HS},
          { HS, -HS, -HS},
          {-HS, -HS, -HS},
          {-HS,  HS, -HS},
          {-HS,  HS,  HS},
          { HS,  HS,  HS},
          { HS, -HS,  HS},
          {-HS, -HS,  HS}},

%%   Colors = {{ 1.0,  1.0,  0.0},
%%             { 1.0,  0.0,  0.0},
%%             { 0.0,  0.0,  0.0},
%%             { 0.0,  1.0,  0.0},
%%             { 0.0,  1.0,  1.0},
%%             { 1.0,  1.0,  1.0},
%%             { 1.0,  0.0,  1.0},
%%             { 0.0,  0.0,  1.0}},

  Colors = { TheCube#object3d.color,
             TheCube#object3d.color,
             TheCube#object3d.color,
             TheCube#object3d.color,
             TheCube#object3d.color,
             TheCube#object3d.color,
             TheCube#object3d.color,
             TheCube#object3d.color },

  gl:glBegin(?GL_QUADS),

  gl:color3fv(element(1, Colors)),
  gl:vertex3fv(element(1, Cube)),
  gl:color3fv(element(2, Colors)),
  gl:vertex3fv(element(2, Cube)),
  gl:color3fv(element(3, Colors)),
  gl:vertex3fv(element(3, Cube)),
  gl:color3fv(element(4, Colors)),
  gl:vertex3fv(element(4, Cube)),

  gl:color3fv(element(4, Colors)),
  gl:vertex3fv(element(4, Cube)),
  gl:color3fv(element(5, Colors)),
  gl:vertex3fv(element(5, Cube)),
  gl:color3fv(element(8, Colors)),
  gl:vertex3fv(element(8, Cube)),
  gl:color3fv(element(3, Colors)),
  gl:vertex3fv(element(3, Cube)),

  gl:color3fv(element(1, Colors)),
  gl:vertex3fv(element(1, Cube)),
  gl:color3fv(element(6, Colors)),
  gl:vertex3fv(element(6, Cube)),
  gl:color3fv(element(7, Colors)),
  gl:vertex3fv(element(7, Cube)),
  gl:color3fv(element(2, Colors)),
  gl:vertex3fv(element(2, Cube)),

  gl:color3fv(element(6, Colors)),
  gl:vertex3fv(element(6, Cube)),
  gl:color3fv(element(5, Colors)),
  gl:vertex3fv(element(5, Cube)),
  gl:color3fv(element(8, Colors)),
  gl:vertex3fv(element(8, Cube)),
  gl:color3fv(element(7, Colors)),
  gl:vertex3fv(element(7, Cube)),

  gl:color3fv(element(6, Colors)),
  gl:vertex3fv(element(6, Cube)),
  gl:color3fv(element(1, Colors)),
  gl:vertex3fv(element(1, Cube)),
  gl:color3fv(element(4, Colors)),
  gl:vertex3fv(element(4, Cube)),
  gl:color3fv(element(5, Colors)),
  gl:vertex3fv(element(5, Cube)),

  gl:color3fv(element(7, Colors)),
  gl:vertex3fv(element(7, Cube)),
  gl:color3fv(element(2, Colors)),
  gl:vertex3fv(element(2, Cube)),
  gl:color3fv(element(3, Colors)),
  gl:vertex3fv(element(3, Cube)),
  gl:color3fv(element(8, Colors)),
  gl:vertex3fv(element(8, Cube)),

  gl:glEnd(),

  {ok, TheCube}.

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

