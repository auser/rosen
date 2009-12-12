%
% pipe.erl
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
% $Id: pipe.erl,v 1.4 2008/02/27 15:29:13 corrado_santoro Exp $
%
%%
%% @doc The module implementing a 3D pipe, that is a cylinder without
%% the two bases.
%%
%% <p>A pipe is created using the
%% function <code>oject3d:new/1</code>, passing the proper
%% <code>#object3d{}</code>
%% record, whose fields have the following meaning:</p>
%% <ul>
%% <li><code>type</code>, must be set equal to atom <code>pipe</code>.</li>
%% <li><code>name</code>, a name assigned to the process handling
%%     this pipe (set it only if you want to register the process).</li>
%% <li><code>position</code>, a <code>#vector{} </code>representing the
%%     <i>(x,y,z)</i> position of the center of the <b>base</b> of the
%%     pipe (use the
%%     <code>?VECTOR(X,Y,Z)</code> macro defined in <code>geometry.hrl</code>).
%%     </li>
%% <li><code>axis</code>, a <code>#vector{}</code> representing the orientation
%%     of the pipe length (height).
%%     The default is along the <i>z</i> axis.</li>
%% <li><code>up</code>, a <code>#vector{}</code> representing the orientation
%%     of the up vector. The default is along the <i>y</i> axis.</li>
%% <li><code>color</code>, the pipe's color, expressed in RGB using
%%     macro <code>?RGB(R,G,B)</code> defined in <code>geometry.hrl</code>.
%%     </li>
%% <li><code>radius</code>, the radius of the base of the pipe.</li>
%% <li><code>size</code>, pipe's height.</li>
%% </ul>
%%
%% <p>Example:</p>
%% <pre>
%%  object3d:new (#object3d { type = pipe,
%%                            name = mypipe,
%%                            radius = 0.1,
%%                            size = 0.75,
%%                            position = ?VECTOR (0.0, -0.1, 0.6),
%%                            axis = ?VECTOR (0.0, 1.0, 1.0),
%%                            color = ?RGB(1.0, 1.0, 0.0)}).
%% </pre>
%%
%
-module (pipe).
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
%%====================================================================
%% @private
draw (Obj) ->
  if
    size (Obj#object3d.color) == 3 ->
      gl:color3fv (Obj#object3d.color);
    true ->
      gl:color4fv (Obj#object3d.color)
  end,
  glu:cylinder (Obj#object3d.quad,
                Obj#object3d.radius,
                Obj#object3d.radius,
                Obj#object3d.size,
                32,
                32),
  {ok, Obj}.

%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
init (Obj) ->
  {ok, object3d:copy_default_axis (
         Obj#object3d { quad = glu:newQuadric (),
                        default_axis = ?VECTOR (0.0, 0.0, 1.0) })}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
%% @private
terminate (_, Obj) ->
  glu:deleteQuadric (Obj#object3d.quad),
  ok.

