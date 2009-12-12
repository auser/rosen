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
% $Id: compound.erl,v 1.6 2008/02/29 15:44:10 corrado_santoro Exp $
%
%%
%% @doc The module implementing a compound object, that is a rigid object
%% which is composed of other objects.
%%
%% <p>A compound is created using the
%% function <code>oject3d:new/1</code>, passing the proper
%% <code>#object3d{}</code>
%% record, whose fields have the following meaning:</p>
%% <ul>
%% <li><code>type</code>, must be set equal to atom <code>compound</code>.</li>
%% <li><code>name</code>, a name assigned to the process handling
%%     this compound (set it only if you want to register the process).</li>
%% <li><code>position</code>, a <code>#vector{}</code> representing the
%%     <i>(x,y,z)</i> position of the center of the compound (use the
%%     <code>?VECTOR(X,Y,Z)</code> macro defined in <code>geometry.hrl</code>).
%%     </li>
%% <li><code>axis</code>, a <code>#vector{}</code> representing the orientation
%%     of the compound.
%%     The default is along the <i>z</i> axis.</li>
%% <li><code>up</code>, a <code>#vector{}</code> representing the orientation
%%     of the up vector. The default is along the <i>y</i> axis.</li>
%% <li><code>objects</code>, a list of <code>#object3d{}</code> records,
%%     representing the objects the compound is composed of.</li>
%% </ul>
%%
%% Sub-object's position and axis are considered <b>relative</b> with
%% respect to the compound.
%%
%% <p>Example:</p>
%% <pre>
%%  C1 = #object3d { type = sphere,
%%                   radius = 0.095,
%%                   position = ?VECTOR (X1, Y1, 0),
%%                   color = ?RGB (1.0, 0.0, 0.0)},
%%
%%  C2 = #object3d { type = pipe,
%%                   size = 0.1,
%%                   radius = 0.41,
%%                   position = ?VECTOR (0.0, 0.0, -0.05)},
%%
%%  object3d:new (#object3d { type = compound,
%%                            name = mycompound,
%%                            position = ?VECTOR (0.0, -0.1, 0.6),
%%                            objects = [C1, C2] }).
%% </pre>
%%
%
-module (compound).
-behaviour (object3d).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").

-include("geometry.hrl").

-export ([init/1,
          start_component/2,
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
  {ok, Obj}.

%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
init (Obj) ->
  Obj2 = object3d:copy_default_axis (
           Obj#object3d { default_axis = ?VECTOR (0.0, 0.0, 1.0) }),
  Compound = object3d:copy_default_up (
               Obj2#object3d { default_up = ?VECTOR (0.0, 1.0, 0.0) }),

  Pids =
    lists:map (
      fun (O) ->
          start_component (Compound, O)
      end,
      Compound#object3d.objects),
  %%object3d:pids (self(), Pids),

  {ok, Compound#object3d { pids = Pids} }.



%%====================================================================
%% Func: start_component/2
%%====================================================================
%% @private
start_component (Compound, ComponentObject) ->
  Name =
    if
      (ComponentObject#object3d.name =/= noname) and
      (Compound#object3d.name =/= noname) ->
        list_to_atom (atom_to_list (Compound#object3d.name)
                      ++ "_" ++
                      atom_to_list (ComponentObject#object3d.name));
      true ->
        noname
    end,
  {ok, P}  =
    object3d:new (ComponentObject#object3d { name = Name,
                                             parent_object = self () } ),
  P.


%%====================================================================
%% Func: terminate/2
%%====================================================================
%% @private
terminate (_, _) ->
  ok.

