%
% balls.erl
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
% $Id: bounce.erl,v 1.5 2007/12/10 09:36:32 corrado_santoro Exp $
%
-module (bounce).
-export ([go/0]).

-include ("geometry.hrl").

-behaviour (gen_activity).

-export ([init/2,
          step/5,
          terminate/2]).

go () ->
  rosen:start_link(),
  rosen:zoom (-20),
  rosen:up (30),
  object3d:new (#object3d { type = surface,
                            name = mybox,
                            width = 30.0,
                            depth = 8.0,
                            position = ?VECTOR (0.0, 0.0, 0.0),
                            color = ?RGB(0.25,0.25,0.25)}),

  object3d:new (#object3d { type = sphere,
                            name = ball1,
                            radius = 0.2,
                            position = ?VECTOR (-14.0, 3.0, 1.0),
                            color = ?RGB (1.0, 0.25, 0.25)}),

  object3d:new (#object3d { type = sphere,
                            name = ball2,
                            radius = 0.2,
                            position = ?VECTOR (-14.0, 3.0, -1.0),
                            color = ?RGB (0.25, 1.0, 0.25)}),

  object3d:add_activity (ball1, bounce, {1.5, -14.0, 3.0}),

  object3d:add_activity (ball2, bounce, {1.0, -14.0, 4.0}),

  ok.


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: step/3
%%====================================================================
step (ObjectPid, ObjectState, Time, DeltaTime, {Angle, T, V0, X0, Y0}) ->
  X = V0 * math:cos (geometry:to_radiants (Angle)) * T + X0,
  Y = V0 * math:sin (geometry:to_radiants (Angle)) * T + Y0 -
    (9.81 * T * T) / 2.0,
  Z = (ObjectState#object3d.position)#vector.z,

%%   io:format ("~p,~p~n", [X, Y]),

  VX = V0 * math:cos (geometry:to_radiants (Angle)),
  VY = V0 * math:sin (geometry:to_radiants (Angle)) - 9.81 * T,
  if
    ((Y - 0.1) < 0) and (VY < 0) and (X < 15.0) ->
      NewAngle = geometry:to_degrees (math:atan2 (-VY, VX)),
      object3d:position (ObjectPid, #vector { x = X, y = 0.1, z = Z}),
      NewV = math:sqrt(VX * VX + VY * VY),
%%       io:format ("Collision speed ~p,~p, ~p, ~p~n",
%%                  [VX, VY, NewAngle, NewV]),
      {ok, {NewAngle, 0.01, NewV, X, 0.1}};
    true ->
      object3d:position (ObjectPid, #vector { x = X, y = Y, z = Z}),
      {ok, {Angle, T + 0.01, V0, X0, Y0}}
  end.


%%====================================================================
%% Func: init/1
%%====================================================================
init (_, {V0, X0, Y0}) ->
  {ok, {0.0, 0, V0, X0, Y0}}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, State) ->
  ok.

