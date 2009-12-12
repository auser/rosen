%
% all_balls.erl
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
% $Id: all_balls.erl,v 1.4 2007/11/23 11:37:05 corrado_santoro Exp $
%
-module (all_balls).
-export ([go/0]).

-behaviour (gen_activity).

-include ("geometry.hrl").

-export ([init/2,
          step/5,
          terminate/2]).

-define (SIZE, 30.0).
-define (BALLS, 50).

go () ->
  rosen:start_link(),
  rosen:zoom (-35),
  rosen:up (15),
  object3d:new (#object3d { type = surface,
                            name = mybox,
                            width = ?SIZE + 4.0,
                            depth = ?SIZE + 4.0,
                            position = ?VECTOR (0.0, 0.0, 0.0),
                            color = ?RGB(0.25,0.25,0.25)}),

  lists:foreach (
    fun (_) ->
        X = ?SIZE/2 - random:uniform (?SIZE),
        Y = 1.0 + random:uniform (?SIZE / 10.0),
        Z = ?SIZE/2 - random:uniform (?SIZE),
        R = random:uniform (100) / 100.0,
        G = random:uniform (100) / 100.0,
        B = random:uniform (100) / 100.0,

        SP = #object3d { type = sphere,
                         radius = 0.2,
                         position = ?VECTOR (X, Y, Z),
                         color = ?RGB (R, G, B)},

        {ok, Pid} = object3d:new (SP),

        object3d:add_activity (Pid, all_balls, {random:uniform (20), X, Y})
    end,
    lists:seq (1, ?BALLS)).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: step/5
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
    ((Y - 0.2) < 0) and (VY < 0) and (X < 15.0) ->
      NewAngle = geometry:to_degrees (math:atan2 (-VY, VX)),
      object3d:position (ObjectPid, #vector { x = X, y = 0.2, z = Z}),
      NewV = math:sqrt(VX * VX + VY * VY) * 0.8,
%%       io:format ("Collision speed ~p,~p, ~p, ~p~n",
%%                  [VX, VY, NewAngle, NewV]),
      {ok, {NewAngle, 0.01, NewV, X, 0.2}};
    true ->
      object3d:position (ObjectPid, #vector { x = X, y = Y, z = Z}),
      {ok, {Angle, T + 0.01, V0, X0, Y0}}
  end.


%%====================================================================
%% Func: init/2
%%====================================================================
init (_, {V0, X0, Y0}) ->
  {ok, {-90.0, 0, V0, X0, Y0}}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, State) ->
  ok.

