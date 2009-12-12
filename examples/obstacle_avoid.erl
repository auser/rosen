%
% obstacle_avoid.erl
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
% $Id: obstacle_avoid.erl,v 1.1 2008/03/27 15:34:26 corrado_santoro Exp $
%
-module (obstacle_avoid).

-include ("geometry.hrl").
-include ("robot.hrl").

-behaviour(gen_activity).

-export ([init/2,
          step/5,
          terminate/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: step/5
%%====================================================================
step (ObjectPid, ObjectState, Time, DeltaTime, {Robot}) ->
  Sensors = robot:get_sensors (Robot),
  DistanceLeft = proplists:get_value (distance_left, Sensors),
  DistanceRight = proplists:get_value (distance_right, Sensors),
  Wall = under_threshold (DistanceLeft, DistanceRight, ?CM(20)),
  if
    Wall ->
      Omega =
        if
          DistanceRight == DistanceLeft ->
            ?DEG_S (5);
          true ->
            (20.0 - (DistanceRight - DistanceLeft)) * 3.0
        end,
      V = 0;
    true ->
      Omega = 0,
      V = ?CM_S (30)
  end,
  robot:command (Robot, {set_speed, V, Omega}),
  {ok, {Robot}}.



under_threshold (L, R, D) when L < D; R < D -> true;
under_threshold (_, _, _) -> false.


%%====================================================================
%% Func: init/2
%%====================================================================
init (Object, Properties) ->
  Robot = proplists:get_value (robot, Properties),

  {ok, {Robot}}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, State) ->
  ok.

