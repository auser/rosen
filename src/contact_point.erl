%
% contact_point.erl
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
% $Id: contact_point.erl,v 1.5 2008/02/29 15:44:10 corrado_santoro Exp $
%
-module (contact_point).

-include ("geometry.hrl").
-include ("robot.hrl").

-behaviour (gen_activity).

-export ([init/2, step/5, model/2, terminate/2]).

-define (DYNAMICS, 10).

%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: model/2
%%====================================================================
%%
model (Position, Color) ->
  X = Position#vector.x,
  Y = Position#vector.y,
  Z = Position#vector.z,
  #object3d { type = cube,
              size = ?CM(1),
              position = world:world_to_3d (?VECTOR (-Y, -X, Z)),
              color = Color}.


%%====================================================================
%% Func: step/5
%%====================================================================
%%
step (_, _, _, _, Sensor)
     when (Sensor#sensor.state > 0), (Sensor#sensor.value == 1) ->
  io:format ("BUMP!~n"),
  {ok, Sensor#sensor { state = Sensor#sensor.state - 1}};
%%
step (ObjectPid, ObjectState, Time, DeltaTime, Sensor) ->
  Position = Sensor#sensor.position,

  XSensor = Position#vector.x,
  YSensor = Position#vector.y,
  ZSensor = Position#vector.z,

  {XRobot, YRobot, ThetaRobot} = robot:command (Sensor#sensor.robot,
                                                {get_real_position}),
  {XS, YS} = world:robot_to_world ({XRobot, YRobot},
                                   ThetaRobot,
                                   {XSensor, YSensor}),
  %% io:format ("contact point ~p~n", [{XS, YS}]),
  Walls = world:get_walls (Sensor#sensor.world),
  %%
  Value = check_wall ({XS, YS}, ZSensor, Walls),
  %%
  {ok, Sensor#sensor { value = Value,
                       state = ?DYNAMICS}}.



%%
%%
check_wall (_, _, []) -> 0;
%%
check_wall ({XS, YS}, ZSensor, [W | Walls]) when ZSensor =< W#wall.height ->
  case geometry:point_in_segment ({XS, YS}, W, ?CM(1)) of
    true -> 1;
    _ -> check_wall ({XS, YS}, ZSensor, Walls)
  end;
%%
check_wall ({XS, YS}, ZSensor, [_ | Walls]) ->
  check_wall ({XS, YS}, ZSensor, Walls).
%%
%%
%%



%%====================================================================
%% Func: init/2
%%====================================================================
init (_, Sensor) ->
  {ok, Sensor#sensor { value = 0,
                       state = 0}}.
		
%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, State) ->
  ok.



