%
% gp2d12.erl
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
% $Id: gp2d12.erl,v 1.3 2008/02/29 15:44:10 corrado_santoro Exp $
%
-module (gp2d12).

-include ("geometry.hrl").
-include ("robot.hrl").

-behaviour (gen_activity).

-export ([init/2, step/5, model/2, terminate/2]).

-define (MIN_DIST, ?CM(10)).
-define (MAX_DIST, ?CM(80)).


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
%%

step (ObjectPid, ObjectState, Time, DeltaTime, Sensor) ->
  Position = Sensor#sensor.position,
  XSensor = Position#vector.x,
  YSensor = Position#vector.y,
  ZSensor = Position#vector.z,

  ThetaSensor = Sensor#sensor.parameters,

  {XRobot, YRobot, ThetaRobot} = robot:command (Sensor#sensor.robot,
                                                {get_real_position}),
  {XS, YS} = world:robot_to_world ({XRobot, YRobot},
                                   ThetaRobot,
                                   {XSensor, YSensor}),
  ThetaS = geometry:normalize_angle (ThetaSensor + ThetaRobot),

  SensorLine = geometry:line_for_one_point (XS, YS, ThetaS),

  Walls = world:get_walls (Sensor#sensor.world),

%%   io:format ("Sensor line ~p~n", [SensorLine]),

  Intersections = lists:map (fun (X) ->
                                 P = geometry:line_intersect (SensorLine,
                                                              X#wall.line),
%%                                  io:format ("Intersection ~p,~p~n",
%%                                              [P, X#wall.line]),
                                 {P, X}
                             end, Walls),
  ValidIntersections =
    lists:filter (fun ({{parallel}, _}) -> false;
                      ({{ok, {XI, YI}}, Wall}) ->
%%                       io:format ("Intersection local ~p~n",
%%                                  [world:world_to_robot ({XRobot, YRobot},
%%                                                         ThetaRobot,
%%                                                         {XI, YI})]),
                      if
                        ZSensor =< Wall#wall.height ->
                          case world:is_in_front ({XRobot, YRobot},
                                                  ThetaRobot,
                                                  {XI, YI}) of
                            false -> false;
                            true -> geometry:point_in_segment ({XI, YI},
                                                               Wall, ?CM(1))
                          end;
                        true ->
                          false
                      end
                  end, Intersections),

  Distances = lists:map (fun ({{ok, {XI, YI}}, _}) ->
                             XD = (XI - XS),
                             YD = (YI - YS),
                             XD * XD + YD * YD
                         end, ValidIntersections),

%%   io:format ("Distances ~p~n", [Distances]),
  RetVal =
    if
      Distances == [] ->
        DMin = empty,
        {ok, Sensor#sensor { value = ?MAX_DIST}};
      true ->
        DMin = math:sqrt (lists:min (Distances)),
        if
          DMin < ?MIN_DIST ->
            {ok, Sensor#sensor { value = ?MIN_DIST}};
          DMin > ?MAX_DIST ->
            {ok, Sensor#sensor { value = ?MAX_DIST}};
          true ->
            {ok, Sensor#sensor { value = DMin}}
        end
    end,
%  io:format ("Distance ~p~n", [(element (2, RetVal))#sensor.value]),
  RetVal.




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


