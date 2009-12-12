%
% two_wheels_driving.erl
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
% $Id: two_wheels_driving.erl,v 1.5 2007/12/10 14:57:17 corrado_santoro Exp $
%
-module (two_wheels_driving).

-behaviour (robot).

-include ("geometry.hrl").
-include ("robot.hrl").

-export ([create_driving_objects/1,
          create_activities/4,
          handle_command/2]).

%%====================================================================
%% Func: create_driving_objects/1
%%====================================================================
create_driving_objects (Robot = #robot{}) ->
  WheelRadius = Robot#robot.wheel_radius,
  WheelDistance = Robot#robot.wheel_distance,

  LeftW = wheel (WheelRadius, left_wheel,
                 ?VECTOR (WheelDistance / 2, 0.0, 0.0),
                 ?VECTOR (1.0, 0.0, 0.0)),

  RightW = wheel (WheelRadius, right_wheel,
                  ?VECTOR (-WheelDistance / 2, 0.0, 0.0),
                  ?VECTOR (-1.0, 0.0, 0.0)),

  Ax = #object3d { type = cylinder,
                   radius = ?MM(5),
                   axis = ?VECTOR (1.0, 0, 0.0),
                   size = WheelDistance,
                   color = ?RGB(0.1, 0.1, 0),
                   position = ?VECTOR (- WheelDistance / 2.0, 0.0, 0.0) },

  {[LeftW, RightW, Ax], []}.


%%====================================================================
%% Func: create_activities/4
%%====================================================================
create_activities (Robot, TheRobotPid, _DrivingObjects, _DrivingInfo) ->
  WheelRadius = Robot#robot.wheel_radius,
  WheelDistance = Robot#robot.wheel_distance,
  WheelTicks = Robot#robot.wheel_ticks,
  {ok, Pid} = object3d:add_activity ( TheRobotPid, diff_drive,
                                      [{v, 0},
                                       {omega, 0},
                                       {position, {-?CM(100), ?CM(50)}},
                                       {orientation, -90},
                                       {left_wheel, left_wheel},
                                       {right_wheel, right_wheel},
                                       {wheel_radius, WheelRadius},
                                       {wheel_distance, WheelDistance},
                                       {ticks, WheelTicks}]),
  Pid.


%%====================================================================
%% Func: handle_command/2
%%====================================================================
handle_command ({set_speed, V, W}, Robot) ->
  gen_activity:set_property (Robot#robot.motion_pid, v, V),
  gen_activity:set_property (Robot#robot.motion_pid, omega, W),
  {ok, Robot};
%%
handle_command ({set_position, X, Y, Theta}, Robot) ->
  gen_activity:set_property (Robot#robot.motion_pid,
                             position, {X, Y, Theta}),
  {ok, Robot};
%%
handle_command ({get_position}, Robot) ->
  Reply = gen_activity:get_property (Robot#robot.motion_pid,
                                     current_position),
  {Reply, Robot};
%%
handle_command ({get_real_position}, Robot) ->
  Reply = gen_activity:get_property (Robot#robot.motion_pid,
                                     current_real_position),
  {Reply, Robot}.
%%



%%--------------------------------------------------------------------
%% Func: wheel/4
%%--------------------------------------------------------------------
wheel (Radius, Tag, Position, Axis) ->
  Wheel = #object3d { type = cylinder,
                      radius = Radius,
                      size = ?CM(2),
                      color = ?RGB(0, 1.0, 0)},
  Screw1 = #object3d { type = cylinder,
                       radius = ?MM(3),
                       size = ?CM(2),
                       color = ?RGB(0, 0, 0),
                       position = ?VECTOR (0.0, -Radius / 2, ?MM(1))},
  Screw2 = #object3d { type = cylinder,
                       radius = ?MM(3),
                       size = ?CM(2),
                       color = ?RGB(0, 0, 0),
                       position = ?VECTOR (0.0, Radius / 2, ?MM(1))},

  #object3d { type = compound,
              tag = Tag,
              position = Position,
              axis = Axis,
              objects = [Wheel,
                         Screw1,
                         Screw2]}.



