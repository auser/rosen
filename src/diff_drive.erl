%
% diff_drive.erl
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
%%

-module (diff_drive).
-behaviour(gen_activity).

-include ("geometry.hrl").


-export ([init/2,
          step/5,
          set_property/4,
          get_property/3,
          terminate/2]).

-record (diff_motion_state, { object_pid,
                              wheel_radius = 0.0,
                              wheel_distance = 0.0,
                              left_wheel,
                              right_wheel,
                              ramp_left,
                              ramp_right,
                              ticks = 0.0,
                              ticks_per_distance = 0.0,
                              w_left = 0.0,
                              w_right = 0.0,
                              ticks_left = 0.0,
                              ticks_right = 0.0,
                              distance_remainder_left = 0.0,
                              distance_remainder_right = 0.0,
                              pos_x = 0.0,
                              pos_y = 0.0,
                              pos_theta = 0.0,
                              theta = 0.0,
                              speed_v = 0.0,
                              speed_w = 0.0,
                              plane}).

%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: step/5
%%====================================================================
step (ObjectPid, ObjectState, Time, DeltaTime, MotionState) ->
  %% rotate wheels
  WL = s_curve_profile:y (MotionState#diff_motion_state.ramp_left,
                          MotionState#diff_motion_state.w_left),

  WR = s_curve_profile:y (MotionState#diff_motion_state.ramp_right,
                          MotionState#diff_motion_state.w_right),

  rotate_wheel (MotionState#diff_motion_state.left_wheel, -WL,
                DeltaTime),

  rotate_wheel (MotionState#diff_motion_state.right_wheel, WR,
                DeltaTime),

  R = MotionState#diff_motion_state.wheel_radius,
  VL = ?DEG_S_TO_V (WL, R),
  VR = ?DEG_S_TO_V (WR, R),
  L = MotionState#diff_motion_state.wheel_distance,
  % Determine linear and angolar speeds
  V = (VL + VR) / 2.0,
  W = geometry:to_degrees ((VR - VL) / L),
  Angle = MotionState#diff_motion_state.theta + W * DeltaTime,
  Axis = rotation (MotionState#diff_motion_state.plane,
                   robot:angle_to_3d (Angle)),
  object3d:axis (ObjectPid, Axis),
  SpeedVector = geometry:dot_v (V * DeltaTime, Axis),
  NewPos = geometry:add (object3d:position (ObjectPid), SpeedVector),
  object3d:position (ObjectPid, NewPos),

  %% now let's compute encoder ticks...
  DistanceLeft = VL * DeltaTime +
    MotionState#diff_motion_state.distance_remainder_left,
  DistanceRight = VR * DeltaTime +
    MotionState#diff_motion_state.distance_remainder_right,

  %% now do dead reckoning LEFT
  NL =
    trunc (DistanceLeft * MotionState#diff_motion_state.ticks_per_distance),
  %%    + random_error (MotionState#diff_motion_state.ticks),

  DistanceRemainderLeft =
    (DistanceLeft * MotionState#diff_motion_state.ticks_per_distance - NL) /
    MotionState#diff_motion_state.ticks_per_distance,

  %% now do dead reckoning RIGHT
  NR =
    trunc (DistanceRight * MotionState#diff_motion_state.ticks_per_distance),
  %%    + random_error (MotionState#diff_motion_state.ticks),

  DistanceRemainderRight =
    (DistanceRight * MotionState#diff_motion_state.ticks_per_distance - NR) /
    MotionState#diff_motion_state.ticks_per_distance,

  DL = NL / MotionState#diff_motion_state.ticks_per_distance,
  DR = NR / MotionState#diff_motion_state.ticks_per_distance,

  TH = geometry:to_radiants (MotionState#diff_motion_state.pos_theta) +
    (DR - DL) / L,

  X = MotionState#diff_motion_state.pos_x + ((DL + DR) / 2.0 * math:cos (TH)),
  Y = MotionState#diff_motion_state.pos_y + ((DL + DR) / 2.0 * math:sin (TH)),

%%   io:format ("~p, ~p, ~p, ~p, ~p~n",
%%              [{DeltaTime, VL, VR},
%%               {DistanceLeft,  DistanceRight}, {NL, NR},
%%               geometry:to_degrees (TH), Angle]),

%%  io:format ("Theta ~p~n", [TH]),
  NAngle = geometry:normalize_angle (Angle),
  PosTheta = geometry:normalize_angle (geometry:to_degrees (TH)),

  {RealPosX, RealPosY} = world:world_from_3d (NewPos),

%%%%%%%%%%  io:format ("Pos. Real = ~p~n",
%             [{RealPosX, RealPosY, NAngle}]),
 % io:format ("     Est. = ~p, Delta = ~p~n~n",
  %           [{X, Y, PosTheta},
  %            {abs (X - RealPosX),
  %             abs (Y - RealPosY),
  %             abs (NAngle - PosTheta)}]),

  {ok,
   MotionState#diff_motion_state {
     speed_w = W,
     speed_v = V,
     theta = NAngle,
     pos_theta = PosTheta,
     pos_x = X,
     pos_y = Y,
     distance_remainder_left = DistanceRemainderLeft,
     distance_remainder_right = DistanceRemainderRight
    } }.




rotate_wheel (Pid, W, DeltaTime) ->
  ThetaRot = W * DeltaTime,
  Up = object3d:up (Pid),
  Axis = ?VECTOR(0.0, 0.0, 1.0),
  NewUp = geometry:rotate (Up, ThetaRot, Axis),
  object3d:up (Pid, NewUp).


%%
%%
rotation (P, Angle) when P == xy; P == yx->
  geometry:spheric2cartesian (#spheric { rho = 1.0,
                                         theta = Angle,
                                         phi = 90.0});
%%
rotation (P, Angle) when P == xz; P == zx ->
  geometry:spheric2cartesian (#spheric { rho = 1.0,
                                         theta = 0.0,
                                         phi = Angle});
%%
rotation (P, Angle) when P == yz; P == zy ->
  geometry:spheric2cartesian (#spheric { rho = 1.0,
                                         theta = 90.0,
                                         phi = Angle}).


%%
%%
random_error (C) -> 0.
%%   X = random:uniform (C),
%%   X rem 2.
  %%X rem trunc (C * 0.01).


%%====================================================================
%% Func: get_property/3
%%====================================================================
get_property (_, current_position, MotionState) ->
  {MotionState#diff_motion_state.pos_x,
   MotionState#diff_motion_state.pos_y,
   MotionState#diff_motion_state.pos_theta};
%%
get_property (Pid, current_real_position, MotionState) ->
  {RealPosX, RealPosY} = world:world_from_3d (object3d:position (Pid)),
  {RealPosX, RealPosY, MotionState#diff_motion_state.theta};
%%
get_property (_, v, MotionState) ->
  MotionState#diff_motion_state.speed_v;
%%
get_property (_, omega, MotionState) ->
  MotionState#diff_motion_state.speed_w;
%%
get_property (_, theta, MotionState) ->
  MotionState#diff_motion_state.pos_theta;
%%
get_property (_, _, _) ->
  undefined.



%%====================================================================
%% Func: set_property/4
%%====================================================================
set_property (_, w_left, Value, MotionState) ->
  MotionState#diff_motion_state { w_left = Value };
%%
set_property (_, w_right, Value, MotionState) ->
  MotionState#diff_motion_state { w_right = Value};
%%
set_property (_, v, Value, MotionState) ->
  set_lr (MotionState#diff_motion_state { speed_v = Value });
%%
set_property (_, omega, Value, MotionState) ->
  set_lr (MotionState#diff_motion_state { speed_w = Value });
%%
set_property (_, position, {X, Y, Theta}, MotionState) ->
  CP = object3d:position (MotionState#diff_motion_state.object_pid),
  object3d:position (MotionState#diff_motion_state.object_pid,
                     world:world_to_3d (X, Y, CP#vector.y)),
  MotionState#diff_motion_state { pos_x = X,
                                  pos_y = Y,
                                  pos_theta = Theta,
                                  theta = Theta};
%%
set_property (_, pos_x, Value, MotionState) ->
  MotionState#diff_motion_state { pos_x = Value};
%%
set_property (_, pos_y, Value, MotionState) ->
  MotionState#diff_motion_state { pos_y = Value};
%%
set_property (_, pos_theta, Value, MotionState) ->
  MotionState#diff_motion_state { pos_theta = Value};
%%
set_property (_, _, _, MotionState) ->
  MotionState.



%%
%%
set_lr (State) ->
  L = State#diff_motion_state.wheel_distance,
  R = State#diff_motion_state.wheel_radius,
  V = State#diff_motion_state.speed_v,
  W = geometry:to_radiants (State#diff_motion_state.speed_w),
%%   io:format ("~p, ~p, ~p~n",
%%              [State#diff_motion_state.speed_w, W, L]),
  VL = V - L * W / 2.0,
  VR = V + L * W / 2.0,
%%   io:format ("V ~p, R ~p, W ~p~n",
%%              [VL, R, ?V_TO_DEG_S (VL, R)]),
  State#diff_motion_state { w_left = ?V_TO_DEG_S (VL, R),
                            w_right = ?V_TO_DEG_S (VR, R)}.


%%
%%
opengl_to_robot_world (V = #vector{}, Theta) ->
  {V#vector.x, -V#vector.z, geometry:normalize_angle (Theta - 90)}.
%%
%%





%%====================================================================
%% Func: init/2
%%====================================================================
init (Object, Properties) ->
  Orientation3D = geometry:angle (Object#object3d.axis,
                                  Object#object3d.default_axis),
  {PositionX, PositionY} = proplists:get_value (position, Properties),
  Orientation = proplists:get_value (orientation, Properties),
  R = proplists:get_value (wheel_radius, Properties),
  T = proplists:get_value (ticks, Properties),
  LeftTag = proplists:get_value (left_wheel, Properties),
  RightTag = proplists:get_value (right_wheel, Properties),
  [Left] = [X || X <- Object#object3d.pids,
                 object3d:tag (X) == LeftTag],
  [Right] = [X || X <- Object#object3d.pids,
                  object3d:tag (X) == RightTag],

  {ok, RampLeft} = s_curve_profile:start_link (0.65),
  {ok, RampRight} = s_curve_profile:start_link (0.65),

  {ok,
   #diff_motion_state {
     object_pid = Object#object3d.pid,
     left_wheel = Left,
     right_wheel = Right,
     ramp_left = RampLeft,
     ramp_right = RampRight,
     theta = Orientation3D,
     pos_theta = Orientation,
     pos_x = PositionX,
     pos_y = PositionY,
     wheel_distance = proplists:get_value (wheel_distance, Properties),
     wheel_radius = R,
     ticks = T,
     ticks_per_distance = T / (2 * ?PI * R),
     plane = xz}}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, State) ->
  ok.

