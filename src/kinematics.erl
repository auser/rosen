%%
%% kinematics.erl
%%
%% ----------------------------------------------------------------------
%%
%%  ROSEN, a RObotic Simulation Erlang eNgine
%%  Copyright (C) 2007 Corrado Santoro (csanto@diit.unict.it)
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%%
%%
%%  @doc This module provides kinematics for ROSEN objects.
%%       It is a gen_activity, so that basic kinematics is naturally added
%%       to an object as one of his activities. <br/>
%%       <br/>
%%       The step callback function updates the position of the object according
%%       to the linear and angular speeds properties (the <I>v</I>, <I>omega</I>
%%       and <I>theta</I>, respectively). A simple constant-speed linear
%%       kinematics has only a linear speed <I>v</I> set to the desired speed value.
%%
%%

-module (kinematics).
-behaviour(gen_activity).

-include ("geometry.hrl").


-export ([init/2,
          step/5,
          set_property/4,
          get_property/3,
          terminate/2]).

-record (motion_state, { speed_v,
                         speed_w,
                         theta,
                         plane}).

%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: step/5
%%====================================================================
%%
%% @spec step (ObjectPid, ObjectState, Time, DeltaTime, MotionState) -> {ok, NewMotionState}
%%       ObjectPid = pid()
%%       ObjectState = object3d()
%%       Time = float()
%%       DeltaTime = float()
%%       MotionState = activity_state()
%% @doc Perform a step of the kinematic activity.
%%      This callback calculates the next position of the object according
%%      to the linear and angular speeds specified as parameters.
%%
step (ObjectPid, ObjectState, Time, DeltaTime, MotionState) ->
  Angle = MotionState#motion_state.theta +
    MotionState#motion_state.speed_w * DeltaTime,
  Axis = rotation (MotionState#motion_state.plane, Angle),
  object3d:axis (ObjectPid, Axis),
  %%relPos = self.__ThetaVector * self.__SpeedV * self.__DeltaTime
  SpeedVector = geometry:dot_v (MotionState#motion_state.speed_v * DeltaTime,
                                Axis),
  NewPos = geometry:add (object3d:position (ObjectPid),
                         SpeedVector),
  object3d:position (ObjectPid, NewPos),
  {ok, MotionState#motion_state { theta = geometry:normalize_angle (Angle) } }.



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


%%====================================================================
%% Func: get_property/3
%%====================================================================
%%
%% @spec get_property (Object3d, Property, MotionState) -> term()
%%       Object3d = object_3d()
%%       Property = v | omega | theta
%%       MotionState = activity_state()
%% @doc  Retrieve the value of kinematic parameters.
%%       Depending on the Property parameter passed (either <i>v</i> or
%%       <i>omega</i> or <i>theta</i>), this function returns the current
%%       linear speed, the x-y angular speed or the y-z angular speed,
%%       respectively.
%%
%%
get_property (_, v, MotionState) ->
  MotionState#motion_state.speed_v;
%%
get_property (_, omega, MotionState) ->
  MotionState#motion_state.speed_w;
%%
get_property (_, theta, MotionState) ->
  MotionState#motion_state.theta;
%%
get_property (_, _, _) ->
  undefined.



%%====================================================================
%% Func: set_property/4
%%====================================================================
%%
%% @spec set_property(Object3d, Property, Value, MotionState) -> term()
%%       Object3d = object_3d()
%%       Property = v | omega | theta
%%       Value = float()
%%       MotionState = activity_state()
%% @doc  Set the value of kinematic parameters.
%%       Depending on the Property parameter passed (either <i>v</i> or
%%       <i>omega</i> or <i>theta</i>), this function sets the current
%%       linear speed, the x-y angular speed or the y-z angular speed,
%%       respectively, to Value.
%%
%%
set_property (_, v, Value, MotionState) ->
  MotionState#motion_state { speed_v = Value };
%%
set_property (_, omega, Value, MotionState) ->
  MotionState#motion_state { speed_w = Value};
%%
set_property (_, _, _, MotionState) ->
  MotionState.



%%====================================================================
%% Func: init/2
%%====================================================================
init (_, Properties) ->
  {ok, #motion_state { speed_v = proplists:get_value (v, Properties),
                       speed_w = proplists:get_value (omega, Properties),
                       plane = xz,
                       theta = 0.0}}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, State) ->
  ok.

