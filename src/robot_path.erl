%
% robot_path.erl
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
%
-module (robot_path).
-behaviour(gen_activity).

-include ("geometry.hrl").


-export ([init/2,
          step/5,
          get_property/3,
          terminate/2]).

-record (path_state, { path,
                       path_tail,
                       current_target,
                       restart = false,
                       robot_pid,
                       robot_position,
                       robot_orientation,
                       motion_tolerance,
                       motion_speed,
                       rotation_tolerance,
                       rotation_speed}).

%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: step/5
%%====================================================================
step (ObjectPid, ObjectState, _, _,
      PathState = #path_state { current_target = nil,
                                path_tail = [],
                                restart = true}) ->
  {ok, PathState#path_state {
         path_tail = PathState#path_state.path
        }
  };
%%
step (ObjectPid, ObjectState, _, _,
      PathState = #path_state { current_target = nil,
                                path_tail = []}) ->
  robot:command (PathState#path_state.robot_pid,
                 {set_speed, 0.0, 0.0}),
  {ok, PathState};
%%
step (ObjectPid, ObjectState, _, _,
      PathState = #path_state { current_target = nil }) ->
  [Current | Tail] = PathState#path_state.path_tail,
  {X, Y, Theta} = robot:command (PathState#path_state.robot_pid,
                                 {get_position}),
  NewPathState =
    PathState#path_state {
      path_tail = Tail,
      current_target = Current,
      robot_position = {X, Y},
      robot_orientation = Theta
      },
  {ok, NewPathState};
%%
step (ObjectPid, ObjectState, _, _,
      PathState = #path_state { current_target = {forward, X} }) ->
  SPos = PathState#path_state.robot_position,

  io:format("Forward ~p ~p~n",[PathState#path_state.path_tail,PathState]),

  {RobotX, RobotY, _} = robot:command (PathState#path_state.robot_pid,
                                       {get_position}),
  D = abs (geometry:distance (SPos, {RobotX, RobotY}) - X),

  %%  io:format ("~p, ~p~n", [D,PathState#path_state.motion_tolerance]),

  if
    D < PathState#path_state.motion_tolerance ->
      NewPathState = PathState#path_state { current_target = nil },
      {ok, NewPathState};
    true ->
      robot:command (PathState#path_state.robot_pid,
                     {set_speed, PathState#path_state.motion_speed, 0.0}),
      {ok, PathState}
  end;
%%
step (ObjectPid, ObjectState, _, _,
      PathState = #path_state { current_target = {rotate, X} }) ->
  SPos = PathState#path_state.robot_orientation,

  io:format("Rotate ~p ~p~n",[PathState#path_state.path_tail,PathState]),

  {_, _, CPos} = robot:command (PathState#path_state.robot_pid,
                                {get_position}),
  D = abs (geometry:normalize_angle (X - (CPos - SPos))),
  DA = abs (D),
  %%io:format ("~p, ~p, ~p, ~p~n", [SPos, CPos, X, D]),

  %%io:format ("~p, ~p~n", [DA,PathState#path_state.rotation_tolerance]),
  if
    DA < PathState#path_state.rotation_tolerance ->
      NewPathState = PathState#path_state { current_target = nil },
      {ok, NewPathState};
    true ->
      robot:command (PathState#path_state.robot_pid,
                     {set_speed, 0,
                      PathState#path_state.rotation_speed * sgn (X)}),
      {ok, PathState}
  end.

sgn (X) when X < 0 -> -1.0;
sgn (X) when X > 0 -> 1.0;
sgn (_) -> 0.



%%====================================================================
%% Func: get_property/3
%%====================================================================
get_property (_, path_done, PathState) ->
  (PathState#path_state.path_tail == []) and
   (PathState#path_state.current_target == nil).


%%====================================================================
%% Func: set_property/4
%%====================================================================
set_property (_, path_done, PathState) ->
  PathState#path_state { path = [],
                         restart = true}.


%%====================================================================
%% Func: init/1
%%====================================================================
init (_, Properties) ->
  {ok,
   #path_state {
     path = proplists:get_value (path, Properties),
     robot_pid = proplists:get_value (robot, Properties),
     motion_speed = proplists:get_value (motion_speed, Properties),
     rotation_speed = proplists:get_value (rotation_speed, Properties),
     motion_tolerance = proplists:get_value (motion_tolerance, Properties),
     rotation_tolerance = proplists:get_value (rotation_tolerance, Properties),
     restart = proplists:get_value (restart, Properties),
     current_target = nil,
     path_tail = proplists:get_value (path, Properties)}}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, State) ->
  ok.

