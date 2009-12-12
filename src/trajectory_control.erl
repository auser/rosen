%
% trajectory_control.erl
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
-module (trajectory_control).
-behaviour(gen_activity).

-include ("geometry.hrl").


-export ([init/2,
          set_property/4,
          step/5,
          terminate/2]).

-record (trajectory_state, { x,
                             y,
                             theta,
                             reached = false,
                             stopped = true,
                             diff_drive_pid,
                             k_rho,
                             k_alpha,
                             k_beta}).

-define (TARGET_EPS, ?CM(5)).
-define (TARGET_ANGLE_EPS, 3).

%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: step/5
%%====================================================================
step (ObjectPid, ObjectState, _, _,
      State = #trajectory_state { reached = true }) ->
  {ok, State};
%%
step (ObjectPid, ObjectState, _, _,
      State = #trajectory_state { stopped = true }) ->
  {ok, State};
%%
step (ObjectPid, ObjectState, _, _, State) ->
  DiffDrive = State#trajectory_state.diff_drive_pid,
  {XR, YR, ThetaR} = gen_activity:get_property (DiffDrive,
                                                current_position),
  {XT, YT, ThetaT} = {State#trajectory_state.x,
                      State#trajectory_state.y,
                      State#trajectory_state.theta},
  {EX, EY, ETheta} = global_to_robot ( {XR, YR, ThetaR},
                                       {XT, YT, ThetaT} ),

  io:format ("Current ~p, Target ~p~n",
             [{XR, YR, ThetaR}, {XT, YT, ThetaT}]),

  TargetReached =
    (abs (EX) < ?TARGET_EPS) and
    (abs (EY) < ?TARGET_EPS) and
    (abs (ETheta) < ?TARGET_ANGLE_EPS),

  if
    TargetReached ->
      {ok, State#trajectory_state { reached = true } };
    true ->
      DX = XT - XR,
      DY = YT - YR,
      Rho = math:sqrt (DX * DX + DY * DY),
      Alpha = - ThetaR + geometry:to_degrees (math:atan2 (DX, DY)),
      Beta = ThetaR + Alpha,
      V = State#trajectory_state.k_rho * Rho,
      W =
        State#trajectory_state.k_alpha * Alpha +
        State#trajectory_state.k_beta * Beta,
      gen_activity:set_property (DiffDrive, v, V),
      gen_activity:set_property (DiffDrive, omega, W),
      {ok, State}
  end.
%%


%%
global_to_robot ( {XR, YR, ThetaR}, {XT, YT, ThetaT} ) ->
  TraslX = XT - XR,
  TraslY = YT - YR,
  DX = TraslX * math:cos(ThetaR) - TraslY * math:sin(ThetaR),
  DY = TraslX * math:sin(ThetaR) + TraslY * math:cos(ThetaR),
  {DX, DY, ThetaT - ThetaR}.
%%



%%====================================================================
%% Func: set_property/4
%%====================================================================
set_property (_, target, {X, Y, T}, State) ->
  State#trajectory_state { x = X,
                           y = Y,
                           theta = T,
                           reached = false,
                           stopped = false};
%%
set_property (_, _, _, MotionState) ->
  MotionState.


%%====================================================================
%% Func: init/1
%%====================================================================
init (_, Properties) ->
  {ok,
   #trajectory_state {
     diff_drive_pid = proplists:get_value (diff_drive, Properties),
     k_rho = proplists:get_value (k_rho, Properties),
     k_alpha = proplists:get_value (k_alpha, Properties),
     k_beta = proplists:get_value (k_beta, Properties)}}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, State) ->
  ok.

