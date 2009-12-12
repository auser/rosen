%
% s_curve_profile.erl
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
% $Id: s_curve_profile.erl,v 1.1 2007/12/12 17:43:12 corrado_santoro Exp $
%
-module (s_curve_profile).

-behaviour (gen_server).

-export ([start_link/1, y/2, test/0]).

-export ([init/1, handle_call/3]).

-record (s_curve, {pedestal = 0.0,
                   slope = 1.0,
                   func,
                   t = 0.0,
                   last_input = 0.0}).

-define (OFFSET, -3).

%%====================================================================
%% Func: start_link/1
%%====================================================================
start_link (Slope) ->
  gen_server:start_link (?MODULE, {Slope}, []).



%%====================================================================
%% Func: y/2
%%====================================================================
y (Pid, Input) ->
  gen_server:call (Pid, {y, Input}).



%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
%%
init ({Slope}) ->
  {ok, #s_curve {slope = Slope,
                 func = fun (X) -> X end,
                 t = 0.0,
                 last_input = 0.0}}.


%%====================================================================
%% Func: handle_call/3
%%====================================================================
%% @private
%%
handle_call ({y, Input}, _, S_Curve) ->
  if
    Input =/= S_Curve#s_curve.last_input ->
      T = 0.0;
    true ->
      T = S_Curve#s_curve.t
  end,

  if
    Input > S_Curve#s_curve.last_input ->
      %% rising
      Gain = Input - S_Curve#s_curve.last_input,
      Pedestal = S_Curve#s_curve.last_input,
      Fun =
        fun (X) ->
            Gain / (1 + math:exp (- (S_Curve#s_curve.slope * X + ?OFFSET))) +
              Pedestal
        end;
    Input < S_Curve#s_curve.last_input ->
      %% falling
      Gain = S_Curve#s_curve.last_input - Input,
      Pedestal = Input,
      Fun =
        fun (X) ->
            Gain *
              (1 -
               1 / (1 + math:exp (- (S_Curve#s_curve.slope * X + ?OFFSET))))
              + Pedestal
        end;
    Input == S_Curve#s_curve.last_input ->
      Fun = S_Curve#s_curve.func
  end,

  Y = Fun (T),

  %%
  {reply, Y, S_Curve#s_curve { last_input = Input,
                               t = T + 1,
                               func = Fun }}.


%%
%%

test () ->
  {ok, Pid} = s_curve_profile:start_link (0.5),
  [ s_curve_profile:y (Pid, -10) || X <- lists:seq (1,15) ] ++
    [ s_curve_profile:y (Pid, 0) || X <- lists:seq (1,15) ].

