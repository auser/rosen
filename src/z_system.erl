%
% z_system.erl
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
% $Id: z_system.erl,v 1.1 2007/12/12 17:44:39 corrado_santoro Exp $
%
-module (z_system).

-behaviour (gen_server).

-export ([start_link/1, y/2, test/0]).

-export ([init/1, handle_call/3]).

%%====================================================================
%% Func: start_link/1
%%====================================================================
start_link ({YCoefficients, BCoefficients, BU}) ->
  gen_server:start_link (?MODULE, {YCoefficients, BCoefficients, BU}, []).



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
init ({YCoefficients, BCoefficients, BU}) ->
  Outputs = [0 || _ <- YCoefficients],
  Inputs = [0 || _ <- BCoefficients],
  {ok, {Inputs, Outputs, {YCoefficients, BCoefficients, BU}}}.


%%====================================================================
%% Func: handle_call/3
%%====================================================================
%% @private
%%
handle_call ({y, Input}, _,
             {Inputs, Outputs, {YCoefficients, BCoefficients, BU}}) ->
  YTuples = lists:zip (Outputs, YCoefficients),
  YMultiplies = [ O * A || {O, A} <- YTuples ],
  UTuples = lists:zip (Inputs, BCoefficients),
  UMultiplies = [ O * A || {O, A} <- UTuples ],
  Y =
    lists:foldl( fun (X, Sum) -> X + Sum end, 0, YMultiplies) +
    lists:foldl( fun (X, Sum) -> X + Sum end, 0, UMultiplies) +
    Input * BU,
  %%
  [_ | ReversedOutputs ] = lists:reverse (Outputs),
  NewOutputs = [Y | lists:reverse (ReversedOutputs)],
  %%
  [_ | ReversedInputs ] = lists:reverse (Inputs),
  NewInputs = [Input | lists:reverse (ReversedInputs)],
  %%
  {reply, Y, {NewInputs, NewOutputs, {YCoefficients, BCoefficients, BU}}}.


%%
%%

test () ->
  Y = [0.1, 0.1],
  B = [0.0, 0.0, 0.0, 0.0, 0.2, 0.2],
  BU = 0.1,
  {ok, Pid} = z_system:start_link ({Y, B, BU}),
  [ z_system:y (Pid, 1) || _ <- lists:seq (1,20) ] ++
    [ z_system:y (Pid, 0) || _ <- lists:seq (1,20) ].

