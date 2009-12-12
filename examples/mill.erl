%
% mill.erl
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
% $Id: mill.erl,v 1.5 2007/11/16 15:42:39 corrado_santoro Exp $
%
-module (mill).
-behaviour(gen_activity).

-export ([mill/1]).

-include ("geometry.hrl").

-export ([init/2,
          step/5,
          terminate/2]).

mill (Pos) ->
  C = #object3d { type = cylinder,
                  name = mycyl,
                  size = 0.2,
                  radius = 0.4,
                  position = ?VECTOR (0.0, 0.0, -0.1),
                  color = ?RGB (0.25, 1.0, 0.25)},

  X1 = 0.31 * math:cos (geometry:to_radiants (45.0 / 2.0)),
  Y1 = 0.31 * math:sin (geometry:to_radiants (45.0 / 2.0)),
  C1 = #object3d { type = sphere,
                   name = mysphere,
                   radius = 0.095,
                   position = ?VECTOR (X1, -Y1, 0),
                   color = ?RGB (1.0, 0.0, 0.0)},

  C2 = #object3d { type = pipe,
                   name = mypipe,
                   size = 0.1,
                   radius = 0.71,
                   position = ?VECTOR (0.0, 0.0, -0.05)},

  P = lists:map (
        fun (Angle) ->
            X = 0.55 * math:cos (geometry:to_radiants (Angle)),
            Y = 0.55 * math:sin (geometry:to_radiants (Angle)),
            #object3d { type = box,
                        height = 0.3,
                        width = 0.05,
                        depth = 0.1,
                        up = geometry:spheric2cartesian (
                               #spheric { rho = 1.0,
                                          theta = Angle,
                                          phi = 90.0}),
                        position = ?VECTOR (X, Y, 0.0),
                        color = ?RGB(0.25, 0.25, 0.25)}
        end,
        lists:seq(0,359,45)),

  #object3d { type = compound,
              name = threadmill,
              position = Pos,
              objects = [C, C2 | P] }.



%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: step/5
%%====================================================================
step (ObjectPid, ObjectState, Time, _, RotationSpeed) ->
  Angle = geometry:normalize_angle (Time * RotationSpeed),
  Axis = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                phi = 90.0,
                                                theta = Angle }),
  object3d:up (ObjectPid, Axis),
  {ok, RotationSpeed}.


%%====================================================================
%% Func: init/2
%%====================================================================
init (_, RotationSpeed) ->
  {ok, RotationSpeed}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, State) ->
  ok.

