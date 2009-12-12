%
% geometry.hrl
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

-record (vector, {x, y, z}).
-record (spheric, {theta, phi, rho}).
-record (speed, {v, omega}).

-define (VECTOR(X,Y,Z), #vector { x = X, y = Y, z = Z}).
%%-define (SPEED(V,W), #speed { v = V, omega = W }).

-define (RGB(R,G,B), { R, G, B}).
-define (RGBA(R,G,B,A), { R, G, B, A}).

-define (DEFAULT_AXIS, ?VECTOR (1.0, 0.0, 0.0)).

-record (object3d, { type,
                     name = noname,
                     pid = undefined,
                     tag = undefined,
                     objects = [],
                     pids = [],
                     parent_object = noname,
                     position = ?VECTOR (0.0, 0.0, 0.0),
                     default_axis = ?DEFAULT_AXIS,
                     axis = undefined,
                     default_up = ?VECTOR (0.0, 1.0, 0.0),
                     up = undefined,
                     color = ?RGB(1.0, 1.0, 1.0),
                     base_radius = 1.0,
                     top_radius = 0,
                     radius = 1.0,
                     size = 1.0,
                     width = 1.0,
                     height = 1.0,
                     depth = 1.0,
                     start_point,
                     end_point,
                     quad} ).


%%
%% DEFAULT UNITS:
%%      Distance --- Decimeters
%%      Speed    --- Decimeters / second
%%      Angles   --- Degrees
%% Angular Speed --- Degrees / second
%%


-define (CM(X), (X / 10.0)).
-define (MM(X), (X / 100.0)).
-define (M(X), X * 10.0).


-define (CM_S(X), (X / 10.0)).
-define (DEG_S(X), X).

-define (PI, 3.1415926535897932).

-define (V_TO_DEG_S(V, R), ((V / (2 * ?PI * R)) * 360.0)).

-define (DEG_S_TO_V(W, R), ((W * 2 * ?PI * R) / 360.0)).


-record (wall, {center = {0,0},
                length = 0.0,
                height = 0.0,
                orientation = 0.0,
                point_a = {0,0},
                point_b = {0,0},
                line }).

-record (line, {slope = 0.0,
                intercept = 0.0 }).
