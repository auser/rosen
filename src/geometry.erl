%
% geometry.erl
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
% $Id: geometry.erl,v 1.8 2008/02/27 15:29:13 corrado_santoro Exp $
%%
%% @doc A module providing basic functions for vector and coordinate
%% operations.
%%
%% The include file "geometry.hrl" provides the definition of the records
%% <code>#vector{}</code> and <code>#speric{}</code>,
%% to represent coordinates and vectors (see below)
%% and some macros, used to define colors, vectors and real
%% measurement units. The following macros
%% are available:
%% <ul>
%% <li><code>?VECTOR(X,Y,Z)</code>, defines a vector.</li>
%% <li><code>?RGB(R,G,B)</code>, defines an RGB color; costants are real
%%     numbers in the range [0,1].</li>
%% <li><code>?CM(X)</code>, defines a value in centimeters.</li>
%% <li><code>?MM(X)</code>, defines a value in millimeters.</li>
%% <li><code>?M(X)</code>, defines a value in meters.</li>
%% <li><code>?CM_S(X)</code>, defines a linear speed in cm/s.</li>
%% <li><code>?DEG_S(X)</code>, defines an angular speed in degree/s.</li>
%% </ul>
%%
%% @type vector() = #vector{}.
%% A record representing a vector (or a point) in 3D space and
%% has the fields <code>x</code>, <code>y</code> and <code>z</code>.
%%
%% @type spheric() = #spheric{}.
%% A record representing spheric coordinates. It has the fields:
%% <ul>
%% <li><code>rho</code>, the distance of the point from the origin;</li>
%% <li><code>theta</code>, the angle formed with the <i>x</i> axis
%%     along <i>xy</i> plane;</li>
%% <li><code>phi</code>, the angle formed with the <i>z</i> axis.</li>
%% </ul>
%%
%%
-module (geometry).
-include ("geometry.hrl").

-export ([norm/1,
          dot_v/2,
          dot/2,
          cross/2,
          angle/2,
          distance/2,
          cartesian2spheric/1,
          spheric2cartesian/1,
          rotate/3,
          move/2,
          add/2,
          to_radiants/1,
          to_degrees/1,
          normalize_angle/1,
          line_for_one_point/4,
          line_for_one_point/3,
          line_intersect/2,
          point_in_segment/3]).

%%====================================================================
%% Function: add/2
%%====================================================================
%% @spec add(V1::vector(), V2::vector()) -> vector()
%%
%% @doc Adds two vectors.
%%
add (V1 = #vector{}, V2 = #vector{}) ->
  #vector { x = V1#vector.x + V2#vector.x,
            y = V1#vector.y + V2#vector.y,
            z = V1#vector.z + V2#vector.z }.


%%====================================================================
%% Function: dot_v/2
%%====================================================================
%% @spec dot_v(A::number(), V::vector()) -> vector()
%%
%% @doc Multiplies a scalar with a vector.
%%
dot_v (A, V = #vector {}) ->
  #vector { x = A * V#vector.x,
            y = A * V#vector.y,
            z = A * V#vector.z }.


%%====================================================================
%% Function: dot/2
%%====================================================================
%% @spec dot(V1::vector(), V2::vector()) -> number()
%%
%% @doc Vector dot product.
%%
dot (A = #vector {}, B = #vector {}) ->
  A#vector.x * B#vector.x +
    A#vector.y * B#vector.y +
    A#vector.z * B#vector.z.


%%====================================================================
%% Function: cross/2
%%====================================================================
%% @spec cross(V1::vector(), V2::vector()) -> number()
%%
%% @doc Vector cross product.
%%
cross (A = #vector {}, B = #vector {}) ->
  #vector
        { x = A#vector.y * B#vector.z - A#vector.z * B#vector.y,
          y = A#vector.z * B#vector.x - A#vector.x * B#vector.z,
          z = A#vector.x * B#vector.y - A#vector.y * B#vector.x }.

%%====================================================================
%% Function: angle/2
%%====================================================================
%% @spec angle(V1::vector(), V2::vector()) -> number()
%%
%% @doc Determines the angle (in degrees) formed by two vectors.
%%
angle (A = #vector {}, B = #vector {}) ->
  to_degrees (math:acos (dot (A, B) / (norm(A) * norm (B)))).


%%====================================================================
%% Function: norm/1
%%====================================================================
%% @spec norm(V::vector()) -> number()
%%
%% @doc Computes the length of a vector.
%%
norm (C = #vector {}) ->
  math:sqrt (C#vector.x * C#vector.x +
             C#vector.y * C#vector.y +
             C#vector.z * C#vector.z).

%%====================================================================
%% Function: distance/2
%%====================================================================
%% @spec distance(V1::vector(), V2::vector()) -> number()
%%
%% @doc Compute the distance between the targets of by two vectors.
%%
distance (C1 = #vector {}, C2 = #vector {}) ->
  X1 = C1#vector.x,
  Y1 = C1#vector.y,
  Z1 = C1#vector.z,

  X2 = C2#vector.x,
  Y2 = C2#vector.y,
  Z2 = C2#vector.z,
  math:sqrt ( (X1 - X2) * (X1 - X2) +
              (Y1 - Y2) * (Y1 - Y2) +
              (Z1 - Z2) * (Z1 - Z2) );
%%
distance ({X1, Y1}, {X2, Y2}) ->
  math:sqrt ( (X1 - X2) * (X1 - X2) +
              (Y1 - Y2) * (Y1 - Y2)).


%%====================================================================
%% Function: cartesian2spheric/1
%%====================================================================
%% @spec cartesian2spheric (V::vector()) -> spheric()
%%
%% @doc Converts coordinates from cartesian to spheric.
%%
cartesian2spheric (C = #vector {}) ->
  Rho = math:sqrt (C#vector.x * C#vector.x +
                   C#vector.y * C#vector.y +
                   C#vector.z * C#vector.z),
  Phi = to_degrees (math:acos (C#vector.z / Rho)),
  Theta = to_degrees (math:atan2 (C#vector.y, C#vector.x)),
  #spheric { rho = Rho, phi = Phi, theta = Theta}.


%%====================================================================
%% Function: spheric2cartesian/1
%%====================================================================
%% @spec spheric2cartesian (S::spheric()) -> vector()
%%
%% @doc Converts coordinates from spheric to cartesian.
%%
spheric2cartesian (S = #spheric {}) ->
  X = S#spheric.rho *
    math:sin (to_radiants (S#spheric.phi)) *
    math:cos (to_radiants (S#spheric.theta)),

  Y = S#spheric.rho *
    math:sin (to_radiants (S#spheric.phi)) *
    math:sin (to_radiants (S#spheric.theta)),

  Z = S#spheric.rho * math:cos (to_radiants (S#spheric.phi)),

  #vector {x = X, y = Y, z = Z}.


%%====================================================================
%% Function: rotate/3
%%====================================================================
%% @private
rotate (V = #vector{}, Angle, Axis = #vector{}) ->
  RAngle = to_radiants (Angle),
  %% build rotation matrix
  C = math:cos(RAngle),
  S = math:sin(RAngle),
  IC = 1.0 - C,
  Icxx = IC * Axis#vector.x * Axis#vector.x,
  Icxy = IC * Axis#vector.x * Axis#vector.y,
  Icxz = IC * Axis#vector.x * Axis#vector.z,
  Icyy = IC * Axis#vector.y * Axis#vector.y,
  Icyz = IC * Axis#vector.y * Axis#vector.z,
  Iczz = IC * Axis#vector.z * Axis#vector.z,

  M00 = Icxx + C,
  M01 = Icxy + Axis#vector.z * S,
  M02 = Icxz - Axis#vector.y * S,

  M10 = Icxy - Axis#vector.z * S,
  M11 = Icyy + C,
  M12 = Icyz + Axis#vector.x * S,

  M20 = Icxz + Axis#vector.y * S,
  M21 = Icyz - Axis#vector.x * S,
  M22 = Iczz + C,

  #vector { x = M00*V#vector.x + M01*V#vector.y + M02*V#vector.z,
            y = M10*V#vector.x + M11*V#vector.y + M12*V#vector.z,
            z = M20*V#vector.x + M21*V#vector.y + M22*V#vector.z };


%%   AxisSph = cartesian2spheric (Axis),
%%   TowardsZ = rotate (C, 0, -AxisSph#spheric.phi),
%%   %% now vector C is such that its rotation axis is Z,
%%   %% so rotation plane is XY
%%   TowardsRot = rotate (TowardsZ, Angle, 0),
%%   %% retrasform to original coordinate system
%%   rotate (TowardsRot, 0, AxisSph#spheric.phi);
%%
rotate (C = #vector{}, Theta, Phi) ->
  spheric2cartesian (rotate (cartesian2spheric (C),
                             Theta, Phi));
%%
rotate (S = #spheric{}, Theta, Phi) ->
  S#spheric { theta = S#spheric.theta + Theta,
              phi = S#spheric.phi + Phi}.


%%====================================================================
%% Function: move/2
%%====================================================================
%% @private
move (C = #vector{}, O = #vector{}) ->
  #vector { x = C#vector.x + O#vector.x,
            y = C#vector.y + O#vector.y,
            z = C#vector.z + O#vector.z};
move (C = #vector{}, O) when is_number (O) ->
  spheric2cartesian (move (cartesian2spheric (C), O));
move (S = #spheric{}, O) when is_number (O) ->
  S#spheric { rho = S#spheric.rho + O }.



%%====================================================================
%% Func: to_radiants/1
%%====================================================================
%% @spec to_radiants(Angle::number()) -> number()
%%
%% @doc Coverts an angle from degree to radiants.
to_radiants (Angle) ->
  Angle / 180.0 * math:pi().


%%====================================================================
%% Func: to_degrees/1
%%====================================================================
%% @spec to_degrees(Angle::number()) -> number()
%%
%% @doc Coverts an angle from radiants to degree.
to_degrees (Angle) ->
  Angle / math:pi() * 180.0.


%%====================================================================
%% Func: normalize_angle/1
%%====================================================================
%% @spec normalize_angle(Angle::number()) -> number()
%%
%% @doc Normalizes an angle (in degrees) in the range -180, 180.
normalize_angle (A) when A > 180.0 ->
  normalize_angle (A - 360.0);
normalize_angle (A) when A < -180.0 ->
  normalize_angle (A + 360.0);
normalize_angle (A) ->
  A.


%%====================================================================
%% Func: line_for_one_point/4
%%====================================================================
line_for_one_point (X, Y, Orientation, SegmentWidth) ->
  %% compute line coefficients
  if
    abs (Orientation) == 90 ->
      M = {vertical, X},
      K = {vertical, X},
      XA = XB = X,
      YA = Y - (SegmentWidth / 2.0),
      YB = Y + (SegmentWidth / 2.0);
    true ->
      M = math:tan (geometry:to_radiants (Orientation)),
      K = Y - X * M,
      %% compute segment points
      XA = X -
        (SegmentWidth / 2.0) * math:cos (geometry:to_radiants (Orientation)),
      YA = XA * M + K,

      XB = XA + SegmentWidth * math:cos (geometry:to_radiants (Orientation)),
      YB = XB * M + K
  end,
  { {XA, YA}, {XB, YB}, #line { slope = M, intercept = K } }.


%%====================================================================
%% Func: line_for_one_point/3
%%====================================================================
line_for_one_point (X, Y, Orientation) ->
  %% compute line coefficients
  if
    abs (Orientation) == 90 ->
      M = {vertical, X},
      K = {vertical, X};
    true ->
      M = math:tan (geometry:to_radiants (Orientation)),
      K = Y - X * M
  end,
  #line { slope = M, intercept = K }.


%%====================================================================
%% Func: line_intersect/2
%%====================================================================
line_intersect (_R1 = #line { slope = {vertical, _}},
                _R2 = #line { slope = {vertical, _}}) -> {parallel};
%%
line_intersect (_R1 = #line { slope = M},
                _R2 = #line { slope = M}) -> {parallel};
%%
line_intersect (_R1 = #line { slope = {vertical, X}}, R2) ->
  {ok, {X, X * R2#line.slope + R2#line.intercept}};
%%
line_intersect (R1, _R2 = #line { slope = {vertical, X}}) ->
  {ok, {X, X * R1#line.slope + R1#line.intercept}};
%%
line_intersect (R1, R2) ->
  X =
    (R1#line.intercept - R2#line.intercept) /
    (R2#line.slope - R1#line.slope),
  {ok, {X, X * R1#line.slope + R1#line.intercept}}.
%%


%%====================================================================
%% Func: point_in_segment/3
%%====================================================================
point_in_segment ({XP, YP} ,
                  Segment = #wall { line = #line { slope = {vertical, X}}},
                  Tolerance) ->
  %% compute the intersection point of normal and wall
  XI = X,
  YI = YP,
  %% check if the intersection point is within segment limits
  case point_in_segment_extremes ({XI, YI},
                                  Segment#wall.point_a,
                                  Segment#wall.point_b) of
    true ->
      D = math:sqrt ( (XI - XP) * (XI - XP) +
                      (YI - YP) * (YI - YP) ),
      abs (D) < Tolerance;
    false ->
      false
  end;
%%
point_in_segment ({XP, YP} ,
                  Segment,
                  Tolerance) when (Segment#wall.line)#line.slope == 0 ->
  %% compute the intersection point of normal and wall
  XI = XP,
  {_, YI} = Segment#wall.point_a,
  %% check if the intersection point is within segment limits
  case point_in_segment_extremes ({XI, YI},
                                  Segment#wall.point_a,
                                  Segment#wall.point_b) of
    true ->
      D = math:sqrt ( (XI - XP) * (XI - XP) +
                      (YI - YP) * (YI - YP) ),
      abs (D) < Tolerance;
    false ->
      false
  end;
%%
point_in_segment ({XP, YP} ,
                  Segment,
                  Tolerance) ->
  M = (Segment#wall.line)#line.slope,
  K = (Segment#wall.line)#line.intercept,
  %% compute the normal
  M_normal = 1.0 / M,
  K_normal = YP - M_normal * XP,
  %% compute the intersection point of normal and wall
  XI = (K_normal - K) / (M - M_normal),
  YI = M * XI + K,
  %% check if the intersection point is within segment limits
  case point_in_segment_extremes ({XI, YI},
                                  Segment#wall.point_a,
                                  Segment#wall.point_b) of
    true ->
      D = math:sqrt ( (XI - XP) * (XI - XP) +
                      (YI - YP) * (YI - YP) ),
      abs (D) < Tolerance;
    false ->
      false
  end.
%%


%%
point_in_segment_extremes ({XP, YP},
                           {XA, YA},
                           {XB, YB}) when XA =< XB, YA =< YB ->
  (XA =< XP) and (XP =< XB) and (YA =< YP) and (YP =< YB);
%%
point_in_segment_extremes ({XP, YP},
                           {XA, YA},
                           {XB, YB}) when XA > XB, YA =< YB ->
  (XB =< XP) and (XP =< XA) and (YA =< YP) and (YP =< YB);
%%
point_in_segment_extremes ({XP, YP},
                           {XA, YA},
                           {XB, YB}) when XA =< XB, YA > YB ->
  (XA =< XP) and (XP =< XB) and (YB =< YP) and (YP =< YA);
%%
point_in_segment_extremes ({XP, YP},
                           {XA, YA},
                           {XB, YB}) when XA > XB, YA > YB ->
  (XB =< XP) and (XP =< XA) and (YB =< YP) and (YP =< YA).
%%
