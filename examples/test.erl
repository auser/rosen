%
% test.erl
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
-module (test).
-export ([objects/0, compounds/0, robot/0, mill/0, c/0]).

-include ("geometry.hrl").

objects () ->
  rosen:start_link(),
  object3d:new (#object3d { type = surface,
                            name = mybox,
                            width = 10.0,
                            depth = 10.0,
                            position = ?VECTOR (0.0, -2.0, 0.0),
                            color = ?RGB(0.25,0.25,0.25)}),
  object3d:new (#object3d { type = cube,
                            name = mycube,
                            position = ?VECTOR (1.0, 2.0, 1.0),
                            color = ?RGB(0.5,0.5,0.5)}),
  object3d:new (#object3d { type = cylinder,
                            name = mycyl,
                            position = ?VECTOR (-1.0, 0.75, 1.0),
                            color = ?RGB (0.25, 1.0, 0.25)}),
  object3d:new (#object3d { type = sphere,
                            name = ciccio,
                            radius = 0.5,
                            position = ?VECTOR (-1.0, 2.0, 1.0),
                            color = ?RGB (1.0, 0.25, 0.25)}),
  object3d:new (#object3d { type = cone,
                            name = icecream,
                            position = ?VECTOR (-2.0, 2.0, 1.0),
                            base_radius = 1.0,
                            size = 2.0,
                            axis = ?VECTOR (0.0, 1.0, 0.0),
                            color = ?RGB (0.25, 0.25, 1.0)}),
  object3d:new (#object3d { type = cone,
                            name = icecream2,
                            position = ?VECTOR (-2.0, -2.0, 2.0),
                            base_radius = 1.0,
                            top_radius = 0.2,
                            size = 0.5,
                            color = ?RGB (0.25, 1.0, 1.0)}),
  rotate (0).


rotate (Angle) ->
  Axis1 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 phi = 90.0,
                                                 theta = Angle }),
  Axis2 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 theta = 0.0,
                                                 phi = -Angle }),
  Axis3 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 phi = 45.0,
                                                 theta = Angle }),
  Axis4 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 theta = 0.0,
                                                 phi = Angle }),
  object3d:axis (mycube, Axis1),
  object3d:axis (mycyl, Axis2),
  object3d:axis (icecream2, Axis2),
  object3d:axis (icecream, Axis3),
  object3d:axis (mybox, Axis4),
  timer:sleep (10),
  rotate (Angle + 1).


compounds () ->
  rosen:start_link(),
  A = #object3d { type = surface,
                  name = mybox,
                  width = 10.0,
                  depth = 10.0,
                  position = ?VECTOR (0.0, -2.0, 0.0),
                  color = ?RGB(0.25,0.25,0.25)},
  B = #object3d { type = cube,
                  name = mycube,
                  position = ?VECTOR (1.0, 2.0, 1.0),
                  color = ?RGB(0.5,0.5,0.5)},
  C = #object3d { type = cylinder,
                  name = mycyl,
                  position = ?VECTOR (-1.0, 0.75, 1.0),
                  color = ?RGB (0.25, 1.0, 0.25)},
  D = #object3d { type = sphere,
                  name = ciccio,
                  radius = 0.5,
                  position = ?VECTOR (-1.0, 2.0, 1.0),
                  color = ?RGB (1.0, 0.25, 0.25)},
  E = #object3d { type = cone,
                  name = icecream,
                  position = ?VECTOR (-2.0, 2.0, 1.0),
                  base_radius = 1.0,
                  size = 2.0,
                  axis = ?VECTOR (0.0, 1.0, 0.0),
                  color = ?RGB (0.25, 0.25, 1.0)},
  F = #object3d { type = cone,
                  name = icecream2,
                  position = ?VECTOR (-2.0, -2.0, 2.0),
                  base_radius = 1.0,
                  top_radius = 0.2,
                  size = 0.5,
                  color = ?RGB (0.25, 1.0, 1.0)},

  object3d:new (#object3d { type = compound,
                            name = test_compound,
                            objects = [A, B, C, D, E, F] }),
  compound_rotate (0).


compound_rotate (Angle) ->
  Axis1 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 phi = 90.0,
                                                 theta = Angle }),
  Axis2 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 theta = 0.0,
                                                 phi = -Angle }),
  Axis3 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 phi = 45.0,
                                                 theta = Angle }),
  Axis4 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 theta = 0.0,
                                                 phi = Angle }),
  object3d:axis (test_compound_mycube, Axis1),
  object3d:axis (test_compound_mycyl, Axis2),
  object3d:axis (test_compound_icecream2, Axis2),
  object3d:axis (test_compound_icecream, Axis3),
  object3d:axis (test_compound, Axis4),
  timer:sleep (10),
  compound_rotate (Angle + 1).


robot () ->
  rosen:start_link(),
  object3d:new (#object3d { type = surface,
                            name = mybox,
                            width = 40.0,
                            depth = 40.0,
                            position = ?VECTOR (0.0, -1.3, 0.0),
                            color = ?RGB(0.25,0.25,0.25)}),

  Base = #object3d { type = cube,
                     size = 2.0,
                     position = ?VECTOR (0.0, 0.0, 0.0) },
  LeftW = #object3d { type = cylinder,
                      radius = 0.3,
                      axis = ?VECTOR (1.0, 0, 0.0),
                      size = 0.1,
                      color = ?RGB(0, 1.0, 0),
                      position = ?VECTOR (-1.05, -1.0, -0.6) },
  RightW = #object3d { type = cylinder,
                       radius = 0.3,
                       axis = ?VECTOR (1.0, 0, 0.0),
                       size = 0.1,
                       color = ?RGB(0, 1.0, 0),
                       position = ?VECTOR (0.95, -1.0, -0.6) },
  Sphere = #object3d { type = sphere,
                       position = ?VECTOR (0.0, -1.0, 0.7),
                       radius = 0.3},

  object3d:new (#object3d { type = compound,
                            name = robot1,
                            objects = [Base, LeftW, RightW, Sphere] }),

  object3d:add_activity ( robot1, kinematics, robot1_k,
                          [{v, 10}, {omega, 80}, {plane, xz}]),

  object3d:new (#object3d { type = compound,
                            name = robot2,
                            objects = [Base, LeftW, RightW, Sphere] }),

  object3d:add_activity ( robot2, kinematics, robot2_k,
                          [{v, 10}, {omega, -70}, {plane, xz}]),

  ok.



mill () ->
  rosen:start_link(),

  C = #object3d { type = cylinder,
                  name = mycyl,
                  size = 0.2,
                  radius = 0.2,
                  position = ?VECTOR (0.0, 0.0, -0.1),
                  color = ?RGB (0.25, 1.0, 0.25)},

  X1 = 0.31 * math:cos (geometry:to_radiants (45.0 / 2.0)),
  Y1 = 0.31 * math:sin (geometry:to_radiants (45.0 / 2.0)),
  C1 = #object3d { type = sphere,
                   radius = 0.095,
                   position = ?VECTOR (X1, -Y1, 0),
                   color = ?RGB (1.0, 0.0, 0.0)},

  C2 = #object3d { type = pipe,
                   size = 0.1,
                   radius = 0.41,
                   position = ?VECTOR (0.0, 0.0, -0.05)},

  P = lists:map (
        fun (Angle) ->
            X = 0.1 * math:cos (geometry:to_radiants (Angle)),
            Y = 0.1 * math:sin (geometry:to_radiants (Angle)),
            #object3d { type = box,
                        height = 0.6,
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

  object3d:new (#object3d { type = compound,
                             name = threadmill,
                             objects = [C, C1, C2 | P] }),
  roto_mill (0.0, threadmill).


roto_mill (Angle, Name) ->
  Axis = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                phi = 90.0,
                                                theta = Angle }),
  object3d:up (Name, Axis),
  timer:sleep (5),
  roto_mill (Angle + 1, Name).




c () ->
  rosen:start_link(),
  Base = #object3d { type = cube,
                     name = mycube,
                     size = 2.0,
                     position = ?VECTOR (0.0, 0.0, 0.0) },
  Stick = #object3d { type = box,
                      name = mybox,
                      height = 0.6,
                      width = 0.05,
                      depth = 0.1,
                      position = ?VECTOR (0.0, 0.0, 0.0),
                      color = ?RGB(0.0, 1.0, 0.0)},
  StickComp = #object3d { type = compound,
                          name = b,
                          position = ?VECTOR (0.0,0.0, 0.0),
                          objects = [Stick] },
  object3d:new (#object3d { type = compound,
                            name = a,
                            position = ?VECTOR (1.0, 1.0, 1.0),
                            objects = [Base, StickComp] }),
  ok.

