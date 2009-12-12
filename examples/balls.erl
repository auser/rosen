%
% balls.erl
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
% $Id: balls.erl,v 1.2 2007/11/03 23:20:17 corrado_santoro Exp $
%
-module (balls).
-export ([go/0]).

-include ("geometry.hrl").

go () ->
  rosen:start_link(),
  object3d:new (#object3d { type = surface,
                            name = mybox,
                            width = 10.0,
                            depth = 10.0,
                            position = ?VECTOR (0.0, -3.0, 0.0),
                            color = ?RGB(0.25,0.25,0.25)}),

  SP1 = #object3d { type = sphere,
                    radius = 0.3,
                    position = ?VECTOR (0.0, 1.0, -2.0),
                    color = ?RGB (1.0, 0.25, 0.25)},

  SP2 = #object3d { type = sphere,
                    radius = 0.3,
                    position = ?VECTOR (1.0, 0.0, -1.0),
                    color = ?RGB (0.25, 1.0, 0.25)},

  SP3 = #object3d { type = sphere,
                    radius = 0.3,
                    position = ?VECTOR (0.0, -2.0, 0.0),
                    color = ?RGB (0.25, 0.25, 1.0)},

  SP4 = #object3d { type = sphere,
                    radius = 0.3,
                    position = ?VECTOR (-2.0, 0.0, 1.0),
                    color = ?RGB (1.0, 1.0, 0.25)},

  object3d:new (#object3d { type = compound,
                            name = compound1,
                            objects = [SP1]}),

  object3d:new (#object3d { type = compound,
                            name = compound2,
                            objects = [SP2]}),

  object3d:new (#object3d { type = compound,
                            name = compound3,
                            objects = [SP3]}),

  object3d:new (#object3d { type = compound,
                            name = compound4,
                            objects = [SP4]}),

  rotate (0).


rotate (Angle) ->
  Axis1 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 phi = 90.0,
                                                 theta = Angle }),
  Axis2 = geometry:spheric2cartesian (#spheric { rho = 1.0,
                                                 phi = 90.0,
                                                 theta = -Angle }),
  object3d:up (compound1, Axis1),
  object3d:up (compound2, Axis2),
  object3d:up (compound3, Axis1),
  object3d:up (compound4, Axis2),
  timer:sleep (10),
  rotate (geometry:normalize_angle (Angle + 1)).


