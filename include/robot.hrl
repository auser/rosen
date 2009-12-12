%
% robot.hrl
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


-record (robot, { type = two_wheels_driving,
                  name = noname,
                  object_name = noname,
                  structure = [],
                  wheel_radius = 0.0,
                  wheel_distance = 0.0,
                  wheel_ticks = 0.0,
                  motion_pid,
                  object_pid,
                  world,
                  sensors = []}).


-record (world, { name = noname,
                  width,
                  height,
                  color,
                  walls = []}).


-record (sensor, { type = undefined,
                   name = noname,
                   sensor_name = undefined,
                   robot = undefined,
                   robot_object = undefined,
                   world = undefined,
                   value = 0,
                   state = nil,
                   color = nil,
                   position,
                   parameters = []}).
