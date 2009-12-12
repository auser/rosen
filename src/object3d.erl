%
% object3d.erl
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
% $Id: object3d.erl,v 1.10 2008/02/29 15:44:10 corrado_santoro Exp $
%
%%
%% @doc Main module for handling 3D objects.
%%
%% <p>This module provides functions for creating a 3D object and
%% manipulating it in order to change its attributes like position,
%% orientation, color, etc.</p>
%%
%% <p>This is also a <b>behaviour</b>, since it can be used as the
%% general part for implementing new ad-hoc 3D objects to be manipulated
%% by ROSEN.</p>
%%
%% @type object3d() = #object3d{}.
%% A record with all parameters related to a 3D object.
%%
%% @type vector() = #vector{}.
%%
-module (object3d).
-behaviour(gen_server).

-include ("geometry.hrl").


-export ([new/1,
          behaviour_info/1,
          start_link/2,
          copy_default_axis/1,
          copy_default_up/1,
          add_activity/3,
          add_activity/4,
          activities/1,
          add_to_compound/2,
          obj/1,
          axis/1,
          axis/2,
          pids/2,
          object_for_tag/2,
          tag/1,
          default_axis/1,
          position/1,
          position/2,
          up/1,
          up/2,
          start_point/1,
          start_point/2,
          draw/1,
          stop/1]).

-export ([init/1, handle_call/3, terminate/2]).

-record (object3d_state, { module,
                           activities = [],
                           module_state }).


%%====================================================================
%% Function: new/1
%%====================================================================
%% @spec new(Obj::object3d()) -> {ok, Pid}
%%
%% @doc Creates a new 3D object as specified in the parameter.
%% <br/>
%% The object is handled has a new process linked with the calling
%% process. The function result is thus the pid of the spawned process.
%% If a name is specified in the #object3d record, the process is bound
%% to that name, so that, in all the functions of this module, the object
%% can be referred with either the pid or its registered name.
%%
new (Object = #object3d{}) ->
  M = Object#object3d.type,
  start_link (M, Object).
%%  M:start_link (Object).
%%



%%====================================================================
%% Function: behaviour_info/1
%% @private
%%====================================================================
behaviour_info (callbacks) ->
  [ {init, 1},
    {draw, 1},
    {terminate, 2}];
behaviour_info (_Other) ->
  undefined.



%%====================================================================
%% Func: start_link/2
%% @private
%%====================================================================
start_link (Module, Params = #object3d {}) ->
  if
    Params#object3d.name == noname ->
      {ok, Pid} = gen_server:start_link (?MODULE, [Module, Params], []);
    true ->
      {ok, Pid} = gen_server:start_link ({local, Params#object3d.name},
                                         ?MODULE,
                                         [Module, Params], [])
  end,
  rosen:add_object (Pid),
  {ok, Pid}.



%%====================================================================
%% Func: copy_default_axis/1
%% @private
%%====================================================================
copy_default_axis (Obj = #object3d{}) ->
  Axis =
    if
      Obj#object3d.axis == undefined -> Obj#object3d.default_axis;
      true -> Obj#object3d.axis
    end,
  Obj#object3d { axis = Axis }.



%%====================================================================
%% Func: copy_default_up/1
%% @private
%%====================================================================
copy_default_up (Obj = #object3d{}) ->
  Axis =
    if
      Obj#object3d.up == undefined -> Obj#object3d.default_up;
      true -> Obj#object3d.up
    end,
  Obj#object3d { up = Axis }.



%%====================================================================
%% Func: add_activity/3
%%====================================================================
%% @spec add_activity(ObjPid::pid(), Module::atom(),
%%       InitialParams::property_list()) -> {ok, ActivityPid}
%%
%% @doc Associates an activity to a 3D object.
%% <br/>
%% The <code>ObjPid</code> represents the object to which
%% the activity has to be associated;
%% <code>Module</code> is the name of the module implementing the
%% activity (using <code>gen_activity</code> behaviour);
%% <code>InitialParams</code>
%% is a property list which provides the initial values of the
%% activity parameters.
%% <br/>
%% It returns the pid of the process executing this activity.
%%
add_activity (Pid, ActivityModule, InitialParams) ->
  gen_server:call (Pid, {add_activity, ActivityModule, noname, InitialParams}).



%%====================================================================
%% Func: add_activity/4
%%====================================================================
%% @spec add_activity(ObjPid::pid(), Module::atom(), Name::term(),
%%       InitialParams::property_list()) -> {ok, ActivityPid}
%%
%% @doc Associates an activity to a 3D object.
%% <br/>
%% Same as <code>add_activity/3</code>, but registering with
%% <code>Name</code> the process executing this activity.
add_activity (Pid, ActivityModule, ActivityName, InitialParams) ->
  gen_server:call (Pid, {add_activity, ActivityModule,
                         ActivityName, InitialParams}).



%%====================================================================
%% Func: activities/1
%%====================================================================
%% @spec activities(ObjPid::pid()) -> [pid()]
%%
%% @doc Gets the list of activities associated to a 3D object.
%%
activities (Pid) ->
  gen_server:call (Pid, {get_activities}).



%%====================================================================
%% Func: add_to_compound/2
%%====================================================================
%% @spec add_to_compound(ObjPid::pid(), Obj::object3d()) -> ok
%%
%% @doc Adds a new component to a compound object.
%%
add_to_compound (Pid, Object) ->
  gen_server:call (Pid, {add_to_compound, Object}).



%%====================================================================
%% Func: draw/1
%%====================================================================
%% @spec draw(ObjPid::pid()) -> ok
%%
%% @doc Draws the object.
%% <br/>
%% This function is automatically called by the ROSEN engine
%% to draw the object. In turn, it calls the draw/1 callback of the
%% code implementing the drawing routines for the specific object.
%%
draw (Pid) ->
  gen_server:call (Pid, {draw}).



%%====================================================================
%% Func: obj/1
%%====================================================================
%% @spec obj(ObjPid::pid()) -> object3d()
%%
%% @doc Gets the object info.
%% <br/>
%% Returns an #object3d record with the whole description
%% of the object represented by the given pid or name.
%%
obj (Pid) ->
  gen_server:call (Pid, {obj}).


%%====================================================================
%% Func: position/1
%%====================================================================
%% @spec position(ObjPid::pid()) -> vector()
%%
%% @doc Gets the <i>(x,y,z)</i> position of an object.
%%
position (Pid) ->
  gen_server:call (Pid, {get_position}).


%%====================================================================
%% Func: position/2
%%====================================================================
%% @spec position(ObjPid::pid(), Position::vector()) -> ok
%%
%% @doc Sets the <i>(x,y,z)</i> position of an object.
%%
position (Pid, Pos) ->
  gen_server:call (Pid, {set_position, Pos}).


%%====================================================================
%% Func: pids/2
%%====================================================================
%% @private
pids (Pid, Pids) ->
  gen_server:call (Pid, {set_pids, Pids}).


%%====================================================================
%% Func: object_for_tag/2
%%====================================================================
%% @private
object_for_tag (Pid, Tag) ->
  gen_server:call (Pid, {object_for_tag, Tag}).


%%====================================================================
%% Func: tag/1
%%====================================================================
%% @private
tag (Pid) ->
  gen_server:call (Pid, {tag}).


%%====================================================================
%% Func: axis/1
%%====================================================================
%% @spec axis(ObjPid::pid()) -> vector()
%%
%% @doc Gets the axis of an object.
%%
axis (Pid) ->
  gen_server:call (Pid, {get_axis}).



%%====================================================================
%% Func: default_axis/1
%%====================================================================
%% @private
default_axis (Pid) ->
  gen_server:call (Pid, {get_default_axis}).



%%====================================================================
%% Func: axis/2
%%====================================================================
%% @spec axis(ObjPid::pid(), NewAxis::vector()) -> ok
%%
%% @doc Changes the axis of an object.
%%
axis (Pid, NewAxis) ->
  gen_server:call (Pid, {set_axis, NewAxis}).



%%====================================================================
%% Func: up/1
%%====================================================================
%% @spec up(ObjPid::pid()) -> vector()
%%
%% @doc Gets the up direction of an object.
%%
up (Pid) ->
  gen_server:call (Pid, {get_up}).



%%====================================================================
%% Func: up/2
%%====================================================================
%% @spec up(ObjPid::pid(), NewUp::vector()) -> ok
%%
%% @doc Changes the up direction of an object.
%%
up (Pid, NewAxis) ->
  gen_server:call (Pid, {set_up, NewAxis}).



%%====================================================================
%% Func: start_point/1
%%====================================================================
%% @spec start_point(ObjPid::pid()) -> vector()
%%
%% @doc Gets the starting point of a line.
%%
start_point (Pid) ->
  gen_server:call (Pid, {get_start_point}).



%%====================================================================
%% Func: start_point/2
%%====================================================================
%% @spec start_point(ObjPid::pid(), NewPt::vector()) -> ok
%%
%% @doc Gets the starting point of a line.
%%
start_point (Pid, St) ->
  gen_server:call (Pid, {set_start_point, St}).



%%====================================================================
%% Func: end_point/1
%%====================================================================
%% @spec end_point(ObjPid::pid()) -> vector()
%%
%% @doc Gets the ending point of a line.
%%
end_point (Pid) ->
  gen_server:call (Pid, {get_end_point}).



%%====================================================================
%% Func: end_point/2
%%====================================================================
%% @spec end_point(ObjPid::pid(), NewPt::vector()) -> ok
%%
%% @doc Gets the ending point of a line.
%%
end_point (Pid, End) ->
  gen_server:call (Pid, {set_end_point, End}).



%%====================================================================
%% Func: stop/1
%%====================================================================
%%
%% @spec stop(Pid::pid()) -> ok
%%
%% @doc Destroys the object.
%%
stop (Pid) ->
  gen_server:call (Pid, {stop}).



%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: handle_call/3
%% @private
%%====================================================================
handle_call ({stop}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  {stop, normal, ok, State};
%%
handle_call ({add_to_compound, NewObject}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,

  NewComponentPid = compound:start_component (ModuleState, NewObject),

  NewModuleState =
    ModuleState#object3d
      { pids = [NewComponentPid | ModuleState#object3d.pids] },

  {reply, ok, State#object3d_state { module_state = NewModuleState} };
%%
handle_call ({add_activity, Activity, ActivityName, ActivityParams}, _,
             State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  if
    ActivityName == noname ->
      {ok, Pid} = gen_activity:start_link (Activity,
                                           ActivityParams, self (),
                                           State#object3d_state.module_state);
    true ->
      {ok, Pid} = gen_activity:start_link (ActivityName, Activity,
                                           ActivityParams, self (),
                                           State#object3d_state.module_state)
    end,
  NewState = State#object3d_state
               { activities = [Pid | State#object3d_state.activities] },
  {reply, {ok, Pid}, NewState};
%%
handle_call ({obj}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  {reply, ModuleState, State};
%%
handle_call ({get_start_point}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  Pt = ModuleState#object3d.start_point,
  {reply, Pt, State};
%%
handle_call ({set_start_point, Pt}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  NewState = ModuleState#object3d { start_point = Pt },
  {reply, ok, State#object3d_state { module_state = NewState }};
%%
handle_call ({get_end_point}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  Pt = ModuleState#object3d.end_point,
  {reply, Pt, State};
%%
handle_call ({set_end_point, Pt}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  NewState = ModuleState#object3d { end_point = Pt },
  {reply, ok, State#object3d_state { module_state = NewState }};
%%
handle_call ({get_position}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  Pos = ModuleState#object3d.position,
  {reply, Pos, State};
%%
handle_call ({set_position, Pos}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  NewState = ModuleState#object3d { position = Pos },
  {reply, ok, State#object3d_state { module_state = NewState }};
%%
handle_call ({get_default_axis}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  Speed = ModuleState#object3d.default_axis,
  {reply, Speed, State};
%%
handle_call ({get_axis}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  Axis = ModuleState#object3d.axis,
  {reply, Axis, State};
%%
handle_call ({set_axis, NewAxis}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  NewState = ModuleState#object3d { axis = NewAxis },
  {reply, ok, State#object3d_state { module_state = NewState }};
%%
handle_call ({get_up}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  Up = ModuleState#object3d.up,
  {reply, Up, State};
%%
handle_call ({set_up, NewAxis}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  NewState = ModuleState#object3d { up = NewAxis },
  {reply, ok, State#object3d_state { module_state = NewState }};
%%
handle_call ({tag}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  Tag = ModuleState#object3d.tag,
  {reply, Tag, State};
%%
%%
%%   OldAxis = ModuleState#object3d.axis,
%%   RotAngle = geometry:angle (OldAxis, NewAxis),
%%   RotVector = geometry:cross (OldAxis, NewAxis),
%%   Norm = geometry:norm (RotVector),
%% %%   io:format ("~p~n~p~nRotVect:~p~nNorm: ~p, ~p~n",
%% %%              [OldAxis, NewAxis,
%% %%               RotVector, Norm, RotAngle]),
%%   if
%%     Norm =/= 0.0 ->
%%       lists:foreach (fun (Pid) ->
%%                          %% first rotate the axis
%%                          A = object3d:axis (Pid),
%%                          RotA = geometry:rotate (A, RotAngle, RotVector),
%%                          object3d:axis (Pid, RotA)
%%                          %% now set the new position
%% %%                          Pos = object3d:position (Pid),
%% %%                          PosR = geometry:rotate (Pos, RotAngle, RotVector),
%% %%                          object3d:position (Pid, PosR)
%%                      end,
%%                      ModuleState#object3d.pids),
%%       NewState = ModuleState#object3d { axis = NewAxis };
%%     true ->
%%       NewState = ModuleState
%%   end,
%%   %%io:format ("~p, ~p, ~p~n", [self(), OldAxis, NewAxis]),
%%   {reply, ok, State#object3d_state { module_state = NewState }};
%%
handle_call ({set_pids, Pids}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  NewState = ModuleState#object3d { pids = Pids },
  {reply, ok, State#object3d_state { module_state = NewState }};
%%
handle_call ({object_for_tag, Tag}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  TagList = [X ||
              X <- ModuleState#object3d.pids,
              object3d:tag(X) == Tag],
  {reply, TagList, State};
%%
handle_call ({draw}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  {ok, NewState} = Module:draw (ModuleState),
  {reply, ok, State#object3d_state { module_state = NewState }};
%%
handle_call ({get_activities}, _, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  {reply, State#object3d_state.activities, State}.




%%====================================================================
%% Func: init/1
%% @private
%%====================================================================
init ([Module, Params]) ->
  {ok, ModuleState} = Module:init (Params#object3d { pid = self() }),
  {ok,
   #object3d_state { module = Module,
                     module_state = ModuleState }
  }.



%%====================================================================
%% Func: terminate/2
%% @private
%%====================================================================
terminate (Reason, State) ->
  Module = State#object3d_state.module,
  ModuleState = State#object3d_state.module_state,
  lists:foreach (fun (P) ->
                     gen_activity:stop (P)
                 end,
                 State#object3d_state.activities),
  Module:terminate (Reason, ModuleState),
  ok.

