%
% activity.erl
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
%% @doc This behaviour provides an abstraction for object activities.
%%      A gen_activity allows an object "to do" something during the
%%      simulation, and alla activities can be performed in parallel.
%%      For example, if the robot has an helic which is a compound
%%      Object3d(), the right way to make the helic rotating is adding
%%      an activity "rotate_helic" which uses the gen_activity behaviour
%%      and takes care of helic rotation at each step.
%%      Each activity is passed the current state of the object it is
%%      applied to, so that it is possible to modify objects properties
%%      (position, colors, etc...) as a result of the activity. <br/>
%%      The callback module should provide those functions:<br/>
%%      <ul>
%%      <li>init/2</li>
%%      <li>step/5</li>
%%      <li>get_property/3</li>
%%      <li>set_property/4</li>
%%      </ul>
%%      which are used, respectively, to perform a step of the activity and
%%      to get/set activity properties. <br/>
%%      <br/>
%%      Some other modules of ROSEN, for example <i>kinematics</i>,
%%      <i>simple_path</i> and <i>diff_drive</i> are
%%      implemented as activities, since the movement of an object
%%      is, actually, one of the main activities of an object in a 3d sim.
%%
%%
-module (gen_activity).
-behaviour(gen_server).

-include ("geometry.hrl").

-export ([behaviour_info/1,
          start_link/4,
          start_link/5,
          step/2,
          get_property/2,
          set_property/3,
          module_state/1,
          stop/1]).

-export ([init/1, handle_call/3, terminate/2]).

-record (activity_state, { module,
                           module_state,
                           associated_object,
                           time,
                           delta_time,
                           last_time} ).


%%====================================================================
%% Function: behaviour_info/1
%%====================================================================
%% @private
behaviour_info (callbacks) ->
  [ {init, 2},
    {step, 5},
    {get_property, 3},
    {set_property, 4},
    {terminate, 2}];
behaviour_info (_Other) ->
  undefined.



%%====================================================================
%% Func: start_link/3
%%====================================================================
%%
%% @spec start_link(Module, Params, ObjectPid, Object) -> Result
%%       Module = atom()
%%       Params = [term()]
%%       ObjectPid = pid()
%%       Object = object3d()
%%       Result = {ok, Pid} | {error, Reason} | undefined
%% @doc create an activity for an object.
%%      Module is the name of the module where callbacks are implemented.
%%      Params is the list of params to be passed to the <pre>Module:init</pre>
%%      function <code>ObjectPid</code> is the pid of the Object3d to which
%%      this activity applies, while <code>Object</code> must be the complete
%%      description of the object3d.<br/>
%%      This function is not called directly, you may use
%%      <pre>object3d:add_activity</pre> to add an activity to a given
%%      object.
%%
%%
start_link (Module, Params, ObjectPid, Object) ->
  gen_server:start_link (?MODULE, [Module, Params, ObjectPid, Object], []).


%%====================================================================
%% Func: start_link/4
%%====================================================================
%% @spec start_link(Name, Module, Params, ObjectPid, Object) -> Result
%%       Name = atom()
%%       Module = atom()
%%       Params = [term()]
%%       ObjectPid = pid()
%%       Object = object3d()
%%       Result = {ok, Pid} | {error, Reason} | undefined
%% @doc create an activity for an object, registering it with local name Name.
%%      Name = the name of the activity
%%      Module is the name of the module where callbacks are implemented.
%%      Params is the list of params to be passed to the <pre>Module:init</pre>
%%      function <code>ObjectPid</code> is the pid of the Object3d to which
%%      this activity applies, while <code>Object</code> must be the complete
%%      description of the object3d.<br/>
%%      This function is not called directly, you may use
%%      <pre>object3d:add_activity</pre> to add an activity to a given
%%      object.
%%
start_link (Name, Module, Params, ObjectPid, Object) ->
  gen_server:start_link ({local, Name}, ?MODULE,
                         [Module, Params, ObjectPid, Object], []).


%%====================================================================
%% Func: step/2
%%====================================================================
%% @spec step(Pid, ObjectState) -> Reply
%%       Pid = pid()
%%       ObjectState = object3d()
%%       Reply = term()
%% @doc Perform a step of an activity. The step acts on the ObjectState of
%%      the object whose pid is Pid.
%%      Reply is a term returned by the activity. Note that activities are
%%      performed on a step-by-step base: whenever an object3d() is updated,
%%      all its activities perform a step.
%%
%%
step (Pid, ObjectState) ->
  %%io:format ("step ~p~n", [Pid]),
  gen_server:call (Pid, {step, ObjectState}).



%%====================================================================
%% Func: get_property/2
%%====================================================================
%%
%% @spec get_property(Pid, PropName) -> Result
%%       Pid = pid()
%%       PropName = term()
%%       Result = {value, {PropName, PropValue}} | {error, noprop} | CustomRes
%%       PropValue = term()
%%       CustomRes = term()
%% @doc Get the property PropName of the activity at pid Pid. The result
%%      should be {error, noprop} if the property does ot exist, and should
%%      be {value, {PropName, PropVal}} if the property exists. Also a
%%      custom CustomRes term can be returned.
%%
get_property (Pid, PropName) ->
  gen_server:call (Pid, {get_property, PropName}).



%%====================================================================
%% Func: set_property/3
%%====================================================================
%%
%% @spec set_property(Pid, PropName, PropertyValue) -> Result
%%       Pid = pid()
%%       PropName = term()
%%       PropValue = term()
%%       Result = ok | {error, noprop}
%%       PropVal = term()
%% @doc Set the property PropName of the activity at pid Pid assigning it to
%%      value PropValue. If the property esists, it is set to the new value
%%      and the result is {ok}. Otherwise, the callback function should return
%%      {error, noprop}
%%
set_property (Pid, PropName, PropertyValue) ->
  gen_server:call (Pid, {set_property, PropName, PropertyValue}).



%%====================================================================
%% Func: module_state/1
%%====================================================================
%%
%% @spec module_state(Pid) -> Result
%%       Pid = pid()
%%       Result = term()
%% @doc Returns the state of the specific module associated to
%%      this gen_activity.
%%
module_state (Pid) ->
  gen_server:call (Pid, {module_state}).



%%====================================================================
%% Func: stop/1
%%====================================================================
%%
%% @spec stop(Pid) -> ok
%%       Pid = pid()
%%
%% @doc Immediately stop the activity with pid Pid, calling the
%%      <code>terminate/2</code> callback function, which
%%      should return ok.
%%
stop (Pid) ->
  gen_server:call (Pid, {stop}).



%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: handle_call/3
%%====================================================================
%% @private
handle_call ({stop}, _, State) ->
  Module = State#activity_state.module,
  ModuleState = State#activity_state.module_state,
  {stop, normal, ok, State};
%%
handle_call ({module_state}, _, State) ->
  ModuleState = State#activity_state.module_state,
  {reply, ModuleState, State};
%%
handle_call ({get_property, PropName}, _, State) ->
  Module = State#activity_state.module,
  ModuleState = State#activity_state.module_state,
  Value = Module:get_property (State#activity_state.associated_object,
                               PropName,
                               ModuleState),
  {reply, Value, State};
%%
handle_call ({set_property, PropName, Value}, _, State) ->
  Module = State#activity_state.module,
  ModuleState = State#activity_state.module_state,
  NewState = Module:set_property (State#activity_state.associated_object,
                                  PropName,
                                  Value,
                                  ModuleState),
  {reply, ok, State#activity_state { module_state = NewState }};
%%
handle_call ({step, ObjectState}, _, State) ->
  Now = now (),
  Delta = timer:now_diff (Now, State#activity_state.last_time) / 1000000.0,
  %% difference in seconds
  Module = State#activity_state.module,
  ModuleState = State#activity_state.module_state,
  NewTime = State#activity_state.time + Delta,
  {ok, NewState} = Module:step (State#activity_state.associated_object,
                                ObjectState,
                                NewTime,
                                Delta,
                                ModuleState),
  {reply, ok,
   State#activity_state {
     module_state = NewState,
     last_time = Now,
     delta_time = Delta,
     time = NewTime
    }}.



%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
init ([Module, Params, ObjectPid, Object]) ->
  {ok, ModuleState} = Module:init (Object, Params),
  {ok, #activity_state { module = Module,
                         module_state = ModuleState,
                         associated_object = ObjectPid,
                         time = 0.0,
                         delta_time = 0.01,
                         last_time = now ()}}.



%%====================================================================
%% Func: terminate/2
%%====================================================================
%% @private
terminate (Reason, State) ->
  Module = State#activity_state.module,
  ModuleState = State#activity_state.module_state,
  Module:terminate (Reason, ModuleState),
  ok.

