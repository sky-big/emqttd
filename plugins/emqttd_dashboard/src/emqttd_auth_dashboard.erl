%%--------------------------------------------------------------------
%% Copyright (c) 2015-2016 Feng Lee <feng@emqtt.io>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc Dashboard authentication module
-module(emqttd_auth_dashboard).

-include("../../../include/emqttd.hrl").

-behaviour(emqttd_auth_mod).

-export([init/1, check/3, description/0]).

init(Opts) ->
    {ok, Opts}.

check(#mqtt_client{ws_initial_headers = undefined}, _Password, _Opts) ->
    ignore;

check(#mqtt_client{client_id = <<"dashboard_", _/binary>>,
                   username  = <<"dashboard">>,
                   ws_initial_headers = Headers}, _Password, [{_Proto, Port, _}]) ->
    Origin = proplists:get_value("Origin", Headers, ""),
    %%TODO: workaround first...
    case string:rstr(Origin, integer_to_list(Port)) of
        0  -> ignore;
        _I -> ok
    end;

check(_Client, _Password, _Opts) ->
    ignore.

description() ->
    "Dashboard Authentication Module".
