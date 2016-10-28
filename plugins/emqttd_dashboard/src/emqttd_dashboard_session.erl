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

%% @doc Session API.
-module(emqttd_dashboard_session).

-include("emqttd_dashboard.hrl").

-include("../../../include/emqttd.hrl").

-include_lib("stdlib/include/qlc.hrl").

-import(proplists, [get_value/2]).

-export([list/3]).

-http_api({"sessions", list, [{"client_key", binary},
                              {"curr_page",  int, 1},
                              {"page_size",  int, 100}]}).

list(ClientId, PageNo, PageSize) when ?EMPTY_KEY(ClientId) ->
    TotalNum = lists:sum([ets:info(Tab, size) || Tab <- tables()]),
    Qh = qlc:append([qlc:q([E || E <- ets:table(Tab)]) || Tab <- tables()]),
    emqttd_dashboard:query_table(Qh, PageNo, PageSize, TotalNum, fun row/1);

list(ClientId, PageNo, PageSize) ->
    MP = {{ClientId, '_'}, '_'},
    Fun = fun() -> lists:append([ets:match_object(Tab, MP) || Tab <- tables()]) end,
    emqttd_dashboard:lookup_table(Fun, PageNo, PageSize, fun row/1).

tables() ->
    [mqtt_transient_session, mqtt_persistent_session].

row({{ClientId, _Pid}, Session}) ->
    InfoKeys = [clean_sess, max_inflight, inflight_queue, message_queue,
                message_dropped, awaiting_rel, awaiting_ack, awaiting_comp, created_at],
     [{clientId, ClientId} | [{Key, format(Key, get_value(Key, Session))} || Key <- InfoKeys]].

format(created_at, Val) ->
    list_to_binary(emqttd_dashboard:strftime(Val));
format(_, Val) ->
    Val.

