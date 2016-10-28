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

%% @doc Clients API.
-module(emqttd_dashboard_client).

-include("emqttd_dashboard.hrl").

-include("../../../include/emqttd.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([list/3]).

-http_api({"clients", list, [{"client_key", binary},
                             {"curr_page",  int, 1},
                             {"page_size",  int, 100}]}).

list(ClientId, PageNo, PageSize) when ?EMPTY_KEY(ClientId) ->
    TotalNum = ets:info(mqtt_client, size),
    Qh = qlc:q([R || R <- ets:table(mqtt_client)]),
    emqttd_dashboard:query_table(Qh, PageNo, PageSize, TotalNum, fun row/1);

list(ClientId, PageNo, PageSize) ->
    Fun = fun() -> ets:lookup(mqtt_client, ClientId) end,
    emqttd_dashboard:lookup_table(Fun, PageNo, PageSize, fun row/1).

row(#mqtt_client{client_id = ClientId,
                 peername = {IpAddr, Port},
                 username = Username,
                 clean_sess = CleanSess,
                 proto_ver = ProtoVer,
                 keepalive = KeepAlvie,
                 connected_at = ConnectedAt}) ->
    [{clientId, ClientId},
     {username, Username},
     {ipaddress, list_to_binary(emqttd_net:ntoa(IpAddr))},
     {port, Port},
     {clean_sess, CleanSess},
     {proto_ver, ProtoVer},
     {keepalive, KeepAlvie},
     {connected_at, list_to_binary(emqttd_dashboard:strftime(ConnectedAt))}].

