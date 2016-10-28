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

%% @doc Subscriptions API.
-module(emqttd_dashboard_subscription).

-include("emqttd_dashboard.hrl").

-include("../../../include/emqttd.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([list/3]).

-http_api({"subscriptions", list, [{"client_key", binary},
                                   {"curr_page",  int, 1},
                                   {"page_size",  int, 100}]}).

list(ClientId, PageNo, PageSize) when ?EMPTY_KEY(ClientId) ->
    TotalNum = ets:info(subscription, size),
    Qh = qlc:q([E || E <- ets:table(subscription)]),
    emqttd_dashboard:query_table(Qh, PageNo, PageSize, TotalNum, fun row/1);

list(ClientId, PageNo, PageSize) ->
    Fun = fun() -> ets:lookup(subscription, ClientId) end,
    emqttd_dashboard:lookup_table(Fun, PageNo, PageSize, fun row/1).

row(#mqtt_subscription{subid = ClientId, topic = Topic, qos = Qos}) ->
    [{clientid, ClientId}, {topic, Topic}, {qos, Qos}].

