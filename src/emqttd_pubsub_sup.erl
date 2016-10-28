%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
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

%% @doc PubSub Supervisor.
-module(emqttd_pubsub_sup).

-behaviour(supervisor).

-include("emqttd.hrl").

-define(CONCURRENCY_OPTS, [{read_concurrency, true}, {write_concurrency, true}]).

%% API
-export([start_link/0, pubsub_pool/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [emqttd_broker:env(pubsub)]).

pubsub_pool() ->
    hd([Pid|| {pubsub_pool, Pid, _, _} <- supervisor:which_children(?MODULE)]).

init([Env]) ->

    %% Create ETS Tabs
	%% 创建订阅发布相关的ETS表
    create_tab(subscriber), create_tab(subscribed),

    %% Router
	%% emqttd_pubsub_sup监督进程下启动路由进程
    Router = {router, {emqttd_router, start_link, []},
                permanent, 5000, worker, [emqttd_router]},

    %% PubSub Pool Sup
	%% emqttd_pubsub_sup监督进程下启动发布订阅进程池树
    PubSubMFA = {emqttd_pubsub, start_link, [Env]},
    PubSubPoolSup = emqttd_pool_sup:spec(pubsub_pool, [pubsub, hash, pool_size(Env), PubSubMFA]),

    %% Server Pool Sup
	%% emqttd_pubsub_sup监督进程下启动emqttd_server进程池树
    ServerMFA = {emqttd_server, start_link, [Env]},
    ServerPoolSup = emqttd_pool_sup:spec(server_pool, [server, hash, pool_size(Env), ServerMFA]),

    {ok, {{one_for_all, 5, 60}, [Router, PubSubPoolSup, ServerPoolSup]}}.

pool_size(Env) ->
    Schedulers = erlang:system_info(schedulers),
    proplists:get_value(pool_size, Env, Schedulers).

%% 创建subscriber这个ETS表，存储主题对应的所有订阅该主题的emqttd_session进程
create_tab(subscriber) ->
    %% subscriber: Topic -> Pid1, Pid2, ..., PidN
    %% duplicate_bag: o(1) insert
    ensure_tab(subscriber, [public, named_table, duplicate_bag | ?CONCURRENCY_OPTS]);

%% 创建subscribed这个ETS表，存储emqttd_session进程订阅的所有主题
create_tab(subscribed) ->
    %% subscribed: Pid -> Topic1, Topic2, ..., TopicN
    %% bag: o(n) insert
    ensure_tab(subscribed, [public, named_table, bag | ?CONCURRENCY_OPTS]).

ensure_tab(Tab, Opts) ->
    case ets:info(Tab, name) of
        undefined -> ets:new(Tab, Opts);
        _ -> ok
    end.

