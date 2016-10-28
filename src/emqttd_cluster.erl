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

-module(emqttd_cluster).

-include("emqttd.hrl").

%% Cluster API
-export([join/1, leave/0, status/0, remove/1]).

%% RPC Call
-export([prepare/0, reboot/0]).

%% @doc Join cluster
-spec(join(node()) -> ok | {error, any()}).
join(Node) when Node =:= node() ->
	{error, {cannot_join_with_self, Node}};

join(Node) when is_atom(Node) ->
	case {is_clustered(Node), emqttd:is_running(Node)} of
		{false, true} ->
			%% 加入集群的时候提前准备，将插件卸载，同时停止对应的应用
			prepare(), ok = emqttd_mnesia:join_cluster(Node), reboot();
		{false, false} ->
			{error, {node_not_running, Node}};
		{true, _} ->
			{error, {already_clustered, Node}}
	end.

%% @doc Prepare to join or leave cluster.
-spec(prepare() -> ok).
%% 加入集群的时候提前准备，将插件卸载，同时停止对应的应用
prepare() ->
	emqttd_plugins:unload(),
	lists:foreach(fun application:stop/1, [emqttd, mochiweb, esockd, gproc]).

%% @doc Is node in cluster?
-spec(is_clustered(node()) -> boolean()).
is_clustered(Node) ->
    lists:member(Node, emqttd_mnesia:running_nodes()).

%% @doc Reboot after join or leave cluster.
-spec(reboot() -> ok).
%% 离开或者加入集群重新启动对应的应用
reboot() ->
	lists:foreach(fun application:start/1, [gproc, esockd, mochiweb, emqttd]).

%% @doc Leave from Cluster.
-spec(leave() -> ok | {error, any()}).
%% 当前节点离开集群
leave() ->
    case emqttd_mnesia:running_nodes() -- [node()] of
        [_|_] ->
            prepare(), ok = emqttd_mnesia:leave_cluster(), reboot();
        [] ->
            {error, node_not_in_cluster}
    end.

%% @doc Remove a node from cluster.
-spec(remove(node()) -> ok | {error, any()}).
%% 将Node节点从当前节点集群中删除掉
remove(Node) when Node =:= node() ->
	{error, {cannot_remove_self, Node}};

remove(Node) ->
	case rpc:call(Node, ?MODULE, prepare, []) of
		ok ->
			case emqttd_mnesia:remove_from_cluster(Node) of
				ok    -> rpc:call(Node, ?MODULE, reboot, []);
				Error -> Error
			end;
		Error ->
			{error, Error}
	end.

%% @doc Cluster status
status() ->
    emqttd_mnesia:cluster_status().

