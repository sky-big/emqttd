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

%% MQTT Protocol Header

%%--------------------------------------------------------------------
%% MQTT Protocol Version and Levels
%%--------------------------------------------------------------------
-define(MQTT_PROTO_V31,  3).
-define(MQTT_PROTO_V311, 4).

-define(PROTOCOL_NAMES, [
    {?MQTT_PROTO_V31,  <<"MQIsdp">>},
    {?MQTT_PROTO_V311, <<"MQTT">>}]).

-type mqtt_vsn() :: ?MQTT_PROTO_V31 | ?MQTT_PROTO_V311.

%%--------------------------------------------------------------------
%% MQTT QoS
%%--------------------------------------------------------------------
-define(QOS_0, 0). %% At most once
-define(QOS_1, 1). %% At least once
-define(QOS_2, 2). %% Exactly once

-define(QOS0, 0). %% At most once
-define(QOS1, 1). %% At least once
-define(QOS2, 2). %% Exactly once

-define(IS_QOS(I), (I >= ?QOS0 andalso I =< ?QOS2)).

-type mqtt_qos() :: ?QOS0 | ?QOS1 | ?QOS2.

-type mqtt_qos_name() :: qos0 | at_most_once  |
                         qos1 | at_least_once |
                         qos2 | exactly_once.

-define(QOS_I(Name),
    begin
        (case Name of
            ?QOS_0        -> ?QOS_0;
            qos0          -> ?QOS_0;
            at_most_once  -> ?QOS_0;
            ?QOS_1        -> ?QOS_1;
            qos1          -> ?QOS_1;
            at_least_once -> ?QOS_1;
            ?QOS_2        -> ?QOS_2;
            qos2          -> ?QOS_2;
            exactly_once  -> ?QOS_2
        end)
    end).

%%--------------------------------------------------------------------
%% Max ClientId Length. Why 1024? NiDongDe...
%%--------------------------------------------------------------------
-define(MAX_CLIENTID_LEN, 1024).

%%--------------------------------------------------------------------
%% MQTT Control Packet Types
%%--------------------------------------------------------------------
-define(RESERVED,     0).   %% Reserved										%% 保留
-define(CONNECT,      1).   %% Client request to connect to Server			%% 客户端请求连接到服务器
-define(CONNACK,      2).   %% Server to Client: Connect acknowledgment		%% 连接确认
-define(PUBLISH,      3).   %% Publish message								%% 发布消息
-define(PUBACK,       4).   %% Publish acknowledgment						%% 发布确认
-define(PUBREC,       5).   %% Publish received (assured delivery part 1)	%% 发布接收(有保证的交付第1部分)
-define(PUBREL,       6).   %% Publish release (assured delivery part 2)	%% 发布释放(有保证的交付第2部分)
-define(PUBCOMP,      7).   %% Publish complete (assured delivery part 3)	%% 发布完成(有保证的交付第3部分)
-define(SUBSCRIBE,    8).   %% Client subscribe request						%% 客户端订阅请求
-define(SUBACK,       9).   %% Server Subscribe acknowledgment				%% 订阅确认
-define(UNSUBSCRIBE, 10).   %% Unsubscribe request							%% 客户端取消订阅请求
-define(UNSUBACK,    11).   %% Unsubscribe acknowledgment					%% 取消订阅确认
-define(PINGREQ,     12).   %% PING request									%% PING请求
-define(PINGRESP,    13).   %% PING response								%% PING回复
-define(DISCONNECT,  14).   %% Client is disconnecting						%% 客户端断开连接

-define(TYPE_NAMES, [
					 'CONNECT',
					 'CONNACK',
					 'PUBLISH',
					 'PUBACK',
					 'PUBREC',
					 'PUBREL',
					 'PUBCOMP',
					 'SUBSCRIBE',
					 'SUBACK',
					 'UNSUBSCRIBE',
					 'UNSUBACK',
					 'PINGREQ',
					 'PINGRESP',
					 'DISCONNECT'
					]).

-type mqtt_packet_type() :: ?RESERVED..?DISCONNECT.

%%--------------------------------------------------------------------
%% MQTT Connect Return Codes
%%--------------------------------------------------------------------
-define(CONNACK_ACCEPT,      0).    %% Connection accepted
-define(CONNACK_PROTO_VER,   1).    %% Unacceptable protocol version
-define(CONNACK_INVALID_ID,  2).    %% Client Identifier is correct UTF-8 but not allowed by the Server
-define(CONNACK_SERVER,      3).    %% Server unavailable
-define(CONNACK_CREDENTIALS, 4).    %% Username or password is malformed
-define(CONNACK_AUTH,        5).    %% Client is not authorized to connect

-type mqtt_connack() :: ?CONNACK_ACCEPT..?CONNACK_AUTH.

%%--------------------------------------------------------------------
%% MQTT Parser and Serializer
%%--------------------------------------------------------------------
-define(MAX_LEN, 16#fffffff).
-define(HIGHBIT, 2#10000000).
-define(LOWBITS, 2#01111111).

%%--------------------------------------------------------------------
%% MQTT Packet Fixed Header
%%--------------------------------------------------------------------
%% 消息头数据结构(占用协议中的第一个字节)
-record(mqtt_packet_header, {
							 type   = ?RESERVED  :: mqtt_packet_type(),		%% 消息类型(占用4位二进制)
							 dup    = false      :: boolean(),				%% 其是用来在保证消息传输可靠的，如果设置为1，则在下面的变长头部里多加MessageId,并需要回复确认，
							 												%% 保证消息传输完成，但不能用于检测消息重复发送(占用1位二进制)
							 qos    = ?QOS_0     :: mqtt_qos(),				%% 主要用于PUBLISH（发布态）消息的，保证消息传递的次数(占用2位二进制)
							 retain = false      :: boolean()				%% 主要用于PUBLISH(发布态)的消息，表示服务器要保留这次推送的信息，如果有新的订阅者出现，
																			%% 就把这消息推送给它。如果不设那么推送至当前订阅的就释放了
							}).

%%--------------------------------------------------------------------
%% MQTT Packets
%%--------------------------------------------------------------------
-type mqtt_client_id()  :: binary().
-type mqtt_packet_id() :: 1..16#ffff | undefined.

%% 客户端连接服务器的连接信息数据包
-record(mqtt_packet_connect,  {
							   client_id   = <<>>              :: mqtt_client_id(),		%% 这是第1个UTF编码字符串。客户端ID(Client ID)的长度为1至23个字符，
																						%% 服务器根据客户端ID可以指定到唯一的客户端。对于连接到某个服务器的所有客户端，
																						%% 它们的客户端ID必须都是唯一的，客户端ID还是处理QoS级别1和2消息的关键。
																						%% 如果发送的CONNECT 消息中客户端ID的长度大于23个字符，
																						%% 则服务器会回复一个返回码值为2（标识符被拒绝）的CONNACK 消息

							   proto_ver   = ?MQTT_PROTO_V311  :: mqtt_vsn(),			%% 协议版本号

							   proto_name  = <<"MQTT">>        :: binary(),				%% 协议名字
							   
							   will_retain = false             :: boolean(),			%% Will Retain标志指明服务器是否需要保持客户端异常离线时发送给客户端的Will消息

							   will_qos    = ?QOS_0            :: mqtt_qos(),			%% Will QoS标志用来设置当客户端异常离线时，服务器发送的Will消息的交付质量级别。
																						%% Will消息的内容在客户端发送的CONNECT 消息里的有效载荷里填写

							   will_flag   = false             :: boolean(),			%% Will消息是指当服务器与客户端通信过程中出现故障或客户端在保活时间内没有与服务器
																						%% 保持正常交流时，服务器特意发给客户端的消息。当客户端通过发送
																						%% DISCONNECT 消息正常断开时，Will消息不会发送

							   clean_sess  = false             :: boolean(),			%% 如果没有被置位（即值为0），则当客户端断线时，服务器必须保存该客户端的订阅信息，
																						%% 包括断线期间发布的该客户端订阅的主题中交付质量级别为QoS1和QoS2的消息，这样
																						%% 当客户端重连时，这部分消息能确保被送达到客户端。同时，服务器还必须保持客户端在断
																						%% 线的那个时刻正在传输中的消息的状态，直到客户端重新连接
							   keep_alive  = 60                :: non_neg_integer(),	%% 保活计时器定义了服务器收到客户端消息的最大时间间隔，
																						%% 它以秒为单位。它使得服务器不需要等待漫长的TCP/IP超时就可以检测与客户端的网络连接是否断开。
																						%% 客户端有义务在每个保活时间间隔内至少发送一条消息给服务器。如果这期间没有业务相关的消息要发送，
																						%% 客户端则发送一个PINGREQ 消息给服务器，相应地服务器返回一个PINGRESQ 消息给客户端

							   will_topic  = undefined         :: undefined | binary(),	%% 如果Will标志被置位，则Will主题将是有效载荷中的下一个字符串。

							   will_msg    = undefined         :: undefined | binary(),	%% 如果Will标志被置位，则Will消息将是有效载荷中的下一个字符串。Will
																						%% 消息定义了客户端异常离线时服务器发送给Will主题的消息内容。当然，
																						%% 消息内容可以为空（消息长度为0，但该字符串仍包含2个字节以记录其长度为0）
							   
							   username    = undefined         :: undefined | binary(),	%% 客户端在连接服务器时可以指定用户名和密码，通过将用户名标志和密码标志
																						%% （可选）置位表明在CONNECT 消息的有效载荷里包含有用户名和密码。
							   password    = undefined         :: undefined | binary()
							  }).

-record(mqtt_packet_connack, {
							  ack_flags = ?RESERVED   :: 0 | 1,
							  return_code             :: mqtt_connack()
							 }).

%% 发布数据包的数据结构
-record(mqtt_packet_publish, {
							  topic_name  :: binary(),
							  packet_id   :: mqtt_packet_id()
							 }).

-record(mqtt_packet_puback, {
							 packet_id   :: mqtt_packet_id()
							}).

-record(mqtt_packet_subscribe, {
								packet_id   :: mqtt_packet_id(),
								topic_table :: list({binary(), mqtt_qos()})
							   }).

-record(mqtt_packet_unsubscribe, {
								  packet_id   :: mqtt_packet_id(),
								  topics      :: list(binary())
								 }).

-record(mqtt_packet_suback, {
							 packet_id   :: mqtt_packet_id(),
							 qos_table   :: list(mqtt_qos() | 128)
							}).

-record(mqtt_packet_unsuback, {
							   packet_id   :: mqtt_packet_id()
							  }).

%%--------------------------------------------------------------------
%% MQTT Control Packet
%%--------------------------------------------------------------------
%% 消息包数据结构
-record(mqtt_packet, {
    header    :: #mqtt_packet_header{},									%% 消息包的头数据结构
    variable  :: #mqtt_packet_connect{} | #mqtt_packet_connack{}		%% 消息包的实际内容
                | #mqtt_packet_publish{} | #mqtt_packet_puback{}
                | #mqtt_packet_subscribe{} | #mqtt_packet_suback{}
                | #mqtt_packet_unsubscribe{} | #mqtt_packet_unsuback{}
                | mqtt_packet_id() | undefined,
    payload   :: binary() | undefined									%% 实际的消息体
					 }).

-type mqtt_packet() :: #mqtt_packet{}.

%%--------------------------------------------------------------------
%% MQTT Packet Match
%%--------------------------------------------------------------------
-define(CONNECT_PACKET(Var),
    #mqtt_packet{header = #mqtt_packet_header{type = ?CONNECT}, variable = Var}).

-define(CONNACK_PACKET(ReturnCode),
    #mqtt_packet{header   = #mqtt_packet_header{type = ?CONNACK},
                 variable = #mqtt_packet_connack{return_code = ReturnCode}}).

-define(CONNACK_PACKET(ReturnCode, SessPresent),
    #mqtt_packet{header   = #mqtt_packet_header{type = ?CONNACK},
                 variable = #mqtt_packet_connack{ack_flags = SessPresent,
                                                 return_code = ReturnCode}}).

-define(PUBLISH_PACKET(Qos, PacketId),
    #mqtt_packet{header   = #mqtt_packet_header{type = ?PUBLISH,
                                                qos = Qos},
                 variable = #mqtt_packet_publish{packet_id = PacketId}}).

-define(PUBLISH_PACKET(Qos, Topic, PacketId, Payload),
    #mqtt_packet{header   = #mqtt_packet_header{type = ?PUBLISH,
                                                qos = Qos},
                 variable = #mqtt_packet_publish{topic_name = Topic,
                                                 packet_id  = PacketId},
                 payload  = Payload}).

-define(PUBACK_PACKET(Type, PacketId),
    #mqtt_packet{header   = #mqtt_packet_header{type = Type},
                 variable = #mqtt_packet_puback{packet_id = PacketId}}).

-define(PUBREL_PACKET(PacketId),
    #mqtt_packet{header   = #mqtt_packet_header{type = ?PUBREL, qos = ?QOS_1},
                 variable = #mqtt_packet_puback{packet_id = PacketId}}).

-define(SUBSCRIBE_PACKET(PacketId, TopicTable), 
    #mqtt_packet{header = #mqtt_packet_header{type = ?SUBSCRIBE, qos = ?QOS_1},
                 variable = #mqtt_packet_subscribe{packet_id   = PacketId,
                                                   topic_table = TopicTable}}).
-define(SUBACK_PACKET(PacketId, QosTable),
    #mqtt_packet{header = #mqtt_packet_header{type = ?SUBACK},
                 variable = #mqtt_packet_suback{packet_id = PacketId,
                                                qos_table = QosTable}}).
-define(UNSUBSCRIBE_PACKET(PacketId, Topics), 
    #mqtt_packet{header = #mqtt_packet_header{type = ?UNSUBSCRIBE, qos = ?QOS_1},
                 variable = #mqtt_packet_unsubscribe{packet_id = PacketId,
                                                     topics    = Topics}}).
-define(UNSUBACK_PACKET(PacketId),
    #mqtt_packet{header = #mqtt_packet_header{type = ?UNSUBACK},
                 variable = #mqtt_packet_unsuback{packet_id = PacketId}}).

-define(PACKET(Type),
    #mqtt_packet{header = #mqtt_packet_header{type = Type}}).

