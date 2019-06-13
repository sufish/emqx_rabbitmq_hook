-module(emqx_rabbitmq_hook).

-include_lib("emqx/include/emqx.hrl").

-export([load/1
  , unload/0
]).

%% Hooks functions
-export([on_client_connected/4
  , on_client_disconnected/3
  , on_message_publish/2
]).

-import(emqx_rabbitmq_hook_cli, [ensure_exchange/1, publish/3]).
-import(bson_binary, [put_document/1]).

-include("emqx_rabbitmq_hook.hrl").


%% Called when the plugin application start
load(_Env) ->
  {ok, ExchangeName} = application:get_env(?APP, exchange),
  emqx_rabbitmq_hook_cli:ensure_exchange(ExchangeName),
  hookup('client.connected', client_connected, fun ?MODULE:on_client_connected/4, [ExchangeName]),
  hookup('client.disconnected', client_disconnected, fun ?MODULE:on_client_disconnected/3, [ExchangeName]),
  hookup('message.publish', message_publish, fun ?MODULE:on_message_publish/2, [ExchangeName]).

on_client_connected(#{client_id := ClientId, username := Username}, ConnAck, ConnInfo, ExchangeName) ->
  {IpAddr, _Port} = maps:get(peername, ConnInfo),
  Doc = {
    client_id, ClientId,
    username, Username,
    keepalive, maps:get(keepalive, ConnInfo),
    ipaddress, iolist_to_binary(ntoa(IpAddr)),
    proto_ver, maps:get(proto_ver, ConnInfo),
    connected_at, emqx_time:now_ms(maps:get(connected_at, ConnInfo)),
    conn_ack, ConnAck
  },
  emqx_rabbitmq_hook_cli:publish(ExchangeName, bson_binary:put_document(Doc), <<"client.connected">>),
  ok.


on_client_disconnected(#{}, auth_failure, _ExchangeName) ->
  ok;

on_client_disconnected(#{client_id := ClientId, username := Username}, ReasonCode, ExchangeName) ->
  Reason = if
             is_atom(ReasonCode) ->
               ReasonCode;
             true ->
               unknown
           end,
  Doc = {
    client_id, ClientId,
    username, Username,
    disconnected_at, emqx_time:now_ms(),
    reason, Reason
  },
  emqx_rabbitmq_hook_cli:publish(ExchangeName, bson_binary:put_document(Doc), <<"client.disconnected">>),
  ok.


on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};

on_message_publish(Message = #message{topic = Topic, flags = #{retain := Retain}}, ExchangeName) ->
  Username = case maps:find(username, Message#message.headers) of
               {ok, Value} -> Value;
               _ -> undefined
             end,
  Doc = {
    client_id, Message#message.from,
    username, Username,
    topic, Topic,
    qos, Message#message.qos,
    retained, Retain,
    payload, {bin, bin, Message#message.payload},
    published_at, emqx_time:now_ms(Message#message.timestamp)
  },
  emqx_rabbitmq_hook_cli:publish(ExchangeName, bson_binary:put_document(Doc), <<"message.publish">>),
  {ok, Message}.

%% Called when the plugin application stop
unload() ->
  emqx:unhook('client.connected', fun ?MODULE:on_client_connected/4),
  emqx:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
  emqx:unhook('message.publish', fun ?MODULE:on_message_publish/2).


ntoa({0, 0, 0, 0, 0, 16#ffff, AB, CD}) ->
  inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256});
ntoa(IP) ->
  inet_parse:ntoa(IP).

hookup(Event, ConfigName, Func, InitArgs) ->
  case application:get_env(?APP, ConfigName) of
    {ok, true} -> emqx:hook(Event, Func, InitArgs);
    _ -> ok
  end.

