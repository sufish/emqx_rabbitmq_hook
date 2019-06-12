-module(emqx_rabbitmq_hook_cli).

-behaviour(ecpool_worker).

-include("emqx_rabbitmq_hook.hrl").
-include("../../amqp_client/include/amqp_client.hrl").
-export([connect/1]).
-export([ensure_exchange/1, ensure_exchange/0, publish/3, publish/2]).

connect(Opts) ->
  ConnOpts = #amqp_params_network{
    host = proplists:get_value(host, Opts),
    port = proplists:get_value(port, Opts),
    username = proplists:get_value(username, Opts),
    password = proplists:get_value(password, Opts)
  },
  {ok, C} = amqp_connection:start(ConnOpts),
  {ok, C}.

ensure_exchange() ->
  {ok, ExchangeName} = application:get_env(?APP, exchange),
  ensure_exchange(ExchangeName).

ensure_exchange(ExchangeName) ->
  ecpool:with_client(?APP, fun(C) -> ensure_exchange(ExchangeName, C) end).

ensure_exchange(ExchangeName, Conn) ->
  {ok, Channel} = amqp_connection:open_channel(Conn),
  Declare = #'exchange.declare'{exchange = ExchangeName, durable = true},
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
  amqp_channel:close(Channel).

publish(Payload, RoutingKey) ->
  {ok, ExchangeName} = application:get_env(?APP, exchange),
  publish(ExchangeName, Payload, RoutingKey).

publish(ExchangeName, Payload, RoutingKey) ->
  ecpool:with_client(?APP, fun(C) -> publish(ExchangeName, Payload, RoutingKey, C) end).

publish(ExchangeName, Payload, RoutingKey, Conn) ->
  {ok, Channel} = amqp_connection:open_channel(Conn),
  Publish = #'basic.publish'{exchange = ExchangeName, routing_key = RoutingKey},
  Props = #'P_basic'{delivery_mode = 2},
  Msg = #amqp_msg{props = Props, payload = Payload},
  amqp_channel:cast(Channel, Publish, Msg),
  amqp_channel:close(Channel).


