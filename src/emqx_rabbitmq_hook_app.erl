-module(emqx_rabbitmq_hook_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([start/2
  , stop/1
]).

start(_StartType, _StartArgs) ->
  {ok, Sup} = emqx_plugin_template_sup:start_link(),
  emqx_rabbitmq_hook:load(application:get_all_env()),
  {ok, Sup}.

stop(_State) ->
  emqx_rabbitmq_hook:unload().

