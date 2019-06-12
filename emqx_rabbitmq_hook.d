src/emqx_rabbitmq_hook.erl:: include/emqx_rabbitmq_hook.hrl src/emqx_rabbitmq_hook_cli.erl; @touch $@
src/emqx_rabbitmq_hook_cli.erl:: include/../../amqp_client/include/amqp_client.hrl include/emqx_rabbitmq_hook.hrl; @touch $@
src/emqx_rabbitmq_hook_sup.erl:: include/emqx_rabbitmq_hook.hrl; @touch $@

COMPILE_FIRST += emqx_rabbitmq_hook_cli
