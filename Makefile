PROJECT = emqx_rabbitmq_hook
PROJECT_DESCRIPTION = EMQ X Rabbitmq Hook

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)
DEPS = lager amqp_client ecpool bson
BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1
dep_lager = git-emqx https://github.com/erlang-lager/lager master
dep_amqp_client = git-emqx https://github.com/rabbitmq/rabbitmq-erlang-client master
dep_ecpool = git-emqx https://github.com/emqx/ecpool v0.3.0
dep_bson = git-emqx https://github.com/comtihon/bson-erlang master
ERLC_OPTS += +debug_info

NO_AUTOPATCH = cuttlefish

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_rabbitmq_hook.conf -i priv/emqx_rabbitmq_hook.schema -d data
