REBAR = $(shell pwd)/rebar3

.PHONY: deps rel stagedevrel package version all


all: version_header version compile

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > dragon.version

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat dragon.version)\">>)." > apps/dragon/include/dragon.hrl

compile: version_header
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean
	make -C rel/pkg clean
	rm -r apps/*/ebin

distclean: clean devclean relclean
	$(REBAR) delete-deps

# rel: all
# 	cd rel; $(REBAR) generate

upgrade: $(REBAR)
	$(REBAR) upgrade
	make tree

update: $(REBAR)
	$(REBAR) update

tree: $(REBAR)
	$(REBAR) tree | grep -v '=' | sed 's/ (.*//' > tree

relclean:
	rm -rf rel/dragon

rel: update
	$(REBAR) as prod compile
	$(REBAR) as prod release

package: rel
	make -C rel/pkg package


tags:
	find . -name "*.[h