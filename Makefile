
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

SRC_TMP=.tmp

REBAR= `which rebar || ./rebar`

all:
	@$(REBAR) compile skip_deps=true

deps: _deps
_deps:
	@$(REBAR) get-deps
	@$(REBAR) compile

docs:
	@$(REBAR) doc skip_deps=true

eunit:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

xref: _xref
_xref: 
	@$(REBAR) xref skip_deps=true

test: ct
ct:
	- @mv src $(SRC_TMP)
	- @mkdir src
	- @find $(SRC_TMP) -name "*erl" -exec ln -s "../{}" src/ \;	
	- @$(REBAR) skip_deps=true ct
	- @rm -Rf src	
	- @mv $(SRC_TMP) src

clean:
	@$(REBAR) clean skip_deps=true

clean_all:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build_plt

dialyzer:
	@$(REBAR) analyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

devrel: dip1 dip2

dip1 dip2 dip3:
	mkdir -p dev
	(cd rel && rebar generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config force=1)
