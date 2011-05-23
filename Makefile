include app.mk

# Special characters
comma := ,
empty :=
space := $(empty) $(empty)

# Compiler
ERL  := erl
ERLC := erlc

# Default apps
SED := sed
RM  := rm
RUN_TEST := run_test

# Application Layout
APPSRC    := $(wildcard src/*.app.src)
APP       := $(APPSRC:src/%.src=%)
APPNAME   := $(APPSRC:src/%.app.src=%)
HRLS      := $(wildcard include/*.hrl) $(wildcard src/*.hrl)
ERLS      := $(wildcard src/*.erl)
BEAMS     := $(ERLS:src/%.erl=ebin/%.beam)
MODS      := $(subst $(space),$(comma),$(ERLS:src/%.erl=%))
OVERVIEW  := $(wildcard doc/overview.edoc)
CONFSRC   := $(wildcard test/conf/*.conf.src)
CONF      := $(CONFSRC:.src=)
SPECSRC   := $(wildcard test/conf/*.spec.src)
SPEC      := $(SPECSRC:.src=)
TESTERLS  := $(wildcard test/*_SUITE.erl)
TESTBEAMS := $(ERLS:src/%.erl=test/%.beam)
STUBERLS  := $(wildcard test/stubs/*.erl)
STUBBEAMS := $(STUBERLS:.erl=.beam)

# Compiler options
EDOC_INCL   := $(subst $(space),$(comma),$(INCLUDE:%="%"))
EDOC        += {title,"$(APPNAME)"} {includes,[$(EDOC_INCL)]}
EDOC_OPTS   := $(subst $(space),$(comma),$(strip $(EDOC)))
INCLUDE     := $(INCLUDE:%=-I %)
CODE_PATH   := $(CODE_PATH:%=-pa %)
TEST_EFLAGS += +debug_info

# Targets
.PHONY: all clean dialyze doc test
.SUFFIXES: .erl .hrl .beam .app.src .app .rel .conf.src .conf .spec.src .spec

all: $(BEAMS) ebin/$(APP)

doc: doc/index.html

test: $(TESTBEAMS) $(TESTERLS) $(STUBBEAMS) test/$(APP) $(CONF) $(SPEC)
	$(RUN_TEST) -dir test/ -config ./test/conf/*.conf -spec ./test/conf/*.spec -cover ./test/conf/cover.conf

dialyze:
	dialyzer --verbose --src -c src $(CODE_PATH) $(INCLUDE)

clean:
	@$(RM) -f ebin/*.beam
	@$(RM) -f ebin/*.app
	@$(RM) -f doc/*.html
	@$(RM) -f doc/edoc-info
	@$(RM) -f doc/erlang.png
	@$(RM) -f doc/stylesheet.css
	@$(RM) -f test/*.beam
	@$(RM) -f test/*.app
	@$(RM) -f test/stubs/*.beam
	@$(RM) -f test/variables-ct*
	@$(RM) -rf test/log/*
	@$(RM) -f test/conf/*.conf
	@$(RM) -f test/conf/*.spec

# Rules
ebin/%.beam: src/%.erl $(HRLS) app.mk
	$(ERLC) $(EFLAGS) $(INCLUDE) $(CODE_PATH) -o ebin $<

test/%.beam: src/%.erl $(HRLS) app.mk
	$(ERLC) $(EFLAGS) $(INCLUDE) $(CODE_PATH) $(TEST_EFLAGS) -o test $<

test/stubs/%.beam: test/stubs/%.erl
	$(ERLC) $(EFLAGS) $(INCLUDE) $(CODE_PATH) $(STUB_EFLAGS) -o test/stubs $<

%/$(APP): $(APPSRC) $(ERLS) app.mk
	$(SED) -e "s;%APPNAME%;$(APPNAME);" \
    -e "s;%MODULES%;$(MODS);" \
    -e "s;%VSN%;$(VSN);" \
    $< > $@

test/conf/%.conf: test/conf/%.conf.src
	$(SED) -e "s;%MODULES%;$(MODS);g" \
    -e "s;%TEST_DIR%;$(CURDIR)/test;g" \
    $^ > $@

test/conf/%.spec: test/conf/%.spec.src
	$(SED) "s;%TEST_DIR%;$(CURDIR)/test;g" $^ > $@

doc/index.html: $(ERLS) $(HRLS) $(OVERVIEW) app.mk 
	$(ERL) -noshell -run edoc_run application '$(APPNAME)' '"."' '[$(EDOC_OPTS)]'
