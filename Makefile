REBAR:=rebar

.PHONY: all build test clean doc 

all: test

build:
	$(REBAR) get-deps compile

buildplt:
	if [ ! -f .plt ]; then \
        	dialyzer --build_plt --output_plt .plt --apps kernel stdlib ; \
    fi

pltclean:
	@rm .plt

dialyze: buildplt
	@ERL_LIBS=deps dialyzer --fullpath -Wno_undefined_callbacks \
        --plts .plt \
        -r ebin --src src \
        | grep -v -f ./dialyzer.ignore-warnings

test: build dialyze
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit ct

clean:
	$(REBAR) clean
	-rm -rvf deps ebin doc .eunit

doc:
	$(REBAR) doc

