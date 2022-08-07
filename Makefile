all: compile

compile: rebar3
	./rebar3 compile

dialyzer: rebar3
	./rebar3 dialyzer

travis: dialyzer

publish: rebar3
	./rebar3 hex publish

rebar3:
	echo "Fetching and compiling updated rebar3 (this will not replace your system-wide rebar3, if you have one)"
	@(cd /tmp && \
	git clone https://github.com/erlang/rebar3 && \
	cd rebar3 && \
	./bootstrap)
	@(echo "Installing rebar3 into sync directory")
	@(mv /tmp/rebar3/rebar3 .)
	echo "Cleaning up..."
	@(rm -fr /tmp/rebar3)
