# Sync: It helps you call the `make` module.

Sync is a utility to help you hot-load changed code in an
Erlang system.  When you run `sync:go()`, sync will recursively find
all Emakefiles under the current directory, plus all relative code
paths, and call `make:all([load])` for each one.

For a more full-featured Erlang build system, check out
[rebar](http://hg.basho.com/rebar).