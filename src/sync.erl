%% Copyright (c) 2011 Rusty Klophaus
%% Released under the MIT License.

-module (sync).
-behaviour(application).
-behaviour(supervisor).

%% API.
-export ([
    start/0,
    go/0, 
    info/0,
    stop/0,
	growl/1,growl/0
]).

%% Application Callbacks.
-export([start/2, stop/1]).

%% Supervisor Callbacks.
-export([init/1]).

-include_lib("kernel/include/file.hrl").
-define(SERVER, ?MODULE).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-define(CHILD(I,Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ----------------------------------------------------------------------
%% API 
%% ----------------------------------------------------------------------

start() ->
    application:start(sync).

go() ->
    case application:start(sync) of
        ok -> 
            ok;
        {error, {already_started, sync}} ->
            sync_scanner:rescan()
    end.

info() ->
    sync_scanner:info().

stop() ->
    application:stop(sync).

growl(Val) when is_boolean(Val) ->
	sync_scanner:set_growl(Val).

growl() ->
	sync_scanner:get_growl().

%% ----------------------------------------------------------------------
%% Application Callbacks
%% ----------------------------------------------------------------------


start(_StartType, _StartArgs) ->
    io:format("Starting Sync (Automatic Code Compiler / Reloader)~n"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    ok.

%% ----------------------------------------------------------------------
%% Supervisor Callbacks
%% ----------------------------------------------------------------------

init([]) ->
    %% Return.
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(sync_scanner, worker),
        ?CHILD(sync_options, worker)
    ]} }.
