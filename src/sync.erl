%% vim: ts=4 sw=4 et
%% Copyright (c) 2011 Rusty Klophaus
%% Released under the MIT License.

-module (sync).
-behaviour(application).
-behaviour(supervisor).

%% API.
-export ([
    start/0,
    patch/0,
    go/0,
    pause/0,
    info/0,
    stop/0,
    growl/1,growl/0,
    log/1,log/0,
    onsync/1,onsync/0
]).

%% Application Callbacks.
-export([start/2, stop/1]).

%% Supervisor Callbacks.
-export([init/1]).

-include_lib("kernel/include/file.hrl").
-define(SERVER, ?MODULE).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-define(CHILD(I,Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(VALID_GROWL_OR_LOG(X), is_boolean(X); is_list(X); X==all; X==none; X==skip_success).

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
            sync_scanner:unpause(),
            sync_scanner:rescan()
    end.

patch() ->
    go(),
    sync_scanner:enable_patching(),
    ok.


pause() ->
    sync_scanner:pause().


info() ->
    sync_scanner:info().

stop() ->
    application:stop(sync).

growl(Val) when ?VALID_GROWL_OR_LOG(Val) ->
    sync_scanner:set_growl(Val).

growl() ->
    sync_scanner:get_growl().

log(Val) when ?VALID_GROWL_OR_LOG(Val) ->
    sync_scanner:set_log(Val).

log() ->
    sync_scanner:get_log().

onsync(Fun) ->
    sync_options:set_onsync(Fun).

onsync() ->
    sync_options:get_onsync().

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
    {ok, { {rest_for_one, 5, 10}, [
        ?CHILD(sync_options, worker),
        ?CHILD(sync_scanner, worker)
    ]} }.
