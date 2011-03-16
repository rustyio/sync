%% Copyright (c) 2011 Rusty Klophaus
%% Released under the MIT License.

-module (sync).
-behaviour(application).
-behaviour(supervisor).

%% API.
-export ([go/0, stop/0, start/0]).

%% Application Callbacks.
-export([start/2, stop/1]).

%% Supervisor Callbacks.
-export([init/1]).

-include_lib("kernel/include/file.hrl").
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ----------------------------------------------------------------------
%% API 
%% ----------------------------------------------------------------------

go() ->
    application:start(sync).

stop() ->
    application:stop(sync).

start() ->
    application:start(sync).

%% ----------------------------------------------------------------------
%% Application Callbacks
%% ----------------------------------------------------------------------


start(_StartType, _StartArgs) ->
    io:format("Starting Sync (Automatic Code Reloader)~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.


%% ----------------------------------------------------------------------
%% Supervisor Callbacks
%% ----------------------------------------------------------------------

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(sync_worker, worker)
    ]} }.
