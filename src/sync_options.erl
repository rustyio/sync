%% vim: ts=4 sw=4 et

-module(sync_options).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_onsync/0,
    set_onsync/1,
    get_options/1,
    set_options/2
]).

%% gen_server callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2,
    terminate/2, 
    code_change/3
]).

-define(SERVER, ?MODULE). 
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(state, { 
    onsync_fun,
    options_table
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_onsync() ->
    gen_server:call(?SERVER, get_onsync).

set_onsync(Fun) ->
    gen_server:call(?SERVER, {set_onsync, Fun}).

%% @private If options are not found for this directory, keep checking the
%% parent directories for options
get_options([]) ->
    undefined;
get_options(SrcDir) ->
    case gen_server:call(?SERVER, {get_options, SrcDir}) of
        {ok, Options} -> {ok, Options};
        undefined -> get_options(filename:dirname(SrcDir))
    end.

set_options(SrcDir, Options) ->
    gen_server:call(?SERVER, {set_options, SrcDir, Options}).

init([]) ->
    %% Create the state and return...
    State = #state {
        options_table = ets:new(options_table, [set, named_table, private])
    },
    {ok, State}.

handle_call(get_onsync, _From, State) ->
    OnSync = State#state.onsync_fun,

    {reply, OnSync, State};

handle_call({set_onsync, Fun}, _From, State) ->
    State2 = State#state{
            onsync_fun = Fun},

    {reply, ok, State2};

handle_call({get_options, SrcDir}, _From, State) ->
    %% Look up the compile options for a SrcDir...
    case ets:lookup(State#state.options_table, SrcDir) of
        [{SrcDir, Options}] -> 
            {reply, {ok, Options}, State};
        _ ->
            {reply, undefined, State}
    end;

handle_call({set_options, SrcDir, Options}, _From, State) ->
    %% Set the compile options for a SrcDir...
    Table = State#state.options_table,
    case ets:lookup(Table, SrcDir) of
        [{SrcDir, OldOptions}] -> 
            NewOptions = lists:usort(Options ++ OldOptions),
            ets:insert(Table, {SrcDir, NewOptions});
        _ -> 
            ets:insert(Table, {SrcDir, Options})
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
