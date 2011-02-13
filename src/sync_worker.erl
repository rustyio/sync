%% Copyright (c) 2011 Rusty Klophaus
%% Released under the MIT License.

-module (sync_worker).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-define(SERVER, ?MODULE).
-record(state, { }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    do_interval(),
    State = #state {},
    {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(recompile, State) ->
    Exceptions = get_system_modules(),
    recompile(Exceptions),
    do_interval(),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ----------------------------------------------------------------------
%% Private Functions
%% ----------------------------------------------------------------------

%% @private
%% @doc Schedule a new cast with the 'recompile' message after a
%%      certain number of MS.
%%
%% TODO - Make the interval configurable.
do_interval() ->
    Interval = get_env(compile_interval, 1000),
    timer:apply_after(Interval, gen_server, cast, [?SERVER, recompile]).

%% @private
%% @doc Recompile any modules that have changed, excluding system
%%      modules.
recompile(Exceptions) ->
    %% Get a list of all loaded modules minus system modules...
    AllModules = erlang:loaded(),
    CheckModules = AllModules -- Exceptions,

    %% Possibly compile the module, log the results.
    clear_sync_out_file(),
    [possibly_compile(X) || X <- CheckModules],
    write_sync_out_file(),
    ok.

possibly_compile(Module) ->
    %% Set up some properties...
    CompileInfo = get_cache({Module, compile_info}, fun() -> Module:module_info(compile) end),

    %% Get some simple vars...
    File = proplists:get_value(source, CompileInfo),
    IsFile = filelib:is_file(File),
    CompileTime = module_compiled_time(Module),
    
    %% Get options...
    Options = case get_cache({Module, options}, undefined) of
        {CompileTime, CachedOptions} ->
            CachedOptions;
        _ ->
            Options1 = proplists:get_value(options, CompileInfo, []),
            Options2 = case code:ensure_loaded(eqc) of
                {module, eqc} ->
                    Options1 ++ [{d, 'EQC'}];
                _ ->
                    Options1
            end,
            Options3 = transform_options(Module, File, Options2),
            Options4 = lists:usort(lists:flatten(Options3)),
            put_cache({Module, options}, {CompileTime, Options4}),
            Options4
    end,

    %% If the file exists, get the last modified time...
    case IsFile of
        true ->
            FileLastModifiedTime = file_last_modified_time(File);
        false ->
            FileLastModifiedTime = undefined
    end,
    IsFileModified = (CompileTime < FileLastModifiedTime),

    %% If the file exists and hasn't been modified, get the include last modified times...
    case IsFile andalso not IsFileModified of
        true ->
            IncludePaths = [Path || {i, Path} <- Options],
            IncludeLastModifiedTime = include_last_modified_time(File, FileLastModifiedTime, IncludePaths);
        false ->
            IncludeLastModifiedTime = undefined
    end,
    IsIncludeModified = (CompileTime < IncludeLastModifiedTime),

    %% If the file or one of it's dependencies has been modified, then recompile...
    case IsFileModified orelse IsIncludeModified of
        true ->
            %% Old Errors and Old Warnings...
            OldErrors = get_cache({Module, errors}, []),
            OldWarnings = get_cache({Module, warnings}, []),
    
            case compile:file(File, [binary, return|Options]) of
                {ok, Module, Binary, []} ->
                    put_cache({Module, errors}, []),
                    put_cache({Module, warnings}, []),
                    compile:file(File, Options),
                    code:load_binary(Module, code:which(Module), Binary),
                    put_cache({Module, compile_info}, Module:module_info(compile)),
                    if
                        OldErrors /= [] orelse OldWarnings /= [] ->
                            Msg = io_lib:format("~s:0: Fixed!~n", [File]),
                            log_sync_out_file(Msg),
                            error_logger:info_msg(Msg);
                        IsFileModified ->
                            Msg = io_lib:format("~s:0: Recompiled. (Reason: Source modified.)~n", [File]),
                            log_sync_out_file(Msg),
                            error_logger:info_msg(Msg);
                        IsIncludeModified ->
                            Msg = io_lib:format("~s:0: Recompiled. (Reason: Include modified.)~n", [File]),
                            log_sync_out_file(Msg),
                            error_logger:info_msg(Msg);
                        true ->
                            ok
                    end;                            
                {ok, Module, _Binary, OldWarnings} ->
                    Msg = [
                        format_errors(File, OldWarnings, [])
                    ],
                    log_sync_out_file(Msg),
                    ok;
                {ok, Module, Binary, Warnings} ->
                    put_cache({Module, errors}, []),
                    put_cache({Module, warnings}, Warnings),
                    Msg = [
                        format_errors(File, Warnings, []),
                        io_lib:format("~s:0: Recompiled with ~p warnings~n", [File, length(Warnings)])
                    ],
                    log_sync_out_file(Msg),
                    error_logger:info_msg(Msg),
                    code:load_binary(Module, code:which(Module), Binary),
                    put_cache({Module, compile_info}, Module:module_info(compile)),
                    ok;
                {error, OldErrors, OldWarnings} ->
                    Msg = [
                        format_errors(File, OldWarnings, OldErrors)
                    ],
                    log_sync_out_file(Msg),
                    ok;
                {error, Errors, Warnings} -> 
                    Msg = [
                        format_errors(File, Warnings, Errors)
                    ],
                    log_sync_out_file(Msg),
                    error_logger:info_msg(Msg),
                    put_cache({Module, errors}, Errors),
                    put_cache({Module, warnings}, Warnings)
            end;
        false ->
            ok
    end.

%% Walk through each option. If it's an include or outdir option, then
%% rewrite the path...
transform_options(Module, File, Options) ->
    F = fun(Option, Acc) ->
        case Option of
            {i, Dir} ->
                [{i, filename:join([filename:dirname(File), "..", Dir])}|Acc];
            {outdir, _Dir} ->
                Acc;
            Other ->
                [Other|Acc]
        end
    end,
    lists:foldl(F, [], Options) ++ [{outdir, filename:dirname(code:which(Module))}].


%% Print error messages in a pretty and user readable way.
format_errors(File, Warnings, Errors) ->
    AllErrors1 = lists:sort(lists:flatten([X || {_, X} <- Errors])),
    AllErrors2 = [{Line, "Error", Module, Description} || {Line, Module, Description} <- AllErrors1],
    AllWarnings1 = lists:sort(lists:flatten([X || {_, X} <- Warnings])),
    AllWarnings2 = [{Line, "Warning", Module, Description} || {Line, Module, Description} <- AllWarnings1],
    Everything = lists:sort(AllErrors2 ++ AllWarnings2),
    F = fun({Line, Prefix, Module, ErrorDescription}) ->
        Msg = Module:format_error(ErrorDescription),
        io_lib:format("~s:~p: ~s: ~s~n", [File, Line, Prefix, Msg])
    end,
    [F(X) || X <- Everything].

%% Return the time that a module was compiled.
module_compiled_time(Module) ->
    CompileInfo = Module:module_info(compile),
    {Y, M, D, HH, MM, SS} = proplists:get_value(time, CompileInfo, {0, 0, 0, 0, 0, 0}),
    {{Y, M, D}, {HH, MM, SS}}.

%% Return the time a file was modified.
file_last_modified_time(File) ->
    [LastMod] = calendar:local_time_to_universal_time_dst(filelib:last_modified(File)),
    LastMod.

%% Given a file and a list of include paths, return the most recently
%% modified include file.
include_last_modified_time(File, FileLastModifiedTime, IncludePaths) ->
    %% Get the list of includes either from cache or by parsing the file...
    case get_cache({File, sync_cache}, []) of
        {FileLastModifiedTime, CachedIncludes} -> 
            Includes = CachedIncludes;
        _ ->
            Includes = get_includes(File, IncludePaths),
            put_cache({File, sync_cache}, {FileLastModifiedTime, Includes})
    end,
    
    %% Get the last modified time of each include...
    ModTimes = [file_last_modified_time(X) || X <- Includes],
    %% ModTimes = [],
    case lists:reverse(lists:sort(ModTimes)) of
        [] ->
            {{0, 0, 0}, {0, 0, 0}};
        [H|_] ->
            H
    end.
    
get_includes(File, IncludePaths) ->
    %% Parse the file...
    IncludePaths1 = [filename:dirname(File)|IncludePaths],
    case epp:parse_file(File, IncludePaths1, []) of
        {ok, Forms} ->
            get_includes_inner(File, Forms, []);
        _ ->
            []
    end.
get_includes_inner(File, [Form|Forms], Acc) ->
    case Form of
        {attribute,1,file, {File, _}} ->
            get_includes_inner(File, Forms, Acc);
        {attribute,1,file, {Include, _}} ->
            get_includes_inner(File, Forms, [Include|Acc]);
        {eof, _} ->
            Acc;
        _Other ->
            get_includes_inner(File, Forms, Acc)
    end.


%% @private 
%% @doc Return a list of all modules that belong to Erlang rather than
%%      whatever application we may be running.
get_system_modules() ->
    Apps = [
        appmon, 
        asn1, 
        common_test, 
        compiler, 
        crypto,
        debugger,
        dialyzer,
        docbuilder,
        edoc,
        erl_interface,
        erts,
        et,
        eunit,
        gs,
        hipe,
        inets,
        inets,
        inviso,
        jinterface,
        kernel, 
        mnesia,
        observer,
        orber, 
        os_mon,
        parsetools,
        percept,
        pman,
        reltool,
        runtime_tools,
        sasl, 
        snmp,
        ssl,
        stdlib, 
        syntax_tools,
        test_server,
        toolbar,
        tools,
        tv,
        webtool,
        wx,
        xmerl
    ],
    F = fun(App) ->
        case application:get_key(App, modules) of
            {ok, Modules} -> Modules;
            _Other -> []
        end
    end,
    lists:flatten([F(X) || X <- Apps]).

clear_sync_out_file() ->
    erlang:put(sync_out_file, []).

log_sync_out_file(IOData) ->
    erlang:put(sync_out_file, [erlang:get(sync_out_file), IOData]).

write_sync_out_file() ->
    case erlang:get(sync_out_file) of
        [] ->
            ok;
        Other -> 
            File = get_env(out_file, "/tmp/sync.out"),
            {{Y,M,D}, {HH,MM,SS}} = calendar:local_time(),
            Header = get_env(file_variables, ""),
            Footer = io_lib:format("~n- Updated ~4.10.0b-~2.10.0b-~2.10.0b ~2.10.0b:~2.10.0b:~2.10.0b~n", [Y, M, D, HH, MM, SS]),
            ok = file:write_file(File, [Header, Other, Footer])
    end.

get_env(Var, Default) ->
    case application:get_env(Var) of
        {ok, Value} -> 
            Value;
        _ -> 
            Default
    end.
        
get_cache(Key, Default) when is_function(Default) ->
    case erlang:get({sync, Key}) of
        undefined -> 
            put_cache({sync,Key}, Default());
        Other -> 
            Other
    end;
get_cache(Key, Default) ->
    case erlang:get({sync, Key}) of
        undefined -> 
            Default;
        Other -> 
            Other
    end.

put_cache(Key, Value) ->
    erlang:put({sync, Key}, Value),
    Value.
    
