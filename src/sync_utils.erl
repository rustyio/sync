-module(sync_utils).
-export([
         get_src_dir_from_module/1,
         get_options_from_module/1,
         get_src_dir/1,
         wildcard/2,
         get_env/2,
         file_last_modified_time/1,
         transform_options/2,
         get_system_modules/0
]).

get_src_dir_from_module(Module)->
    case code:is_loaded(Module) of
        {file, _} ->
            try
                %% Get some module info...
                Props = Module:module_info(compile),
                Source = proplists:get_value(source, Props, ""),

                %% Ensure that the file exists...
                filelib:is_regular(Source) orelse
                    throw(not_found),

                %% Get the source dir...
                Dir = filename:dirname(Source),
                get_src_dir(Dir)
            catch _ : _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

get_options_from_module(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            try
                Props = Module:module_info(compile),
                {ok, proplists:get_value(options, Props, [])}
            catch _ : _ ->
                    undefined
            end;
        _ ->
            {ok, []}
    end.


get_src_dir(Dir) when Dir == ""; Dir == "/"; Dir == "." ->
    undefined;
get_src_dir(Dir) ->
    HasCode =
        filelib:wildcard("*.erl", Dir) /= [] orelse
        filelib:wildcard("*.hrl", Dir) /= [],

    case HasCode of
        true ->
            {ok, Dir};
        false ->
            get_src_dir(filename:dirname(Dir))
    end.

%% @private Return all files in a directory matching a regex.
wildcard(Dir, Regex) ->
    filelib:fold_files(Dir, Regex, true, fun(Y, Acc1) -> [Y|Acc1] end, []).

%% @private Get an environment variable.
get_env(Var, Default) ->
    case application:get_env(Var) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

%% @private Return the time a file was modified.
file_last_modified_time(File) ->
    try
        {ok, filelib:last_modified(File)}
    catch _Error : _Reason ->
        deleted
    end.

%% @private Walk through each option. If it's an include or outdir option, then
%% rewrite the path...
transform_options(SrcDir, Options) ->
    F = fun(Option, Acc) ->
        case Option of
            {i, IncludeDir1} ->
                IncludeDir2 = filename:join([SrcDir, "..", IncludeDir1]),
                [{i, IncludeDir2}|Acc];
            {outdir, _Dir} ->
                Acc;
            Other ->
                [Other|Acc]
        end
    end,

    LastPart = hd(lists:reverse(filename:split(proplists:get_value(outdir, Options, "./ebin")))),
    BinDir = filename:join([SrcDir, "..", LastPart]),
    lists:foldl(F, [], Options) ++ [{outdir, BinDir}].



%% @private Return a list of all modules that belong to Erlang rather
%% than whatever application we may be running.
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
        xmerl,
        zlib
    ],
    F = fun(App) ->
        case application:get_key(App, modules) of
            {ok, Modules} -> Modules;
            _Other -> []
        end
    end,
    lists:flatten([F(X) || X <- Apps]).
