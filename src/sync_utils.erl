%% vim: ts=4 sw=4 et
-module(sync_utils).
-export([
         get_src_dir_from_module/1,
         get_options_from_module/1,
         get_filetype/1,
         get_src_dir/1,
         wildcard/2,
         get_env/2,
         set_env/2,
         file_last_modified_time/1,
         transform_options/2,
         get_system_modules/0
]).

-compile(export_all).

get_src_dir_from_module(Module)->
    case code:is_loaded(Module) of
        {file, _} ->
            try
                %% Get some module info...
                Props = Module:module_info(compile),
                Source = proplists:get_value(source, Props, ""),

                %% Ensure that the file exists, is a decendent of the tree, and
                %% how to deal with that
                
                IsFile = filelib:is_regular(Source),
                IsDecendant = is_path_decendent(Source),
                NonDecendants = get_env(non_descendants, fix),
                Source2 = case {IsFile, IsDecendant, NonDecendants} of
                    %% is file and descendant, we're good to go
                    {true, true,  _}    -> Source;

                    %% is not a descendant, but we allow them, so good to go
                    {true, false, allow}-> Source;

                    %% is not a file, but is a descendant, file is deleted,
                    %% nothing we can do
                    {false,true,  _}    -> undefined;

                    %% is not a descendant, and we fix non-descendants, so let's
                    %% fix it
                    {_,    false, fix}  -> find_descendant_module(Source, IsFile);

                    %% Anything else, and we don't know what to do, so let's
                    %% just bail.
                    _                   -> undefined
                end,

                case Source2 of
                    undefined -> undefined;
                    _ ->
                        %% Get the source dir...
                        Dir = filename:dirname(Source2),
                        get_src_dir(Dir)
                end
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
                Options1 = proplists:get_value(options, Props, []),
                %% transform `outdir'
                BeamDir = filename:dirname(code:which(Module)),
                Options2 = [{outdir, BeamDir} | proplists:delete(outdir, Options1)],
                %% transform `i' (Include Directory)
                IncludeDir1 = proplists:get_value(i, Options2, "include"),
                {ok, SrcDir} = get_src_dir_from_module(Module),
                {ok, IncludeDir2} = determine_include_dir(IncludeDir1, BeamDir, SrcDir),
                %% check if the module is a DTL template.
                Type = get_filetype(Module),
                    
                Options3 = [{i, IncludeDir2}, {type, Type} | proplists:delete(i, Options2)],
                {ok, Options3}
            catch _ : _ ->
                    undefined
            end;
        _ ->
            {ok, []}
    end.

%% @private This will check if the given module or source file is an ErlyDTL template.
%% Currently, this is done by checking if its reported source path ends with
%% ".dtl.erl".
get_filetype(Module) when is_atom(Module) ->
    Props = Module:module_info(compile),
    Source = proplists:get_value(source, Props, ""),
    get_filetype(Source);

get_filetype(Source) when is_list(Source) ->
    Ext = filename:extension(Source),
    Root = filename:rootname(Source),
    SecondExt = filename:extension(Root),
    case Ext of
        ".erl" when SecondExt =:= ".dtl" -> dtl;
        ".erl" -> erl;
        ".ex" -> elixir
    end.

%% @private This will search back to find an appropriate include directory, by
%% searching further back than "..". Instead, it will extract the basename
%% (probably "include" from the include pathfile, and then search backwards in
%% the directory tree until it finds a directory with the same basename found
%% above.
determine_include_dir(IncludeDir, BeamDir, SrcDir) ->
    IncludeBase = filename:basename(IncludeDir),
    case determine_include_dir_from_beam_dir(IncludeBase, BeamDir) of
        {ok, D} -> {ok, D};
        undefined ->
            {ok, Cwd} = file:get_cwd(),
            Cwd2 = normalize_case_windows_dir(Cwd),
            SrcDir2 = normalize_case_windows_dir(SrcDir),
            IncludeBase2 = normalize_case_windows_dir(IncludeBase),
            case find_include_dir_from_ancestors(Cwd2, IncludeBase2, SrcDir2) of
                {ok, D} -> {ok, D};
                undefined -> {ok, IncludeDir} %% Failed, just stick with original
            end
    end.

%% @private First try to see if we have an include file alongside our ebin
%directory, which is typically the case
determine_include_dir_from_beam_dir(IncludeBase, BeamDir) ->
    BeamBasedIncDir = filename:join(filename:dirname(BeamDir),IncludeBase),
    case filelib:is_dir(BeamBasedIncDir) of
        true -> {ok, BeamBasedIncDir};
        false -> undefined
    end.

%% @private Then we dig back through the parent directories until we find our
%include directory
find_include_dir_from_ancestors(Cwd, _, Cwd) -> undefined;
find_include_dir_from_ancestors(_, _, "/") -> undefined;
find_include_dir_from_ancestors(_, _, ".") -> undefined;
find_include_dir_from_ancestors(_, _, "") -> undefined;
find_include_dir_from_ancestors(Cwd, IncludeBase, Dir) ->
    AttemptDir = filename:join(filename:dirname(Dir),IncludeBase),
    case filelib:is_dir(AttemptDir) of
        true ->
            {ok, AttemptDir};
        false ->
            find_include_dir_from_ancestors(Cwd, IncludeBase, filename:dirname(Dir))
    end.
    
normalize_case_windows_dir(Dir) ->
    case os:type() of
        {win32,_} -> string:to_lower(Dir);
        {unix,_} -> Dir
    end.
    

%% @private This is an attempt to intelligently fix paths in modules when a
%% release is moved.  Essentially, it takes a module name and its original path
%% from Module:module_info(compile), say
%% "/some/original/path/site/src/pages/somepage.erl", and then breaks down the
%% path one by one prefixing it with the current working directory until it
%% either finds a match, or fails.  If it succeeds, it returns the Path to the
%% new Source file.
find_descendant_module([], _IsFile) ->
    undefined;
find_descendant_module(Path, IsFile) ->
    PathParts = filename:split(Path),
    {ok, Cwd} = file:get_cwd(),
    case find_descendant_module_worker(Cwd, PathParts) of
        undefined -> use_original_file_if_exists(Path, IsFile);
        FoundPath -> FoundPath
    end.

use_original_file_if_exists(Path, IsFile) ->
    case IsFile of
        true -> Path;
        false -> undefined
    end.

find_descendant_module_worker(_Cwd, []) ->
    undefined;
find_descendant_module_worker(Cwd, [_|T]) ->
    PathAttempt = filename:join([Cwd|T]),
    case filelib:is_regular(PathAttempt) of
        true -> PathAttempt;
        false -> find_descendant_module_worker(Cwd, T)
    end.

%% @private returns true if the provided path is a descendant of the current
%% working directory.
is_path_decendent(Path) ->
    {ok, Cwd} = file:get_cwd(),
    lists:sublist(Path, length(Cwd)) == Cwd.

%% @private Find the src directory for the specified Directory; max 15 iterations
get_src_dir(Dir) ->
    get_src_dir(Dir, 15).

get_src_dir(Dir, Ctr) ->
    HasCode =
        filelib:wildcard("*.erl", Dir) /= [] orelse
        filelib:wildcard("*.dtl", Dir) /= [] orelse
        filelib:wildcard("*.hrl", Dir) /= [] orelse
        filelib:wildcard("*.ex", Dir) /= [],
    if
        HasCode -> {ok,Dir};
        true -> get_src_dir(filename:dirname(Dir), Ctr - 1)
    end.

%% @private Return all files in a directory matching a regex.
wildcard(Dir, Regex) ->
    filelib:fold_files(Dir, Regex, true, fun(Y, Acc1) -> [Y|Acc1] end, []).

%% @private Get an environment variable.
get_env(Var, Default) ->
    case application:get_env(sync, Var) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

%% @private Set a sync environment variable.
set_env(Var,Val) ->
    ok = application:set_env(sync,Var,Val).

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

    LastPart = filename:basename(proplists:get_value(outdir, Options, "./ebin")),
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
