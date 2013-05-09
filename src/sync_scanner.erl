%% -*- mode: nitrogen -*-

-module(sync_scanner).
-behaviour(gen_server).
-compile(export_all).

%% API
-export([
    start_link/0,
    rescan/0,
    info/0,
    enable_patching/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    set_growl/1,
    get_growl/0
]).

-define(SERVER, ?MODULE).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(state, {
    modules,
    src_dirs,
    src_files,
    beam_lastmod,
    src_file_lastmod,
    timers,
    patching = false
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

rescan() ->
    io:format("Scanning source files...~n"),
    gen_server:cast(?SERVER, discover_modules),
    gen_server:cast(?SERVER, discover_src_dirs),
    gen_server:cast(?SERVER, discover_src_files),
    gen_server:cast(?SERVER, compare_beams),
    gen_server:cast(?SERVER, compare_src_files),
    ok.

info() ->
    io:format("Sync Info...~n"),
    gen_server:cast(?SERVER, info),
    ok.

%% I know it's kinda sloppy to get and set env vars to determine if we should
%% be printing growl messages, but, hey, it works, and since growl/3 is called
%% from within the server, we can't call gen_server:call to get the value or it
%% will just hang. So env vars is the easy copout like using the process dict
%% TODO: make not use env_var for this :)
set_growl(true) ->
    sync_utils:set_env(growl,true),
    growl_success("Sync","Notifications Enabled"),
    ok;
set_growl(skip_success) ->
    growl_success("Sync","Notifications Enabled (skip success)"),
    sync_utils:set_env(growl,skip_success),
    ok;
set_growl(false) ->
    growl_success("Sync","Notifications Disabled"),
    sync_utils:set_env(growl,false),
    ok.

get_growl() ->
    case sync_utils:get_env(growl,true) of
        Val when is_boolean(Val) -> Val;
        _ -> true
    end.

enable_patching() ->
    gen_server:cast(?SERVER, enable_patching),
    ok.

init([]) ->
    %% Trap exits to catch failing processes...
    erlang:process_flag(trap_exit, true),

    %% Kick off the discovery process...
    rescan(),

    %% Display startup message...
    case get_growl() of
        true ->
            growl_success("Sync", "The Sync utility is now running.");
        false ->
            io:format("Growl notifications disabled~n")
    end,

    %% Create the state and return...
    State = #state {
        modules = [],
        src_dirs = [],
        src_files = [],
        beam_lastmod = undefined,
        src_file_lastmod = undefined,
        timers=[]
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(discover_modules, State) ->
    %% Get a list of all loaded non-system modules.
    Modules = erlang:loaded() -- sync_utils:get_system_modules(),

    %% Schedule the next interval...
    NewTimers = schedule_cast(discover_modules, 30000, State#state.timers),

    %% Return with updated modules...
    NewState = State#state { modules=Modules, timers=NewTimers },
    {noreply, NewState};

handle_cast(discover_src_dirs, State) ->
    %% Extract the compile / options / source / dir from each module.
    F = fun(X, Acc) ->
        %% Get the dir...
        case sync_utils:get_src_dir_from_module(X) of
            {ok, Dir} ->
                %% Get the options, store under the dir...
                {ok, Options1} = sync_utils:get_options_from_module(X),
                Options2 = sync_utils:transform_options(Dir, Options1),

                %% Store the options for later reference...
                sync_options:set_options(Dir, Options2),

                %% Return the dir...
                [Dir|Acc];
            undefined ->
                Acc
        end
    end,
    Dirs = lists:usort(lists:foldl(F, [], State#state.modules)),

    %% Schedule the next interval...
    NewTimers = schedule_cast(discover_src_dirs, 30000, State#state.timers),

    %% Return with updated dirs...
    NewState = State#state { src_dirs=Dirs, timers=NewTimers },
    {noreply, NewState};

handle_cast(discover_src_files, State) ->
    %% For each source dir, get a list of source files...
    F = fun(X, Acc) ->
        sync_utils:wildcard(X, ".*\.erl$") ++ Acc
    end,
    Files = lists:usort(lists:foldl(F, [], State#state.src_dirs)),

    %% Schedule the next interval...
    NewTimers = schedule_cast(discover_src_files, 5000, State#state.timers),

    %% Return with updated files...
    NewState = State#state { src_files=Files, timers=NewTimers },
    {noreply, NewState};

handle_cast(compare_beams, State) ->
    %% Create a list of beam file lastmod times...
    F = fun(X) ->
        Beam = code:which(X),
        LastMod = filelib:last_modified(Beam),
        {X, LastMod}
    end,
    NewBeamLastMod = lists:usort([F(X) || X <- State#state.modules]),

    %% Compare to previous results, if there are changes, then reload
    %% the beam...
    process_beam_lastmod(State#state.beam_lastmod, NewBeamLastMod, State#state.patching),

    %% Schedule the next interval...
    NewTimers = schedule_cast(compare_beams, 2000, State#state.timers),

    %% Return with updated beam lastmod...
    NewState = State#state { beam_lastmod=NewBeamLastMod, timers=NewTimers },
    {noreply, NewState};

handle_cast(compare_src_files, State) ->
    %% Create a list of file lastmod times...
    F = fun(X) ->
        LastMod = filelib:last_modified(X),
        {X, LastMod}
    end,
    NewSrcFileLastMod = lists:usort([F(X) || X <- State#state.src_files]),

    %% Compare to previous results, if there are changes, then recompile the file...
    process_src_file_lastmod(State#state.src_file_lastmod, NewSrcFileLastMod, State#state.patching),

    %% Schedule the next interval...
    NewTimers = schedule_cast(compare_src_files, 1000, State#state.timers),

    %% Return with updated src_file lastmod...
    NewState = State#state { src_file_lastmod=NewSrcFileLastMod, timers=NewTimers },
    {noreply, NewState};

handle_cast(info, State) ->
    io:format("Modules: ~p~n", [State#state.modules]),
    io:format("Source Dirs: ~p~n", [State#state.src_dirs]),
    io:format("Source Files: ~p~n", [State#state.src_files]),
    {noreply, State};

handle_cast(enable_patching, State) ->
    NewState = State#state { patching = true },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% PRIVATE FUNCTIONS %%%

schedule_cast(Msg, Default, Timers) ->
    %% Cancel the old timer...
    TRef = proplists:get_value(Msg, Timers),
    timer:cancel(TRef),

    %% Lookup the interval...
    IntervalKey = list_to_atom(atom_to_list(Msg) ++ "_interval"),
    Interval = sync_utils:get_env(IntervalKey, Default),

    %% Schedule the call...
    {ok, NewTRef} = timer:apply_after(Interval, gen_server, cast, [?SERVER, Msg]),

    %% Return the new timers structure...
    lists:keystore(Msg, 1, Timers, {Msg, NewTRef}).

process_beam_lastmod(A, B, EnablePatching) ->
    process_beam_lastmod(A, B, EnablePatching, {undefined, []}).

process_beam_lastmod([{Module, LastMod}|T1], [{Module, LastMod}|T2], EnablePatching, Acc) ->
    %% Beam hasn't changed, do nothing...
    process_beam_lastmod(T1, T2, EnablePatching, Acc);
process_beam_lastmod([{Module, _}|T1], [{Module, _}|T2], EnablePatching, {FirstBeam, OtherBeams}) ->
    %% Beam has changed, reload...
    {Module, Binary, Filename} = code:get_object_code(Module),
    code:load_binary(Module, Filename, Binary),

    %% If patching is enabled, then reload the module across *all* connected
    %% erlang VMs, and save the compiled beam to disk.
    case EnablePatching of
        true ->
            {ok, _NumNodes} = load_module_on_all_nodes(Module);
        false ->
            ok
    end,
    Acc1 = case FirstBeam of
               undefined -> {Module, OtherBeams};
               _ -> {FirstBeam, [Module | OtherBeams] }
           end,
    process_beam_lastmod(T1, T2, EnablePatching, Acc1);

process_beam_lastmod([{Module1, LastMod1}|T1], [{Module2, LastMod2}|T2], EnablePatching, Acc) ->
    %% Lists are different, advance the smaller one...
    case Module1 < Module2 of
        true ->
            process_beam_lastmod(T1, [{Module2, LastMod2}|T2], EnablePatching, Acc);
        false ->
            process_beam_lastmod([{Module1, LastMod1}|T1], T2, EnablePatching, Acc)
    end;
process_beam_lastmod([], [], EnablePatching, Acc) ->
    MsgAdd = case EnablePatching of
                 true -> " on " ++ integer_to_list(length(get_nodes())) ++ " nodes.";
                 false -> "."
             end,
    %% Done.
    case Acc of
        {undefined, []} ->
            nop; % nothing changed
        {FirstBeam, []} ->
            %% Print a status message...
            growl_success("Reloaded " ++ atom_to_list(FirstBeam) ++ MsgAdd),
            growl_success("Calling metatyper"),
            send_metatyper(Acc);

        {FirstBeam, N} ->
            %% Print a status message...
            growl_success("Reloaded " ++ atom_to_list(FirstBeam) ++
                              " and " ++ integer_to_list(erlang:length(N)) ++ " other beam files" ++ MsgAdd),
            send_metatyper(Acc)            
    end,
    ok;
process_beam_lastmod(undefined, _Other, _, _) ->
    %% First load, do nothing.
    ok.

send_metatyper(Acc) ->
    case erlang:module_loaded(meta_typer_server) of
        false -> growl_errors("MetaTyper not loaded");
        true  -> call_metatyper(Acc)
    end.



call_metatyper({FirstBeam, Rest}) ->
    try erlang:apply(meta_typer_server, module_reload, [[FirstBeam | Rest]]) of
        {fail, Message} ->
            Error = io_lib:format("Failed property test:~n~p", [Message]),
            growl_errors(Error);
        _ ->
            growl_success("Property tests passed!")
    catch
        _ -> growl_errors("Failed to run property tests.")
    end.

get_nodes() ->
    lists:usort(lists:flatten(nodes() ++ [rpc:call(X, erlang, nodes, []) || X <- nodes()])) -- [node()].

load_module_on_all_nodes(Module) ->
    %% Get a list of nodes known by this node, plus all attached
    %% nodes.
    Nodes = get_nodes(),
    io:format("[~s:~p] DEBUG - Nodes: ~p~n", [?MODULE, ?LINE, Nodes]),
    NumNodes = length(Nodes),

    {Module, Binary, _} = code:get_object_code(Module),
    F = fun(Node) ->
        io:format("[~s:~p] DEBUG - Node: ~p~n", [?MODULE, ?LINE, Node]),
        error_logger:info_msg("Reloading '~s' on ~s.~n", [Module, Node]),
        rpc:call(Node, code, ensure_loaded, [Module]),
        case rpc:call(Node, code, which, [Module]) of
            Filename when is_binary(Filename) orelse is_list(Filename) ->
                %% File exists, overwrite and load into VM.
                ok = rpc:call(Node, file, write_file, [Filename, Binary]),
                rpc:call(Node, code, purge, [Module]),
                {module, Module} = rpc:call(Node, code, load_file, [Module]);
            _ ->
                %% File doesn't exist, just load into VM.
                {module, Module} = rpc:call(Node, code, load_binary, [Module, undefined, Binary])
        end,
        growl_success("Reloaded " ++ atom_to_list(Module) ++ " on " ++ atom_to_list(Node) ++ ".")
    end,
    [F(X) || X <- Nodes],
    {ok, NumNodes}.

process_src_file_lastmod([{File, LastMod}|T1], [{File, LastMod}|T2], EnablePatching) ->
    %% Beam hasn't changed, do nothing...
    process_src_file_lastmod(T1, T2, EnablePatching);
process_src_file_lastmod([{File, _}|T1], [{File, _}|T2], EnablePatching) ->
    %% File has changed, recompile...
    recompile_src_file(File, EnablePatching),
    process_src_file_lastmod(T1, T2, EnablePatching);
process_src_file_lastmod([{File1, LastMod1}|T1], [{File2, LastMod2}|T2], EnablePatching) ->
    %% Lists are different...
    case File1 < File2 of
        true ->
            %% File was removed, do nothing...
            process_src_file_lastmod(T1, [{File2, LastMod2}|T2], EnablePatching);
        false ->
            %% File is new, recompile...
            recompile_src_file(File2, EnablePatching),
            process_src_file_lastmod([{File1, LastMod1}|T1], T2, EnablePatching)
    end;
process_src_file_lastmod([], [{File, _LastMod}|T2], EnablePatching) ->
    %% File is new, recompile...
    recompile_src_file(File, EnablePatching),
    process_src_file_lastmod([], T2, EnablePatching);
process_src_file_lastmod([], [], _) ->
    %% Done.
    ok;
process_src_file_lastmod(undefined, _Other, _) ->
    %% First load, do nothing.
    ok.

recompile_src_file(SrcFile, EnablePatching) ->
    %% Get the module, src dir, and options...
    Module = list_to_atom(filename:basename(SrcFile, ".erl")),
    {ok, SrcDir} = sync_utils:get_src_dir(SrcFile),

    %% Get the old binary code...
    OldBinary = case code:get_object_code(Module) of
        {Module, B, _Filename} -> B;
        _ -> undefined
    end,

    case sync_options:get_options(SrcDir) of
        {ok, Options} ->
            case compile:file(SrcFile, [binary, return|Options]) of
                {ok, Module, OldBinary, Warnings} ->
                    %% Compiling didn't change the beam code. Don't reload...
                    print_results(Module, SrcFile, [], Warnings),
                    {ok, [], Warnings};

                {ok, Module, _Binary, Warnings} ->
                    %% Compiling changed the beam code. Compile and reload.
                    compile:file(SrcFile, Options),
                    case EnablePatching of
                        true -> code:ensure_loaded(Module);
                        false -> ok
                    end,
                    gen_server:cast(?SERVER, compare_beams),

                    %% Print the warnings...
                    print_results(Module, SrcFile, [], Warnings),
                    {ok, [], Warnings};

                {error, Errors, Warnings} ->
                    %% Compiling failed. Print the warnings and errors...
                    print_results(Module, SrcFile, Errors, Warnings),
                    {ok, Errors, Warnings}
            end;

        undefined ->
            error_logger:error_msg("Unable to determine options for ~p", [SrcFile])
    end.


print_results(_Module, _SrcFile, [], []) ->
    %% Do not print message on successful compilation;
    %% We already get a notification when the beam is reloaded.
    ok;

print_results(_Module, SrcFile, [], Warnings) ->
    Msg = [
        format_errors(SrcFile, [], Warnings),
        io_lib:format("~s:0: Recompiled with ~p warnings~n", [SrcFile, length(Warnings)])
    ],
    growl_warnings(growl_format_errors([], Warnings)),
    error_logger:info_msg(lists:flatten(Msg));

print_results(_Module, SrcFile, Errors, Warnings) ->
    Msg = [
        format_errors(SrcFile, Errors, Warnings)
    ],
    growl_errors(growl_format_errors(Errors, Warnings)),
    error_logger:info_msg(lists:flatten(Msg)).


%% @private Print error messages in a pretty and user readable way.
format_errors(File, Errors, Warnings) ->
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

%% @private Print error messages in a pretty and user readable way.
growl_format_errors(Errors, Warnings) ->
    AllErrors1 = lists:sort(lists:flatten([X || {_, X} <- Errors])),
    AllErrors2 = [{Line, "Error", Module, Description} || {Line, Module, Description} <- AllErrors1],
    AllWarnings1 = lists:sort(lists:flatten([X || {_, X} <- Warnings])),
    AllWarnings2 = [{Line, "Warning", Module, Description} || {Line, Module, Description} <- AllWarnings1],
    Everything = lists:sort(AllErrors2 ++ AllWarnings2),
    F = fun({Line, Prefix, Module, ErrorDescription}) ->
        Msg = Module:format_error(ErrorDescription),
        io_lib:format("~p: ~s: ~s~n", [Line, Prefix, Msg])
    end,
    [F(X) || X <- Everything].

growl(Image, Title, Message) ->
    case get_growl() of
        false -> ok;
        true ->
            ImagePath = filename:join([filename:dirname(code:which(sync)), "..", "icons", Image]) ++ ".png",

            Cmd = case application:get_env(sync, executable) of
                      undefined ->
                          case os:type() of
                              {win32, _} ->
                                  make_cmd("notifu", Image, Title, Message);
                              {unix,linux} ->
                                  make_cmd("notify-send", ImagePath, Title, Message);
                              _ ->
                                  make_cmd("growlnotify", ImagePath, Title, Message)
                          end;
                      {ok, Executable} ->
                          make_cmd(Executable, Image, Title, Message)
                  end,
            os:cmd(lists:flatten(Cmd))
    end.

make_cmd("growlnotify" = Util, Image, Title, Message) ->
    [Util, " -n \"Sync\" --image \"", Image,"\"",
     " -m \"", Message, "\" \"", Title, "\""];

make_cmd("notify-send" = Util, Image, Title, Message) ->
    [Util, " -i \"", Image, "\"",
     " \"", Title, "\" \"", Message, "\" --expire-time=5000"];

make_cmd("notifu" = Util, Image, Title, Message) ->
    %% see http://www.paralint.com/projects/notifu/
    [Util, " /q /d 5000 /t ", image2notifu_type(Image), " ",
     "/p \"", Title, "\" /m \"", Message, "\""];

make_cmd("emacsclient" = Util, "warnings", Title, Message0) ->
    Message = lisp_format(Message0),
    io_lib:format("~s --eval \"(mapc (lambda (m) (lwarn \\\"sync: ~s\\\" :warning m)) (list ~s))\"",
                  [Util, Title, Message]);
make_cmd("emacsclient" = Util, "errors", Title, Message0) ->
    Message = lisp_format(Message0),
    io_lib:format("~s --eval \"(mapc (lambda (m) (lwarn \\\"sync: ~s\\\" :error m)) (list ~s))\"",
                  [Util, Title, Message]);
make_cmd("emacsclient" = Util, _, Title, Message0) ->
    Message = replace_chars(Message0, [{$\n, "\\n"}]),
    io_lib:format("~s --eval \"(message \\\"[sync] ~s: ~s\\\")\"",
                  [Util, Title, Message]);

make_cmd(UnsupportedUtil, _, _, _) ->
    error('unsupported-sync-executable',
           lists:flatten(io_lib:format("'sync' application environment variable "
                                       "named 'executable' has unsupported value: ~p",
                                       [UnsupportedUtil]))).


image2notifu_type("success") -> "info";
image2notifu_type("warnings") -> "warn";
image2notifu_type("errors") -> "error".

growl_success(Message) ->
    growl_success("Success!", Message).

growl_success(Title, Message) ->
    case sync_utils:get_env(growl,true) of
        skip_success -> ok;
        _            -> growl("success", Title, Message)
    end.

growl_errors(Message) ->
    growl("errors", "Errors...", Message).

growl_warnings(Message) ->
    growl("warnings", "Warnings", Message).

%% Return a new string with chars replaced.
%% @spec replace_chars(iolist(), [{char(), char() | string()}] -> iolist().
replace_chars(String, Tab) ->
    lists:map(fun (C) ->
                      proplists:get_value(C, Tab, C)
              end,
              lists:flatten(String)).

%% Return a new string constructed of source lines double quoted and
%% delimited by space.
%% spec lisp_format(StringOfLines :: iolist()) -> string().
lisp_format(String0) ->
    String1 = lists:flatten(String0),
    Lines1 = string:tokens(String1, [$\n]),
    String2 = string:join(Lines1, "\\\" \\\""),
    lists:flatten(["\\\"", String2, "\\\""]).
