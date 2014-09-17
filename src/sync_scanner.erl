%% vim: ts=4 sw=4 et

-module(sync_scanner).
-behaviour(gen_server).
-compile(export_all).

%% API
-export([
    start_link/0,
    rescan/0,
    info/0,
    enable_patching/0,
    pause/0,
    unpause/0
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
    get_growl/0,
    set_log/1,
    get_log/0
]).

-define(SERVER, ?MODULE).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG_OR_GROWL_ON(Val),Val==true;Val==all;Val==skip_success;is_list(Val),Val=/=[]).
-define(LOG_OR_GROWL_OFF(Val),Val==false;F==none;F==[]).

-record(state, {
    modules,
    src_dirs,
    src_files,
    hrl_dirs,
    hrl_files,
    beam_lastmod,
    src_file_lastmod,
    hrl_file_lastmod,
    timers,
    patching = false,
    paused = false
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
    gen_server:cast(?SERVER, compare_hrl_files),
    ok.

unpause() ->
    gen_server:cast(?SERVER, unpause),
    ok.

pause() ->
    log_success("Pausing Sync. Call sync:go() to restart~n"),
    growl_success("Pausing Sync"),
    gen_server:cast(?SERVER, pause),
    ok.

info() ->
    io:format("Sync Info...~n"),
    gen_server:cast(?SERVER, info),
    ok.

set_growl(T) when ?LOG_OR_GROWL_ON(T) ->
    sync_utils:set_env(growl,all),
    growl_success("Sync","Desktop Notifications Enabled"),
    sync_utils:set_env(growl,T),
    ok;
set_growl(F) when ?LOG_OR_GROWL_OFF(F) ->
    sync_utils:set_env(growl,all),
    growl_success("Sync","Desktop Notifications Disabled"),
    sync_utils:set_env(growl,none),
    ok.

get_growl() ->
    sync_utils:get_env(growl, all).

set_log(T) when ?LOG_OR_GROWL_ON(T) ->
    sync_utils:set_env(log, T),
    log_success("Console Notifications Enabled~n"),
    ok;
set_log(F) when ?LOG_OR_GROWL_OFF(F) ->
    log_success("Console Notifications Disabled~n"),
    sync_utils:set_env(log, none),
    ok.

get_log() ->
    sync_utils:get_env(log, all).


enable_patching() ->
    gen_server:cast(?SERVER, enable_patching),
    ok.

init([]) ->
    %% Trap exits to catch failing processes...
    erlang:process_flag(trap_exit, true),

    %% Kick off the discovery process...
    rescan(),

    %% Display startup message...
    case {get_growl(),os:type()} of
        {true,{unix,_}} -> growl_success("Sync", "The Sync utility is now running.");
        _ -> io:format("Growl notifications disabled~n") end,

    %% Create the state and return...
    State = #state {
        modules = [],
        src_dirs = [],
        src_files = [],
        hrl_dirs = [],
        hrl_files = [],
        beam_lastmod = undefined,
        src_file_lastmod = undefined,
        hrl_file_lastmod = undefined,
        timers=[]
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(pause, State) ->
    {noreply, State#state {paused=true}};
handle_cast(unpause, State) ->
    {noreply, State#state {paused=false}};
handle_cast(_, State) when State#state.paused==true ->
    %% If paused, just absorb the request and do nothing
    {noreply, State};
handle_cast(discover_modules, State) ->
    %% Get a list of all loaded non-system modules.
    Modules = (erlang:loaded() -- sync_utils:get_system_modules()),

    %% Delete excluded modules/applications
    FilteredModules = filter_modules_to_scan(Modules),

    %% Schedule the next interval...
    NewTimers = schedule_cast(discover_modules, 30000, State#state.timers),

    %% Return with updated modules...
    NewState = State#state { modules=FilteredModules, timers=NewTimers },
    {noreply, NewState};

handle_cast(discover_src_dirs, State) ->
    %% Extract the compile / options / source / dir from each module.
    F = fun(X, Acc = {SrcAcc, HrlAcc}) ->
        %% Get the dir...
        case sync_utils:get_src_dir_from_module(X) of
            {ok, SrcDir} ->
                %% Get the options, store under the dir...
                {ok, Options} = sync_utils:get_options_from_module(X),
                %% Store the options for later reference...
                sync_options:set_options(SrcDir, Options),
                HrlDir = proplists:get_value(i, Options, []),
                %% Return the dir...
                {[SrcDir|SrcAcc], [HrlDir|HrlAcc]};
            undefined ->
                Acc
        end
    end,
    {SrcDirs, HrlDirs} = lists:foldl(F, {[], []}, State#state.modules),
    USortedSrcDirs = lists:usort(SrcDirs),
    USortedHrlDirs = lists:usort(HrlDirs),
    %% InitialDirs = sync_utils:initial_src_dirs(),

    %% Schedule the next interval...
    NewTimers = schedule_cast(discover_src_dirs, 30000, State#state.timers),

    %% Return with updated dirs...
    NewState = State#state { src_dirs=USortedSrcDirs, hrl_dirs=USortedHrlDirs, timers=NewTimers },
    {noreply, NewState};

handle_cast(discover_src_files, State) ->
    %% For each source dir, get a list of source files...
    F = fun(X, Acc) ->
        sync_utils:wildcard(X, ".*\\.erl$") ++ sync_utils:wildcard(X, ".*\\.dtl$") ++ Acc
    end,
    ErlFiles = lists:usort(lists:foldl(F, [], State#state.src_dirs)),

    %% For each include dir, get a list of hrl files...
    Fhrl = fun(X, Acc) ->
        sync_utils:wildcard(X, ".*\\.hrl$") ++ Acc
    end,
    HrlFiles = lists:usort(lists:foldl(Fhrl, [], State#state.hrl_dirs)),

    %% Schedule the next interval...
    NewTimers = schedule_cast(discover_src_files, 5000, State#state.timers),

    %% Return with updated files...
    NewState = State#state { src_files=ErlFiles, hrl_files=HrlFiles, timers=NewTimers },
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

handle_cast(compare_hrl_files, State) ->
    %% Create a list of file lastmod times...
    F = fun(X) ->
        LastMod = filelib:last_modified(X),
        {X, LastMod}
    end,
    NewHrlFileLastMod = lists:usort([F(X) || X <- State#state.hrl_files]),

    %% Compare to previous results, if there are changes, then recompile src files that depends
    process_hrl_file_lastmod(State#state.hrl_file_lastmod, NewHrlFileLastMod, State#state.src_files, State#state.patching),

    %% Schedule the next interval...
    NewTimers = schedule_cast(compare_hrl_files, 2000, State#state.timers),

    %% Return with updated hrl_file lastmod...
    NewState = State#state { hrl_file_lastmod=NewHrlFileLastMod, timers=NewTimers },
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
    Acc1 = case code:get_object_code(Module) of
        error ->
            Msg = io_lib:format("Error loading object code for ~p~n", [Module]),
            log_errors(Msg),
            growl_errors(Msg),
            {FirstBeam, OtherBeams};

        {Module, Binary, Filename} ->
            code:load_binary(Module, Filename, Binary),

            %% If patching is enabled, then reload the module across *all* connected
            %% erlang VMs, and save the compiled beam to disk.
            case EnablePatching of
                true ->
                    {ok, NumNodes} = load_module_on_all_nodes(Module),
                    Msg = io_lib:format("~s: Reloaded on ~p nodes! (Beam changed.)~n", [Module, NumNodes]),
                    log_success(Msg);
                false ->
                    %% Print a status message...
                    Msg = io_lib:format("~s: Reloaded! (Beam changed.)~n", [Module]),
                    log_success(Msg)
            end,
            case FirstBeam of
               undefined -> {Module, OtherBeams};
               _ -> {FirstBeam, [Module | OtherBeams] }
           end
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
            fire_onsync([FirstBeam]);

        {FirstBeam, N} ->
            %% Print a status message...
            growl_success("Reloaded " ++ atom_to_list(FirstBeam) ++
                              " and " ++ integer_to_list(erlang:length(N)) ++ " other beam files" ++ MsgAdd),
            fire_onsync([FirstBeam | N])
    end,
    ok;
process_beam_lastmod(undefined, _Other, _, _) ->
    %% First load, do nothing.
    ok.

fire_onsync(Modules) ->
    case sync_options:get_onsync() of
        undefined -> ok;
        Funs when is_list(Funs) -> onsync_apply_list(Funs, Modules);
        Fun -> onsync_apply(Fun, Modules)
    end.

onsync_apply_list(Funs, Modules) ->
    [onsync_apply(Fun, Modules) || Fun <- Funs].

onsync_apply({M, F}, Modules) ->
    erlang:apply(M, F, [Modules]);
onsync_apply(Fun, Modules) when is_function(Fun) ->
    Fun(Modules).

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
        Msg = io_lib:format("Reloading '~s' on ~s.~n", [Module, Node]),
        log_success(Msg),
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
            maybe_recompile_src_file(File2, LastMod2, EnablePatching),
            process_src_file_lastmod([{File1, LastMod1}|T1], T2, EnablePatching)
    end;
process_src_file_lastmod([], [{File, LastMod}|T2], EnablePatching) ->
    maybe_recompile_src_file(File, LastMod, EnablePatching),
    process_src_file_lastmod([], T2, EnablePatching);
process_src_file_lastmod([], [], _) ->
    %% Done.
    ok;
process_src_file_lastmod(undefined, _Other, _) ->
    %% First load, do nothing.
    ok.


erlydtl_compile(SrcFile, Options) ->
    erlydtl:compile(SrcFile, list_to_atom(lists:flatten(filename:basename(SrcFile, ".dtl") ++ "_dtl")), Options).

maybe_recompile_src_file(File, LastMod, EnablePatching) ->
    Module = list_to_atom(filename:basename(File, ".erl")),
    case code:which(Module) of
        BeamFile when is_list(BeamFile) ->
            %% check with beam file
            case filelib:last_modified(BeamFile) of
                BeamLastMod when LastMod > BeamLastMod ->
                    recompile_src_file(File, EnablePatching);
                _ ->
                    ok
            end;
        _ ->
            %% File is new, recompile...
            recompile_src_file(File, EnablePatching)
    end.


recompile_src_file(SrcFile, _EnablePatching) ->
    %% Get the module, src dir, and options...
    {ok, SrcDir} = sync_utils:get_src_dir(SrcFile),

    {CompileFun, Module} = case sync_utils:is_erlydtl_template(SrcFile) of
         false -> {fun compile:file/2, list_to_atom(filename:basename(SrcFile, ".erl"))};
         true -> {fun erlydtl_compile/2, list_to_atom(lists:flatten(filename:basename(SrcFile, ".dtl") ++ "_dtl"))}
     end,

    %% Get the old binary code...
    OldBinary = case code:get_object_code(Module) of
        {Module, B, _Filename} -> B;
        _ -> undefined
    end,

    case sync_options:get_options(SrcDir) of
        {ok, Options} ->
            case CompileFun(SrcFile, [binary, return|Options]) of
                {ok, Module, OldBinary, Warnings} ->
                    %% Compiling didn't change the beam code. Don't reload...
                    print_results(Module, SrcFile, [], Warnings),
                    {ok, [], Warnings};

                {ok, Module, _Binary, Warnings} ->
                    %% Compiling changed the beam code. Compile and reload.
                    CompileFun(SrcFile, Options),
                    %% Try to load the module...
                    case code:ensure_loaded(Module) of
                        {module, Module} -> ok;
                        {error, embedded} ->
                            %% Module is not yet loaded, load it.
                            case code:load_file(Module) of
                                {module, Module} -> ok
                            end
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
            Msg = io_lib:format("Unable to determine options for ~p", [SrcFile]),
            log_errors(Msg)
    end.


print_results(Module, SrcFile, [], []) ->
    Msg = io_lib:format("~s:0: Recompiled.~n", [SrcFile]),
    case code:is_loaded(Module) of
        {file, _} ->
            ok;
        false ->
            growl_success("Recompiled " ++ SrcFile ++ ".")
    end,
    log_success(lists:flatten(Msg));

print_results(_Module, SrcFile, [], Warnings) ->
    Msg = [
        format_errors(SrcFile, [], Warnings),
        io_lib:format("~s:0: Recompiled with ~p warnings~n", [SrcFile, length(Warnings)])
    ],
    growl_warnings(growl_format_errors([], Warnings)),
    log_warnings(Msg);

print_results(_Module, SrcFile, Errors, Warnings) ->
    Msg = [
        format_errors(SrcFile, Errors, Warnings)
    ],
    growl_errors(growl_format_errors(Errors, Warnings)),
    log_errors(Msg).


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
    ImagePath = filename:join([filename:dirname(code:which(sync)), "..", "icons", Image]) ++ ".png",
    Cmd = case sync_utils:get_env(executable, auto) of
              auto ->
                  case os:type() of
                      {win32, _} ->
                          make_cmd("notifu", Image, Title, Message);
                      {unix,linux} ->
                          make_cmd("notify-send", ImagePath, Title, Message);
                      _ ->
                          make_cmd("growlnotify", ImagePath, Title, Message)
                  end;
              Executable ->
                  make_cmd(Executable, Image, Title, Message)
          end,
    os:cmd(lists:flatten(Cmd)).

make_cmd(Util, Image, Title, Message) when is_atom(Util) ->
    make_cmd(atom_to_list(Util), Image, Title, Message);

make_cmd("growlnotify" = Util, Image, Title, Message) ->
    [Util, " -n \"Sync\" --image \"", Image,"\"",
     " -m \"", Message, "\" \"", Title, "\""];

make_cmd("notification_center" = _Util, _Image, Title, Message) ->
    AppleScript = io_lib:format("display notification \"~s\" with title \"~s\"", [Message, Title]),
    io_lib:format("osascript -e '~s'", [AppleScript]);

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
    can_we_growl(success)
        andalso growl("success", Title, Message).

growl_errors(Message) ->
    can_we_growl(errors)
        andalso growl("errors", "Errors...", Message).

growl_warnings(Message) ->
    can_we_growl(warnings)
        andalso growl("warnings", "Warnings", Message).

log_success(Message) ->
    can_we_log(success)
        andalso error_logger:info_msg(lists:flatten(Message)).

log_errors(Message) ->
    can_we_log(errors)
        andalso error_logger:error_msg(lists:flatten(Message)).

log_warnings(Message) ->
    can_we_log(warnings)
        andalso error_logger:warning_msg(lists:flatten(Message)).

can_we_growl(MsgType) ->
    can_we_notify(growl, MsgType).

can_we_log(MsgType) ->
    can_we_notify(log, MsgType).

can_we_notify(GrowlOrLog,MsgType) ->
    case sync_utils:get_env(GrowlOrLog, all) of
        true              -> true;
        all               -> true;
        none              -> false;
        false             -> false;
        skip_success      -> MsgType==errors orelse MsgType==warnings;
        L when is_list(L) -> lists:member(MsgType, L);
        _                 -> false
    end.

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

process_hrl_file_lastmod([{File, LastMod}|T1], [{File, LastMod}|T2], SrcFiles, Patching) ->
    %% Hrl hasn't changed, do nothing...
    process_hrl_file_lastmod(T1, T2, SrcFiles, Patching);
process_hrl_file_lastmod([{File, _}|T1], [{File, _}|T2], SrcFiles, Patching) ->
    %% File has changed, recompile...
    WhoInclude = who_include(File, SrcFiles),
    [recompile_src_file(SrcFile, Patching) || SrcFile <- WhoInclude],
    process_hrl_file_lastmod(T1, T2, SrcFiles, Patching);
process_hrl_file_lastmod([{File1, LastMod1}|T1], [{File2, LastMod2}|T2], SrcFiles, Patching) ->
    %% Lists are different...
    case File1 < File2 of
        true ->
            %% File was removed, do nothing...
            WhoInclude = who_include(File1, SrcFiles),
            case WhoInclude of
                [] -> ok;
                _ -> io:format(
                        "Warning. Deleted ~p file included in existing src files: ~p",
                        [filename:basename(File1), lists:map(fun(File) -> filename:basename(File) end, WhoInclude)])
            end,
            process_hrl_file_lastmod(T1, [{File2, LastMod2}|T2], SrcFiles, Patching);
        false ->
            %% File is new, look for src that include it
            WhoInclude = who_include(File2, SrcFiles),
            [maybe_recompile_src_file(SrcFile, LastMod2, Patching) || SrcFile <- WhoInclude],
            process_hrl_file_lastmod([{File1, LastMod1}|T1], T2, SrcFiles, Patching)
    end;
process_hrl_file_lastmod([], [{File, _LastMod}|T2], SrcFiles, Patching) ->
    %% File is new, look for src that include it
    WhoInclude = who_include(File, SrcFiles),
    [recompile_src_file(SrcFile, Patching) || SrcFile <- WhoInclude],
    process_hrl_file_lastmod([], T2, SrcFiles, Patching);
process_hrl_file_lastmod([], [], _, _) ->
    %% Done
    ok;
process_hrl_file_lastmod(undefined, _Other, _,  _) ->
    %% First load, do nothing
    ok.

who_include(HrlFile, SrcFiles) ->
    HrlFileBaseName = filename:basename(HrlFile),
    Pred = fun(SrcFile) ->
        {ok, Forms} = epp_dodger:parse_file(SrcFile),
        is_include(HrlFileBaseName, Forms)
        end,
    lists:filter(Pred, SrcFiles).

is_include(_HrlFile, []) ->
    false;
is_include(HrlFile, [{tree, attribute, _, {attribute, _, [{_, _, IncludeFile}]}} | Forms]) when is_list(IncludeFile) ->
    IncludeFileBaseName = filename:basename(IncludeFile),
    case IncludeFileBaseName of
        HrlFile -> true;
        _ -> is_include(HrlFile, Forms)
    end;
is_include(HrlFile, [_SomeForm | Forms]) ->
    is_include(HrlFile, Forms).

%% @private Filter the modules to be scanned.
filter_modules_to_scan(Modules) ->
    exclude_modules_to_scan(whitelist_modules_to_scan(Modules)).

%% @private Filter the whitelisted modules.
whitelist_modules_to_scan(Modules) ->
    case application:get_env(sync, whitelisted_modules) of
        {ok, WhitelistedModules} when is_list(WhitelistedModules) andalso
                                   WhitelistedModules =/= []->
            lists:foldl(fun(Module, Acc) ->
                                case lists:member(Module, WhitelistedModules) of
                                    true ->
                                        [Module | Acc];
                                    false ->
                                        Acc
                                end
                        end, [], Modules);
        _ ->
            Modules
    end.

%% @private Filter the excluded modules.
exclude_modules_to_scan(Modules) ->
    case application:get_env(sync, excluded_modules) of
        {ok, ExcludedModules} when is_list(ExcludedModules) andalso
                                   ExcludedModules =/= []->
            lists:foldl(fun(Module, Acc) ->
                                case lists:member(Module, ExcludedModules) of
                                    true ->
                                        Acc;
                                    false ->
                                        [Module | Acc]
                                end
                        end, [], Modules);
        _ ->
            Modules
    end.
