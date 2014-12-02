%% vim: ts=4 sw=4 et

-module(sync_notify).

%% API
-export([
    startup/1,
    growl_success/1,
    growl_success/2,
    growl_errors/1,
    growl_warnings/1,
    log_success/1,
    log_errors/1,
    log_warnings/1
]).

startup(Growl) ->
    case {Growl, os:type()} of
        {none, _} ->
            growl_startup_disabled_message();
        {false, _} -> 
            growl_startup_disabled_message();
        _ ->
            growl_startup_success_message()
    end.

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

%%% PRIVATE FUNCTIONS %%%

growl(Image, Title, Message) ->
    ImagePath = filename:join([filename:dirname(code:which(sync)), "..", "icons", Image]) ++ ".png",
    Cmd = case sync_utils:get_env(executable, auto) of
              auto ->
                  case os:type() of
                      {win32, _} ->
                          make_cmd("notifu", ImagePath, Title, Message);
                      {unix,linux} ->
                          make_cmd("notify-send", ImagePath, Title, Message);
                      _ ->
                          make_cmd("growlnotify", ImagePath, Title, Message)
                  end;
              Executable ->
                  make_cmd(Executable, ImagePath, Title, Message)
          end,
    Cmd2 = lists:flatten(Cmd),
    os:cmd(Cmd2).

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
    Type = image2notifu_type(Image),
    ImageWin = windows_image(Image),
    [Util, " /q /d 5000 /i \"", ImageWin, "\" /t ", Type,
     " /p \"", Title, "\" /m \"", Message, "\""];

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

windows_image(Image) ->
    FixedSlashes = string:join(string:tokens(Image, "/"),"\\"),
    filename:rootname(FixedSlashes) ++ ".ico".

image2notifu_type(Image) ->
    case filename:rootname(filename:basename(Image)) of
        "success" -> "info";
        "warnings" -> "warn";
        "errors" -> "error"
    end.

growl_startup_disabled_message() ->
    io:format("Growl notifications disabled~n").

growl_startup_success_message() ->
    growl_success("Sync", "The Sync utility is now running.").

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

