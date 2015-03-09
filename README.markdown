# Stay in Sync

## What is Sync?

Sync is a developer utility. It recompiles and reloads your Erlang code
on-the-fly. With Sync, you can code without friction.

![Successful compilation image.](http://rusty.io.s3.amazonaws.com/sync/sync_01.png)

What does "code without friction" mean? It means that with Sync running, you no
longer need to worry about running `make`, or `c:l(Module)` again. Just write
code, save the file, and watch as Erlang automatically detects your changes,
recompiles the code, and reloads the module.

## How can I use Sync?

### Install via rebar dependency

```erlang
{deps, [
		{sync, ".*",
			{git, "git://github.com/rustyio/sync.git", {branch, "master"}}}
]}.
```

### Manual install

```bash
cd $ERL_LIBS
git clone git@github.com:rustyio/sync.git
(cd sync; make)
```

The recommended approach is to put sync in your $ERL_LIBS directory.

Then, go in the Erlang console of an application you are developing, run
`sync:go().`. You can also start sync using `application:start(sync).`

Starting up:

```
(rustyio@127.0.0.1)6> sync:go().

Starting Sync (Automatic Code Compiler / Reloader)
Scanning source files...
ok
08:34:18.609 [info] Application sync started on node 'rustyio@127.0.0.1'
```

Successfully recompiling a module:

```
08:34:43.255 [info] /Code/Webmachine/src/webmachine_dispatcher.erl:0: Recompiled.
08:34:43.265 [info] webmachine_dispatcher: Reloaded! (Beam changed.)
```

Warnings:

```
08:35:06.660 [info] /Code/Webmachine/src/webmachine_dispatcher.erl:33: Warning: function dispatch/3 is unused
```

Errors:

```
08:35:16.881 [info] /Code/Webmachine/src/webmachine_dispatcher.erl:196: Error: function reconstitute/1 undefined
/Code/Webmachine/src/webmachine_dispatcher.erl:250: Error: syntax error before: reconstitute
```

## Stopping and Pausing

You can stop the `sync` application entirely (wiping its internal state) with
`sync:stop()`. You can then restart the application with a new state using `sync:go()`

If, however, you just wish to pause `sync` so that it will not update anything
during some period, you can pause the scanner with `sync:pause()`.  You might
do this while upgrading you wish not to have immediately loaded until
everything is complete. Calling `sync:go()` once again will unpause the scanner.

Bear in mind that running `pause()` will not stop files that are currently
being compiled.

## Specifying folders to sync

To your erlang `config` add

```erlang
[
    {sync, [
        {src_dirs, {strategy(), [src_dir_descr()]}}
    ]}
].
```
```erlang
-type strategy() :: add | replace.
````
If `strategy()` is `replace`, sync will use ONLY specified dirs to sync. If `strategy()` is `add`, sync will add specific dirs to list of dirs to sync.

```erlang
-type src_dir_descr() :: { Dir :: file:filename(), [Options :: compile_option()]}.
```
You probably want to specify `outdir` compile option.

For example
```erlang
[
    {sync, [
        {src_dirs, {replace, [{"./priv/plugins", [{outdir,"./priv/plugins_bin"}]}]}}
    ]}
].
```

## Console Logging

By default, sync will print sucess / warning / failure notifications to the
erlang console.  You can control this behaviour with the `log` configuration options.

### Valid Values For `log`

* `all`: Print all console notifications
* `none`: Print no console notifications
* `[success | warnings | errors]`: A list of any combination of the atoms
  `success`, `warnings`, or `errors`.  Example: `[warnings, errors]` will only
  show warnings and errors, but suppress success messages.
* `true`: An alias to `all`, kept for backwards compatibility
* `false`: An alias to `none`, kept for backwards compatibility
* `skip_success`: An alias to `[errors, warnings]`, kept for backwards compatibility.

The `log` value can be specified in any of the following ways:

#### 1. Loaded from a .config file

	{log, all},	
	{log, [success, warnings]},

#### 2. As an environment variable called from the erlang command line:

	erl -sync log all
	erl -sync log none

#### 3. From within the Erlang shell:

	sync:log(all);
	sync:log(none);
	sync:log([errors, warnings]);

## Desktop Notifications

Sync can pop success / warning / failure notifications onto your desktop to
keep you informed of compliation results. All major operating systems are
supported: Mac via [Growl](http://growl.info), Linux via Libnotify, Windows via
[Notifu](http://www.paralint.com/projects/notifu/) and Emacs via lwarn /
message command. Below are Growl screenshots.


Success:

![Successful compilation image.](http://rusty.io.s3.amazonaws.com/sync/sync_01.png)

Warnings:

![Compilation warnings image.](http://rusty.io.s3.amazonaws.com/sync/sync_02.png)

Errors:

![Compilation errors image.](http://rusty.io.s3.amazonaws.com/sync/sync_03.png)

### Disabling Desktop Notifications

Desktop notifications follow the same conventions as the console logging above,
and can be selectively enabled or disabled with the `growl` configuration variable:

### Valid Values For `growl`

* `all`: Print all console notifications
* `none`: Print no console notifications
* `[success | warnings | errors]`: A list of any combination of the atoms
  `success`, `warnings`, or `errors`.  Example: `[warnings, errors]` will only
  show warnings and errors, but suppress success messages.
* `true`: An alias to `all`, kept for backwards compatibility
* `false`: An alias to `none`, kept for backwards compatibility
* `skip_success`: An alias to `[errors, warnings]`, kept for backwards compatibility.

The `growl` value can be specified in any of the following ways:

#### 1. Loaded from a .config file

	{growl, all},	
	{growl, [success, warnings]},

#### 2. As an environment variable called from the erlang command line:

	erl -sync growl all
	erl -sync growl none

#### 3. From within the Erlang shell:

	sync:growl(all);
	sync:growl(none);
	sync:growl([errors, warnings]);

### Troubleshooting Growl Notifications

Sync attempts to auto-detect the notification package to use via the
`os:type()` command.

If this isn't working for you, or you would like to override the default, use
the `executable` configuration parameter:

	{executable, TYPE}

Where `TYPE` is:
+ `'auto'` Autodetermine (default behaviour)
+ `'growlnotify'` for Mac / Growl.
+ `'notification_center'` for Mac OS X built-in Notification Center.
+ `'notify-send'` for Linux / libnotify.
+ `'notifu'` for Windows / Notifu.
+ `'emacsclient'` for Emacs notifications.

Like all configuration parameters, this can also be specified when launching
Erlang with:

    erl -sync executable TYPE

## Remote Server "Patching"

If you are developing an application that runs on an Erlang cluster, you may
need to recompile a module on one node, and then broadcast the changed module
to other nodes. Sync helps you do that with a feature called "patching."

To use the patching feature:

1. Connect to any machine in your cluster via distributed erlang. A simple
   `net_adm:ping(Node)` should suffice.

2. Run `sync:patch()`. This will start the Sync application if it's not already
   started, and enable "patch mode".

3. Start editing code.

Sync will detect changes to code, recompile your modules, and then sent the
updated modules to every Erlang node connected to your cluster. If the module
already exists on the node, then it will be overwritten on disk with the new
.beam file and reloaded. If the module doesn't exist on the new node, then it
is simply updated in memory.

## How does Sync work?

Upon startup, Sync gathers information about loaded modules, ebin directories,
source files, compilation options, etc.

Sync then periodically checks the last modified date of source files. If a file
has changed since the last scan, then Sync automatically recompiles the module
using the previous set of compilation options. If compilation was successful,
it loads the updated module. Otherwise, it prints compilation errors to the
console.

Sync also periodically checks the last modified date of any beam files, and
automatically reloads the file if it has changed.

The scanning process adds 1% to 2% CPU load on a running Erlang VM. Much care
has been taken to keep this low. Shouldn't have to say this, but this is for
development mode only, don't run it in production.

## Sync Post-hooks

You can register a post-hook to run after Sync reloads modules. This can allow
you to run tests on modules if you like, or anything else for that matter.

You can register a post-hook with:

```erlang
sync:onsync(fun(Mods) ->
				io:format("Reloaded Modules: ~p~n",[Mods]) 
			end).
```

This will simply print the list of modules that were successfully recompiled.

For example, if you wanted to automatically run a unit test on each reloaded
module that has a `test()` function exported, you could do the following:

```erlang
RunTests = fun(Mods) ->
	[Mod:test() || Mod <- Mods, erlang:function_exported(Mod, test, 0)]
end,
sync:onsync(RunTests).
```

A post-hook can also be specified as a `{Module,Function}` tuple, which assumes
`Module:Function/1`

*Note:* Currently, only one post-hook can be registered at a time.

### Unregistering a post-hook

To unregister a post-hook, just call

	sync:onsync(undefined).

## Whitelisting/Excluding modules from the scanning process

Sometimes you may want to focus only on a few modules, or prevent some modules
from being scanned by sync. To achive this just modify `whitelisted_modules` or
`excluded_modules` configuration parameter in the
[node's config file](http://www.erlang.org/doc/man/config.html).

Beyond specifying modules one by one, identified by atoms, you can also specify
them in bulk, identified by regular expressions, but with a slower sync.

## Moving Application Location

Previously, if an entire application (like a reltool-generated release) was
moved from one location to another, sync would fail to recompile files that
were changed until all the beams were remade.  While this is typically as
simple as typing `rebar compile`, it was still a hassle.

The solution to this was to enable the ability for sync to "heal" the paths
when it turned out they had been moved.

The way this works is by checking if the `source` path inside the beam is a
file that exists, and by checking if that path is a descendant of the root of
your application.  If sync has been set to fix the paths, and module's source
is pointing at a path that isn't a descendant of the current working directory,
it will attempt to find the correct file.

You can change how this will be handled with a `non_descendants` setting in the
config:

* `fix`: Fix any file that isn't a descendant

* `allow`: Use the original path in the module, regardless of its location,
  recompiling only if the original location changes.

* `ignore`: If a file is not a descendant, sync will completely ignore it.

## A note about Nitrogen

The `{sync_mode, nitrogen}` option is no longer necessary for users of the
[Nitrogen Web Framework](http://nitrogenproject.com) and will be ignored. Sync
works with Nitrogen without that option.

## Sample Configuration File

Please note that sync loads with the following defaults:

```erlang
[
	{sync, [
		{growl, all},
		{log, all},
		{non_descendants, fix},
		{executable, auto},
		{whitelisted_modules, []},
		{excluded_modules, []}
	]}
].
```

You can view a full sample configuration file
([sync.sample.config](https://github.com/rustyio/sync/blob/master/sync.sample.config))
that you're free to include in your application. Just be sure to use the
`-config` switch for the `erl` program:

	erl -config sync.config

## Sync with relx

If you use [relx](https://github.com/erlware/relx) and wish to use sync with a
relx created release, you'll need to run relx with -d option (symlink all
applications and configuration into the release instead of copying) add a
`syntax_tools` and `compiler` to `release` section to your relx.config file:

```erlang
{release, {your_app, "1.0.0"}, [
	syntax_tools,
	compiler
]}.
```
