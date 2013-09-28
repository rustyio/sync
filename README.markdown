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

The recommended approach is to put sync in your $ERL_LIBS directory.

```bash
cd $ERL_LIBS
git clone git@github.com:rustyio/sync.git
(cd sync; make)
```

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

## Console Logging

By default, sync will print sucess / warning / failure notifications to the
erlang console.  You can control this behaviour with the following
configuration options

#### 1. Loaded from a .config file

	{log, true},		%% print all activity to console
	{log, false}		%% print no activity
	{log, skip_success}	%% Print only warnings and failures

#### 2. As an environment variable called from the erlang command line:

	erl -sync log true
	erl -sync log false
	erl -sync log skip_success

#### 3. From within the Erlang shell:

	sync:log(true);
	sync:log(false);
	sync:log(skip_success);

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

If you find the desktop notifications annoying, you can disable them in one of
two ways:

#### 1. As a config value for the `sync` application

	{growl, true},
	{growl, false},
	{growl, skip_success}

#### 2. As an environment variable called from the erlang command line:

    erl -sync growl false
    erl -sync growl true
	erl -sync growl skip_success

#### 3. From within the Erlang shell:

    sync:growl(true).
    sync:growl(false).
	sync:growl(skip_success).

### Troubleshooting Growl Notifications

Sync attempts to auto-detect the notification package to use via the
`os:type()` command.

If this isn't working for you, or you would like to override the default, use
the `executable` configuration parameter:

	{executable, TYPE}

Where `TYPE` is:
+ `'auto'` Autodetermine (default behaviour)
+ `'growlnotify'` for Mac / Growl.
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

## Excluding modules from the scanning process

Sometimes you may want to prevent some modules from being scanned by sync. To
achive this just modify `excluded_modules` configuration paramter in the
[node's config file](http://www.erlang.org/doc/man/config.html).

## Moving Application Location

Previously, if an entire application (like a reltool-generated release) was
moved from one location to another, sync would fail to recompile files that
were changed until all the beams were remade.  While this is typically as
simple as typing `rebar compile`, it was still a hassle.

The solution to this was to enable the ability for sync to "heal" the paths
when it turned out they had been moved.

The way this works is by checking if the `source` path inside the beam is a
file that exists, and by checking if that path is a decendant of the root of
your application.  If sync has been set to fix the paths, and module's source
is pointing at a path that isn't a decendant of the current working directory,
it will attempt to find the correct file.

You can change how this will be handled with a `non_decendants` setting in the
config:

* `fix`: Fix any file that isn't a decendant

* `allow`: Use the original path in the module, regardless of its location,
  recompiling only if the original location changes.

* `ignore`: If a file is not a decendant, sync will completely ignore it.

## A note about Nitrogen

The `{sync_mode, nitrogen}` option is no longer necessary for users of the
[Nitrogen Web Framework](http://nitrogenproject.com) and will be ignored. Sync
works with Nitrogen without that option.

## Sample Configuration File

Please note that sync loads with the following defaults:

```erlang
[
	{sync, [
		{growl, true},
		{log, true},
		{non_decendants, fix},
		{executable, auto}
	]}
].
```
