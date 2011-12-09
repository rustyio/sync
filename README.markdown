# Stay in Sync

## What is it?

Sync is an Erlang utility that helps you code faster by automatically compiling and hot-loading changed code.

## How can I use sync?

The recommended approach is to put sync in your $ERL_LIBS directory.

```bash
cd $ERL_LIBS
git clone git@github.com:rustyio/sync.git
(cd sync; make)
```

Then, go in the Erlang console of an application you are developing, run `sync:go().`. You can also start sync using `application:start(sync).`

## How does it work?

Upon startup, Sync gathers information about loaded modules, ebin directories, source files, compilation options, etc. 

Sync then periodically checks the last modified date of source files. If a file has changed since the last scan, then Sync automatically recompiles the module using the previous set of compilation options. If compilation was successful, it loads the updated module. Otherwise, it prints compilation errors to the console.

Sync also periodically checks the last modified date of any beam files, and automatically reloads the file if it has changed.

The scanning process adds 1% to 2% CPU load on a running Erlang VM. Much care has been taken to keep this low. Shouldn't have to say this, but this is for development mode only, don't run it in production.

## Using it with Nitrogen

If you are running sync with the [Nitrogen Web Framework](http://www.nitrogenproject.com), be sure to add the following line to your etc/vm.args file:

```txt
-sync sync_mode nitrogen
```


## Growl Notifications

If you are running a Mac and have [Growl](http://growl.info) and the **growlnotify** utility installed, Sync will pop up Growl notifications with compilation results.  This will also work on Linux if you have **notify-send** (Fedora: `libnotify` package, Ubuntu: `libnotify-bin` package):

Successful compilation:

![Successful compilation image.](http://rusty.io.s3.amazonaws.com/sync/sync_01.png)

Warnings:

![Compilation warnings image.](http://rusty.io.s3.amazonaws.com/sync/sync_02.png)

Errors:

![Compilation errors image.](http://rusty.io.s3.amazonaws.com/sync/sync_03.png)

### Disabling Growl Notifications

If you find the Growl/notify-send notifications annoying, you can choose to disable them with two ways:

#### 1. As an environment variable called from the erlang command line:

    erl -sync growl false
    erl -sync growl true   #this is the default


#### 2. From within the Erlang shell:

    sync:growl(true).    % Enable notifications
    sync:growl(false).   % Disable notifications

## Remote Server "Patching"

If you are developing an application that runs on an Erlang cluster,
you may need to recompile a module on one node, and then broadcast the
changed module to other nodes. Sync helps you do that with a feature
called "patching."

To use the patching feature:

1. Connect to any machine in your cluster via distributed
erlang. A simple `net_adm:ping(Node)` should suffice.

2. Run `sync:patch()`. This will start the Sync application if it's not
already started, and enable "patch mode".

3. Start editing code.

Sync will detect changes to code, recompile your modules, and then
sent the updated modules to every Erlang node connected to your
cluster. If the module already exists on the node, then it will be
overwritten on disk with the new .beam file and reloaded. If the
module doesn't exist on the new node, then it is simply updated in
memory.

