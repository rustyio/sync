# Sync: Recompile and Reload Changed Erlang Code

Sync is a developer tool to help you hot-load changed code in an
Erlang system. 

When you start the application using `sync:go()` or
`application:start(sync)`, the system automatically starts checking
for changes to all loaded modules (except for Erlang system
modules). Sync will recompile and reload the module if the source has
changed, or if any include files for a module have changed.

Compilation errors are logged using the error_logger.

*** EMACS PRO TIP ***

Sync outputs Emacs friendly compiler messages. This means you can
navigate directly to your error messages with a single click. 

Simply:

1. Open a `shell` or `eshell` buffer.
2. `tail -f *` on your log directory.
3. Turn on `compilation-minor-mode`.


# New Solution
+ Create an index of every module in the system to every file on disk.
+ Using the modules, create an index of .hrl files to .beam files.
+ Create a list of unique directories in modules from previous step.
+ Create an index of every .hrl file in every directory, including the local directory. 
+ Create an index of every .erl file in every directory, including the local directory.
+ Check the last update time of each src file.
+ If it's newer than the module, then recompile.