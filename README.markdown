# Sync: Recompile and Reload Changed Erlang Code

Sync is a developer tool to help you hot-load changed code in an
Erlang system. 

When you start the application using `sync:go()` or
`application:start(sync)`, the system automatically starts checking
for changes to all loaded modules (except for Erlang system
modules). Sync will recompile and reload the module if the source has
changed, or if any include files for a module have changed.

Compilation errors are logged using the error_logger.