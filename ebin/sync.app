{application,sync,
             [{description,"Sync - Automatic Code Reloader"},
              {vsn,"0.1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{sync,[]}},
              {env,[{compile_inteval,1000},{out_file,"/tmp/sync.out"}]},
              {modules,[sync,sync_worker]}]}.
