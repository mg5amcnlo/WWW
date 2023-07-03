    executable     = xyz

# here you specify where to put .log, .out and .err files
    output         = /dev/null
    error          = condor/err.dat
    log            = condor/log.dat

# the following two parameters enable the file transfer mechanism
# any specify that the output files should be transferred back
# to the submit machine from the remote machine where the job executes
    should_transfer_files   = YES
    transfer_input_files    =/home/madgraph/WWW/cgi-bin/VBF/run/makeruncard, /home/madgraph/WWW/cgi-bin/VBF/run/vbf
    when_to_transfer_output = ON_EXIT

# the following two parameters are required for the ingrid cluster
    universe       = vanilla
    requirements   = (MADGRAPH =?= TRUE)

    queue 1
