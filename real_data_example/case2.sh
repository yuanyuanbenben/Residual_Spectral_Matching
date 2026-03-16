#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


# run for matrix denosing
nohup R --slave --vanilla < realdata2_test.R > test4.log 2>&1 &

wait
echo "All scripts have been run successfully."

