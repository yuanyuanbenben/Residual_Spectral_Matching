#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


# run for matrix denosing
nohup R --slave --vanilla < amazon_data_test1_convex.R > test.log 2>&1 &

wait
echo "All scripts have been run successfully."

