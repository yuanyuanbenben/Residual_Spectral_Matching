#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


# run for matrix denosing

nohup R --slave --vanilla < realdata1_residual.R > test.log 2>&1 &
nohup R --slave --vanilla < realdata2_residual.R > test2.log 2>&1 &
nohup R --slave --vanilla < realdata3_residual.R > test3.log 2>&1 &
nohup R --slave --vanilla < realdata4_residual.R > test4.log 2>&1 &
nohup R --slave --vanilla < realdata1_residual_convex.R > test5.log 2>&1 &
nohup R --slave --vanilla < realdata2_residual_convex.R > test6.log 2>&1 &
nohup R --slave --vanilla < realdata3_residual_convex.R > test7.log 2>&1 &
nohup R --slave --vanilla < realdata4_residual_convex.R > test8.log 2>&1 &

wait
echo "All scripts have been run successfully."

