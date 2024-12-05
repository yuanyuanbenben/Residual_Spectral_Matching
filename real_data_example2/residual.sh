#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT


nohup R --slave --vanilla < amazon1_residual.R > test.log 2>&1 &
nohup R --slave --vanilla < amazon1_residual_convex.R > test2.log 2>&1 &
nohup R --slave --vanilla < amazon2_residual.R > test3.log 2>&1 &
nohup R --slave --vanilla < amazon2_residual_convex.R > test4.log 2>&1 &
nohup R --slave --vanilla < amazon3_residual.R > test5.log 2>&1 &
nohup R --slave --vanilla < amazon3_residual_convex.R > test6.log 2>&1 &
nohup R --slave --vanilla < amazon4_residual.R > test7.log 2>&1 &
nohup R --slave --vanilla < amazon4_residual_convex.R > test8.log 2>&1 &
wait

echo "All scripts have been run successfully."

