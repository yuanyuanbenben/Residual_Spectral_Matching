#!/bin/bash

function terminate_scripts {
    echo "Terminating running scripts..."
    pkill -P $$  
    exit 1
}


trap terminate_scripts SIGINT

# Get the directory where the script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIMULATION_DIR="$(dirname "$SCRIPT_DIR")"

# Change to simulation directory
cd "$SIMULATION_DIR"

# run for convex mathod simulation

echo "r=5 s=6"
nohup R --slave --vanilla --args 500 250 20 102 6 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "r=5 s=7"
nohup R --slave --vanilla --args 500 250 20 102 7 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "r=5 s=8"
nohup R --slave --vanilla --args 500 250 20 102 8 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "r=5 s=9"
nohup R --slave --vanilla --args 500 250 20 102 9 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "r=5 s=10"
nohup R --slave --vanilla --args 500 250 20 102 10 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait

echo "r=5 s=11"
nohup R --slave --vanilla --args 500 250 20 102 11 5 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=10 s=11"
nohup R --slave --vanilla --args 500 250 20 102 11 10 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=10 s=12"
nohup R --slave --vanilla --args 500 250 20 102 12 10 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=10 s=13"
nohup R --slave --vanilla --args 500 250 20 102 13 10 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=10 s=14"
nohup R --slave --vanilla --args 500 250 20 102 14 10 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=10 s=15"
nohup R --slave --vanilla --args 500 250 20 102 15 10 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=10 s=16"
nohup R --slave --vanilla --args 500 250 20 102 16 10 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=20 s=21"
nohup R --slave --vanilla --args 500 250 20 102 21 20 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=20 s=22"
nohup R --slave --vanilla --args 500 250 20 102 22 20 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=20 s=23"
nohup R --slave --vanilla --args 500 250 20 102 23 20 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=20 s=24"
nohup R --slave --vanilla --args 500 250 20 102 24 20 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=20 s=25"
nohup R --slave --vanilla --args 500 250 20 102 25 20 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "r=20 s=26"
nohup R --slave --vanilla --args 500 250 20 102 26 20 100 10 < compare_to_baseline.R > test.log 2>&1 &
echo "waiting..."
wait


echo "All scripts have been run successfully."

