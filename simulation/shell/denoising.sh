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

# run for matrix denosing
nohup R --slave --vanilla < matrix_denoising.R > test.log 2>&1 &

wait
echo "All scripts have been run successfully."

