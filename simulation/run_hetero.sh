#!/bin/sh
# ============================================================================
# Run the heteroscedastic experiment, one job per (mode, sigma, rep):
# modes 1..4 (row-Bernoulli / row-Gaussian / rowcol-Bernoulli /
# rowcol-Gaussian) x the 10-value sigma grid x nrep reps.
#
# Usage:  sh run_hetero.sh [npar] [outdir] [nrep]
#   npar   parallel R processes            (default 100)
#   outdir defaults to output_hetero
#   nrep   repetitions per cell            (default 100, the paper's count)
#
# Protocol: RSM_STOP=val RSM_INIT=oracle RSM_CAP=1500.
# ============================================================================
set -e
cd "$(dirname "$0")"
. ./lib_grids.sh

NPAR=${1:-100}
OUT=${2:-output_hetero}
NREP=${3:-100}

export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 GOTO_NUM_THREADS=1
export RSM_STOP=val RSM_INIT=oracle RSM_CAP=1500
SKIP_EXISTING=${SKIP_EXISTING:-1}; export SKIP_EXISTING
mkdir -p "$OUT"

Q=$(mktemp "${TMPDIR:-/tmp}/hetero_jobs.XXXXXX")
trap 'rm -f "$Q"' EXIT
hetero_job_specs "$NREP" > "$Q"
NJOB=$(wc -l < "$Q")
echo "hetero jobs=$NJOB npar=$NPAR -> $OUT"

cat "$Q" | xargs -P "$NPAR" -n 3 sh -c \
  'Rscript hetero_job.R "$0" "$1" "$2" '"$OUT"' \
   > "'"$OUT"'/log_${0}_${1}_${2}.txt" 2>&1 || echo "!! FAILED hetero $0 $1 $2"' \
  || echo "!! some hetero jobs of $OUT failed"

echo "ALL DONE: $(ls $OUT/*.csv 2>/dev/null | wc -l) result files of $NJOB"
