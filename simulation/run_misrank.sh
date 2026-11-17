#!/bin/sh
# ============================================================================
# The "Misspecified rank" family -- the supplement's misrank tables.
#
#   m=500, n=250, rho=0.2, sigma=1; true rank r in {5,10,20}; the rank actually
#   USED, s, runs from r to r+6; nrep reps.  Metrics E_F / E_sp / E_inf.
#   -> output_misrank/   (base_fac / base_nuc and the RSM arms rsm_fac / rsm_nuc)
#
# Usage: sh run_misrank.sh [npar] [nrep] [outdir]   (defaults 200, 100)
# ============================================================================
set -e
cd "$(dirname "$0")"
. ./lib_grids.sh

NPAR=${1:-200}
NREP=${2:-100}
OUT=${3:-output_misrank}

export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 GOTO_NUM_THREADS=1
export RSM_STOP=val RSM_INIT=oracle RSM_CAP=1500 HOMO_TOR=1e-4
SKIP_EXISTING=${SKIP_EXISTING:-1}; export SKIP_EXISTING
mkdir -p "$OUT"

Q=$(mktemp "${TMPDIR:-/tmp}/misrank_jobs.XXXXXX")
trap 'rm -f "$Q"' EXIT
homo_job_specs misrank "$NREP" > "$Q"
NJOB=$(wc -l < "$Q")
echo "family=misrank jobs=$NJOB npar=$NPAR -> $OUT (HOMO_TOR=$HOMO_TOR)"

cat "$Q" | xargs -P "$NPAR" -n 4 sh -c '
  Rscript homo_job.R misrank "$3" '"$OUT"' "$0" "$1" "$2" \
    > "'"$OUT"'/log_$0_$1_$2_$3.txt" 2>&1 || echo "!! FAILED misrank $0 $1 $2 $3"' \
  || echo "!! xargs reported a non-zero exit (see the per-job logs)"

N=$(ls "$OUT"/homo_misrank_*.csv 2>/dev/null | wc -l)
echo "ALL DONE: $N / $NJOB result files"
echo "log scan (ARM ERROR = an arm was silently swallowed to NA):"
grep -rl "ARM ERROR\|Error\|Execution halted" "$OUT"/log_*.txt 2>/dev/null | head -20 || true
find "$OUT" -name 'log_*.txt' -size 0 | head -20
