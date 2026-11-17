#!/bin/sh
# ============================================================================
# Run ONE homoscedastic family of the simulation study -- the six families of
# run_paper.sh (size / cond / noise / supp / robust / dep) plus the
# misspecified-rank family (see run_misrank.sh).
#
# Usage:  sh run_homo.sh <family> [npar] [nrep] [outdir]
#   npar   parallel R processes            (default 100)
#   nrep   repetitions per configuration   (default 100, the paper's count)
#   outdir defaults to output_homo_<family>/
#
# Protocol: RSM_STOP=val RSM_INIT=oracle RSM_CAP=1500 HOMO_TOR=1e-4.
# ============================================================================
set -e
cd "$(dirname "$0")"
. ./lib_grids.sh

FAM=${1:?usage: sh run_homo.sh <size|cond|noise|supp|robust|dep> [npar] [nrep] [outdir]}
NPAR=${2:-100}
NREP=${3:-100}
OUT=${4:-output_homo_${FAM}}

export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 GOTO_NUM_THREADS=1
export RSM_STOP=val RSM_INIT=oracle RSM_CAP=1500 HOMO_TOR=1e-4
SKIP_EXISTING=${SKIP_EXISTING:-1}; export SKIP_EXISTING
mkdir -p "$OUT"

Q=$(mktemp "${TMPDIR:-/tmp}/homo_jobs_${FAM}.XXXXXX")
trap 'rm -f "$Q"' EXIT
homo_job_specs "$FAM" "$NREP" > "$Q"
NJOB=$(wc -l < "$Q")
echo "family=$FAM jobs=$NJOB npar=$NPAR -> $OUT"

cat "$Q" | xargs -P "$NPAR" -n 4 sh -c \
  'Rscript homo_job.R '"$FAM"' "$3" '"$OUT"' "$0" "$1" "$2" \
   > "'"$OUT"'/log_$0_$1_$2_$3.txt" 2>&1 || echo "!! FAILED '"$FAM"' $0 $1 $2 $3"' \
  || echo "!! some jobs of $FAM failed (see $OUT/log_*.txt)"

echo "ALL DONE: $(ls $OUT/*.csv 2>/dev/null | wc -l) result files of $NJOB"
