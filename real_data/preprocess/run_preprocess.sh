#!/bin/bash
# ============================================================================
# Rebuild the preprocessed real-data CSVs.
#
#   ./run_preprocess.sh netflix <netflix_data.csv> [out_root]
#   ./run_preprocess.sh amazon  <raw_dir>          [out_root]
#
# The output root is REAL_DATA_ROOT, the optional [out_root] argument, or (if
# neither is given) matrix_test/ next to the release -- auto-detected if it
# already holds real_data_example/ or real_data_example2/, and created
# otherwise.
# ============================================================================
set -eu
cd "$(dirname "$0")"
RELEASE=$(cd ../.. && pwd)

if [ -z "${REAL_DATA_ROOT:-}" ] && [ $# -lt 3 ]; then
  for d in "$RELEASE/matrix_test" "$RELEASE/../matrix_test" \
           "$RELEASE/../../matrix_test" "$RELEASE/data"; do
    if [ -d "$d/real_data_example" ] || [ -d "$d/real_data_example2" ]; then
      REAL_DATA_ROOT=$(cd "$d" && pwd); break
    fi
  done
  : "${REAL_DATA_ROOT:=$RELEASE/../matrix_test}"
  echo "output root: $REAL_DATA_ROOT" >&2
fi
if [ -n "${REAL_DATA_ROOT:-}" ]; then
  export REAL_DATA_ROOT
fi

MODE=${1:-}
if [ $# -gt 0 ]; then shift; fi

case "$MODE" in
  netflix) Rscript netflix_preprocess.R "$@" ;;
  amazon)  Rscript amazon_preprocess.R "$@" ;;
  *) echo "usage: $0 netflix <netflix_data.csv> [out_root] | amazon <raw_dir> [out_root]" >&2; exit 1 ;;
esac
