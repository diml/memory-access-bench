#!/usr/bin/env sh
export BENCHMARKS_RUNNER=TRUE
export BENCH_LIB=mem_bench
exec ./_build/default/main.exe -run-without-cross-library-inlining "$@"
