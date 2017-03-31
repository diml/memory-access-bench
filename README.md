Memory access benchmarks. The benchmark is a function that exchange
the source and destination of a UDP packet. It is implemented using:

- bigarray
- bytes
- raw pointers + compiler primitives
- 2-aligned raw pointers as integers + compiler primitives
- raw pointers + C stubs
- 2-aligned raw pointers as integers + C stubs

Results:

```
  Name                                                Time/Run   mWd/Run   Percentage
 --------------------------------------------------- ---------- --------- ------------
  [mem_bench.ml:With_bigarray]                          6.83ns                 15.29%
  [mem_bench.ml:With_bytes]                             5.51ns                 12.33%
  [mem_bench.ml:With_raw_pointers]                      8.03ns                 17.99%
  [mem_bench.ml:With_2aligned_raw_pointers_as_int]      6.71ns                 15.02%
  [mem_bench.ml:With_raw_pointers_c]                   23.04ns                 51.59%
  [mem_bench.ml:With_2aligned_raw_pointers_c]          44.66ns    12.00w      100.00%
```
