# Package design and performance boundaries

Keep public functions small and compose a few focused internal operations.
Abstraction should separate policy, computation and I/O without introducing
per-record R calls, extra collection, hidden copies or an extensible framework
that the package does not need.

## ICD layers

Public `icd_flag()` is local-only. `thip_filter_icd()` constructs a lazy row
filter using the shared SQL normalization and predicates. The former public
SQL flag dispatch remains `.icd_flag_experimental()` for regression work.

### Modules

| Module | Responsibility | Execution cost |
|---|---|---|
| `R/icd_flag.R` | Public local-data contract | Metadata only |
| `R/icd_spec.R` | Shared column/definition validation, definition preparation, R normalization and local value checks | Definitions once per call; local value scans only for local data |
| `R/icd_flag_local.R` | Local orchestration, distinct-code matching, sparse aggregation | Vectorized work over selected data; no per-record R callbacks |
| `R/icd_sql_helpers.R` | DBI quoting, SQL normalization, validation expression, matching predicate and nullable flag construction | Query construction; no data retrieval |
| `R/icd_flag_sql.R` | Row and experimental distinct-code query shapes | Internal experiments; execution remains explicit |

Both SQL strategies consume the same normalized source and matching policy.
They differ only in query shape. Local and SQL normalization implement the
same documented rules in their respective runtimes; a shared test contract
checks agreement. Backend-specific errors are still checked where meaningful:
R validates local values, while SQL validates storage types and ID values when
the query executes. Do not download data just to validate a lazy query.

Definitions are normalized and deduplicated once before dispatch. Internal
engines consume prepared definitions and do not repeat public input checks.
Helpers are internal, not additional public APIs. The row SQL strategy remains
the default because the distinct strategy lost the measured synthetic comparison.
No strategy is selected by an unvalidated speed heuristic.

## Allocation discipline

Vectorize work over records. The production matcher uses whole-vector
normalization, `match()`, `startsWith()` and compiled sparse matrix operations.
Its R loops range over diagnosis columns, condition lists or their definition
codes, not individual input records. Replacing a `for` loop with `lapply()`
alone does not make the work vectorized or parallel. Keep small setup loops
when they are clear, and benchmark changes to the expensive vector operations.
Vectorization can still create large temporary vectors, so check allocations
alongside elapsed time. The row-wise reference matcher is intentionally limited
to tiny synthetic tests and is not part of the production execution path.

The local path converts a data-frame subclass once, creates one first-seen ID
result, matches distinct codes, and aggregates only codes relevant to at least
one definition. Numeric IDs are checked numerically rather than converted to
text for blank detection. Condition-hit indices are flattened once. A dense
all-FALSE matrix is allocated only when there are no matching codes; otherwise
the dense result comes directly from the sparse product.

Modularity does not make the current implementation out of core. It still
flattens the selected diagnosis columns and repeats row-to-ID indices. With
N rows and D diagnosis columns, those temporary vectors scale with N times D.
The final output is dense: I distinct IDs and K conditions require about
4 * I * K bytes for logical values alone. Ten million IDs and 50 conditions
therefore require roughly 2 GB just for the flag payload, before IDs, source
data, matrix workspaces and copies. Sparse intermediates do not remove this
output cost.

For large analyses, select required fields and periods, use bounded extracts,
and measure peak memory as well as total runtime. Future chunking must retain
all records for each ID or explicitly merge partial flags while preserving
TRUE/FALSE/NA semantics. Do not independently flag arbitrary row chunks and
concatenate them. A compact code-index representation or long/sparse output
would be separate, benchmarked changes to the current execution/output contract.

## Shared tests and performance acceptance

`inst/testdata/icd-contract.R` contains synthetic fixtures and an independent,
deliberately simple reference matcher. Offline tests and both packaged smoke
checks reuse it. The reference does not call production normalization,
matching or aggregation helpers, preventing the same bug from supplying both
actual and expected results. Tests cover public behavior rather than requiring
a particular private function call sequence.

When changing an engine:

1. Preserve exact/prefix behavior, dots/case/whitespace normalization, literal
   punctuation, empty definitions, repeated IDs, leading zeros and missingness.
2. Preserve ID types and output order for local data; compare sorted SQL output.
3. Keep query construction lazy, DBI quoting intact and source filters retained.
4. Check equality against the independent reference and previous implementation.
5. Measure runtime and memory on representative synthetic workloads before
   claiming a speed or scale benefit. Keep performance timing out of unit-test
   pass/fail thresholds. Record environment, workload, output equality and what
   the measurement includes.

The modular refactor retained byte-identical SQL in its regression scenarios,
so no live database access was needed to repeat the same generated queries.
Future changes to SQL semantics require opt-in, read-only integration checks.

## Connection and discovery

`setup.R` orchestrates one-time configuration, frontend prompts and optional
verification. `environ.R` handles literal `.Renviron` serialization and atomic,
owner-only persistence. `connection.R` owns configuration validation, connection
construction and SELECT-only server verification; setup reuses those operations
rather than implementing another connection path. Credential values are never
returned or included in setup messages. Persistence is independent of the driver
and tested with synthetic values in temporary files.

`thip_connect()` reads the R process environment on each call, with explicit
argument overrides. Normal R startup loads `.Renviron`; the package does not
silently read files on attach or modify startup hooks. A Jupyter API token belongs
to external development tooling and is not a database credential or package API.

`discovery.R` performs metadata access, compatible table union construction,
and the deprecated bounded-collection compatibility helper. Keep transport, decoding and general dbplyr
translation upstream. Reuse a validator when its meaning is the same; do not
force ICD column/list rules through credential or connection-setting validators.

## Window retrieval layers

- `window.R`: public planning and standard collect/print methods.
- `validation.R`: reusable column/schema checks, with no source-row queries.
- `window_spec.R`: typed key/date validation, column roles and endpoint normalization.
- `window_sql.R`: quoted SELECT-only source projection and person/window matching.
- `window_execute.R`: complete person batches, local index/matcher and final assembly.

One person index is built for a local source, avoiding a full local scan per
batch. The local overlap engine returns match indices; reconstruction always
uses original source columns. SQL explicitly projects source fields and joins
with ALL multiplicity so platform join defaults cannot discard overlapping
entries. Equality ON plus a range WHERE supports older ClickHouse versions.
Window data are SQL literals; no temporary tables or session state are created.

The plan records unique entry ordinals and each person appears in only one
batch, so execution creates no duplicate entry-record pairs of its own. Real
source duplicates and legitimate matches to several entries are retained.
Result rows map back to the original cohort entry-key type in R. Failures yield
an error, not a partial data frame. Tests compare complete pair multisets,
original dates and unmatched-entry counts with an independent tiny reference.

Plans support source filtering before construction, not arbitrary subsequent
lazy verbs. Metadata/zero-row schema inspection is distinct from batched
execution. Collect holds the final result in RAM; there is no streaming or
on-disk promise. Batching is a transport/resource policy, not a row cap.
