# Trajectory demo consolidation — design

**Date:** 2026-07-07
**Branch:** `feat/trajectory` (PR #69 to mihem/cerebroAppLite)
**Status:** design approved, pending spec review

## Motivation

PR #69 (pseudotime Trajectory tab) received review feedback:

- **mihem:** (1) document where the trajectory demo dataset comes from and how it
  was generated, the way the T/BCR demos already are; (2) avoid too many
  datasets — "can't we integrate monocle into one of the existing ones?"
- **ChatGPT review (2 × Medium):** a blank-tab bug and a zero-cell `sample()`
  crash in the trajectory UI.

The current `inst/extdata/v1.4/demo_trajectory.crb` is an opaque binary added in
commit `4f288c1` with **no `data-raw/` build script** — exactly what mihem
flagged. It carries `pbmc_10k_v3` (501 cells) with a single `monocle2` method
(two trajectories: `all_cells`, `subset_of_cells`).

## Decision

**Merge the monocle2 trajectory into the existing `demo_full_tcr_bcr.crb` and
delete the standalone `demo_trajectory.crb`.** One demo then shows TCR + BCR +
trajectory together; dataset count drops by one; nothing is downloaded that
isn't already used.

Rejected alternatives:

- *Swap in a "biologically perfect" B-cell lineage dataset* (eLife GSE214265,
  276 cells, non-10x; or 10x 1k-B, fastq-only needing Cell Ranger). Both break
  one-command reproducibility and diverge from the existing IR-demo philosophy
  (public 10x data, scripted rebuild). mihem never asked to change the data.
- *Keep `pbmc_10k_v3` and only add a build script.* Answers the "document
  source" half but not the "fewer datasets" half.

### Why the IR trio is NOT merged

`demo_full_tcr_bcr.crb` / `demo_healthy_t.crb` / `demo_bcell_rich.crb` stay as
three separate files. Their whole point is to differ (cell composition, UMAP,
TCR/BCR all change on switch) so the multi-crb switching feature has something
to demonstrate. They already have `build_ir_demos.R` + README — the standard
mihem wants trajectory to match. mihem's "too many datasets" was about isolated
per-feature datasets, not this intra-IR variation.

Final demo ledger: keep IR trio (one of them, full T+B, additionally gains a
monocle2 trajectory); delete `demo_trajectory.crb`; net −1 dataset, no lost
feature demo.

## Feasibility (verified)

Inspected `demo_full_tcr_bcr.crb`:

- 1476 cells total; `cell_type` = 915 B cells / 493 T cells / 68 Monocytes.
- Expression matrix 967 × 1476, log-normalized (range 0–8.35) → valid monocle2
  input.
- Trajectory slot structure (from the current demo) is `meta` (DR_1, DR_2,
  pseudotime, state) + `edges` (source, target, weight, source_dim_1/2,
  target_dim_1/2) — reproducible via `addTrajectory()`.
- R6 API confirmed: `getExpressionMatrix()`, `getMetaData()`, `addTrajectory()`,
  `getMethodsForTrajectories()`, `getNamesOfTrajectories()`, `getTrajectory()`.

## Data flow

```
demo_full_tcr_bcr.crb
  getExpressionMatrix()  → 967 × 1476 log-norm
  getMetaData()$cell_type == "B cells"  → 915-cell subset
        │
        ▼
monocle2:  newCellDataSet → estimateSizeFactors/Dispersions
        → setOrderingFilter(high-variance genes)
        → reduceDimension(method = "DDRTree")
        → orderCells()            (set.seed(42) for determinism)
        │  → pseudotime + state + DDRTree skeleton
        ▼
addTrajectory("monocle2", "B_cell_maturation", coords + edges)
        ▼
demo_full_tcr_bcr.crb   (re-exported, one extra trajectory slot)
delete demo_trajectory.crb
```

Trajectory is computed on the **915 B cells only** — the one lineage in this
demo with some differentiation continuity, sharing barcodes with the BCR
clonotypes so the narrative is self-consistent. Named `B_cell_maturation`;
documented honestly as illustrative (PBMC, not a bone-marrow developmental
lineage).

## Components

### 1. `data-raw/build_trajectory_demo.R` (new)

Mirrors `build_ir_demos.R` conventions: header comment with honest data
provenance, env-overridable paths, `set.seed(42)`.

Steps: read `demo_full_tcr_bcr.crb` → subset B cells → run monocle2 (DDRTree) →
`addTrajectory("monocle2", "B_cell_maturation", …)` → overwrite
`demo_full_tcr_bcr.crb` in place.

Dependency `monocle` (Bioconductor, monocle2 line) is used **only in this
data-raw script**, NOT added to `DESCRIPTION` — same treatment
`build_ir_demos.R` gives `scRepertoire` (build-time, not runtime).

### 2. `data-raw/README.md` (edit)

Add a "Trajectory demo" section: trajectory derives from the full T+B demo's
B-cell subset, monocle2 DDRTree, `seed = 42`, plus the honest "illustrative /
not a developmental lineage" note.

### 3. Bug fixes (ChatGPT review)

**Bug 1 — blank tab.** `inst/shiny/v1.4/shiny_server.R:562` inserts the
conditional tab on raw `getMethodsForTrajectories()` (any method), but
`inst/shiny/v1.4/trajectory/select_method_and_name.R:15` filters the UI to
`%in% c('monocle2')` while its empty-check (line 17) still tests the *unfiltered*
list. A .crb with only a non-monocle2 method gets a tab that renders blank
instead of the "No trajectories" message.
*Fix:* gate both the tab-visibility condition and the UI empty-check on the
**supported** methods (monocle2), so unsupported methods fall through to the
existing `trajectory_missing` empty state. Single source of truth for the
supported-method list.

**Bug 2 — zero-cell `sample()`.** `inst/shiny/v1.4/trajectory/projection_plot.R:47`
`cells_df[sample(1:nrow(cells_df)), ]` — when group filters empty the frame,
`1:nrow` is `1:0` = `c(1, 0)`, so `sample` emits an NA row and downstream
plot/range/hover break.
*Fix:* guard `if (nrow(cells_df) == 0)` before subsetting/plotting (return an
empty-state plot/message); replace `1:nrow(...)` with `seq_len(nrow(...))` to
kill the degenerate case at the root.

### 4. Delete `demo_trajectory.crb` + dangling refs

`grep -r demo_trajectory` across tests, demo-loading lists, and docs; remove the
file and every reference so nothing points at a missing file.

## Testing / verification

1. Run `data-raw/build_trajectory_demo.R`; confirm `demo_full_tcr_bcr.crb` gains
   a `monocle2 / B_cell_maturation` trajectory and still carries TCR + BCR.
2. `scripts/precheck.sh fast` (air + testthat) green in the worktree.
3. Hot-reload the app, drive the Trajectory tab with Playwright: B-cell
   trajectory renders; emptying a group filter shows the empty state, no crash;
   the "No trajectories" message path still works.

## Scope / non-goals

- Not changing the IR trio or the multi-crb switching feature.
- Not adding a runtime dependency on `monocle`.
- Not pursuing a new "perfect" B-cell developmental dataset (documented as a
  possible separate future PR, out of scope here).
