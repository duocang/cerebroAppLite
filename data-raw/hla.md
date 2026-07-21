# HLA & TCR motif demos — design and rebuild notes

Provenance, citations and licences live in [`DATASETS.md`](DATASETS.md).
This file carries the design reasoning, the exact acquisition steps, and the known problems of each demo.

Three demos ship. Each answers a different question; the newest one is the only place where real receptors, real genotypes and a legible network meet, and it gets there by using an antigen-selected repertoire.

| demo | cells | TCR | HLA genotype | what it is for | build script |
|---|---|---|---|---|---|
| `demo_hla_tcr_synthetic.crb` | synthetic | synthetic | synthetic | shows the page working on a dense network; proves nothing about real data | `build_hla_tcr_demo.R` |
| `demo_hla_tcr_bulk.crb` | none (bulk) | **real** | **real, independently measured** | HLA Associations on genuine genotypes | `build_hla_tcr_bulk_demo.R` |
| `demo_hla_tcr_dextramer.crb` | **real** | **real** | **real, published (table S1)** | the motif network on measured sequences | `build_hla_tcr_dextramer_demo.R` |

---

# 1. Why a real single-cell demo was needed

The fair objection to the motif page is: *if the network is only legible on synthetic data, what is the feature for?*

The answer is that a CDR3 Hamming-1 network needs an **antigen-selected** repertoire.
An unselected polyclonal repertoire is sparse in CDR3 space: neighbours at distance 1 are rare, and adding cells does not fix it — pair count grows roughly with n², and a few thousand cells still extrapolates to almost nothing.
A selected repertoire converges instead: different donors independently arrive at near-identical CDR3s against the same epitope (public / convergent recombination), which is exactly what the network draws.

That is a claim, so it was measured — on one real source, with the package's own motif core:

| subset of the same donor | unique CDR3β | result |
|---|---|---|
| all cells, unselected | 26,449 | trips the size guard; nothing to draw |
| cells binding any dextramer | 2,910 | **308 nodes in 75 motifs** |
| one epitope, Flu-MP `GILGFVFTL` | 267 | **121 nodes in 7 motifs** |

The last row is the argument: against one immunodominant influenza epitope, 45 % of the observed CDR3s collapse into **seven** families.
The predecessor experiment is the control — the earlier real-sequence demo built from an unselected repertoire rendered a **4-node** graph (TRB: 456 unique CDR3 → 2 Hamming-1 pairs).
Same code, different kind of repertoire.

---

# 2. `demo_hla_tcr_dextramer.crb` — the real antigen-selected demo

## 2.1 Source

10x Genomics, *CD8+ T cells of Healthy Donor 1–4* (2019) — the dextramer / Immune Map experiment published as:

> Zhang W, Hawkins PG, He J, Gupta NT, Liu J, Choonoo G, Jeong SW, Chen CR, Dhanik A, Dillon M, Deering R, Macdonald LE, Thurston G, Atwal GS.
> *A framework for highly multiplexed dextramer mapping and prediction of T cell receptor sequences to antigen specificity.*
> **Science Advances** 7(20):eabf5835 (2021). <https://doi.org/10.1126/sciadv.abf5835>

Licence: **CC BY 4.0**. The attribution ships beside the data as `inst/extdata/v1.4/demo_hla_tcr_dextramer.ATTRIBUTION.md`.

Dataset landing pages (one per donor):

- <https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-1-1-standard-3-0-2>
- <https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-2-1-standard-3-0-2>
- <https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-3-1-standard-3-0-2>
- <https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-4-1-standard-3-0-2>

## 2.2 The experiment, in one paragraph

CD8+ T cells from four HLA-haplotyped healthy donors were stained with a pool of dCODE dextramers — 98 reagents spanning **8 HLA alleles** (A\*01:01, A\*02:01, A\*03:01, A\*11:01, A\*24:02, B\*07:02, B\*08:01, B\*35:01), the same panel for every donor — then **sorted for dextramer binding** and run on 10x 5′ Single Cell Immune Profiling.
Each cell therefore carries, simultaneously: a paired αβ TCR, a transcriptome, a TotalSeq-C surface-protein panel, and the identity of the dextramer it bound.
About **190,000 cells** across the four donors before any filtering.

## 2.3 Exact download

The build script fetches these on demand; the cache is `data-raw/vdj_10x_dextramer/` (gitignored, ~2.7 GB unpacked).
Manually, for each donor `d` in 1..4:

```bash
base=https://cf.10xgenomics.com/samples/cell-vdj/3.0.2
stem=vdj_v1_hs_aggregated_donor${d}
mkdir -p data-raw/vdj_10x_dextramer && cd data-raw/vdj_10x_dextramer

curl -fL -O ${base}/${stem}/${stem}_all_contig_annotations.csv
curl -fL -O ${base}/${stem}/${stem}_binarized_matrix.csv
curl -fL -O ${base}/${stem}/${stem}_filtered_feature_bc_matrix.tar.gz
tar xzf ${stem}_filtered_feature_bc_matrix.tar.gz -C ${stem}_gex
```

Three files per donor are used:

| file | size (d1–d4) | one row / entry is | what it carries |
|---|---|---|---|
| `*_all_contig_annotations.csv` | 32–51 MB | one **contig** | barcode, chain (TRA/TRB), V/J gene, CDR3 nt+aa, productive flag |
| `*_binarized_matrix.csv` | 19–53 MB | one **cell** | donor, `cell_clono_cdr3_aa`, TotalSeq-C protein UMIs, and one `True`/`False` column per dextramer |
| `*_filtered_feature_bc_matrix.tar.gz` | ~283 MB | matrix | gene expression (33,538 genes) + feature-barcode assays |

Download robustness matters here: R's default `timeout` is 60 s, which silently truncates the ~283 MB matrix and leaves a partial file that the next run mistakes for a completed download.
The script therefore downloads to a `.part` file with `curl -C -` (resume) and `--retry`, raises `options(timeout=)`, and only renames into place on success.

## 2.4 What the dextramer columns encode

Each dextramer column is named `<allele>_<peptide>_<antigen>_binder`:

```
A0201_GILGFVFTL_Flu-MP_Influenza_binder
A0301_KLGGALQAK_IE-1_CMV_binder
A1101_AVFDRKSDAK_EBNA-3B_EBV_binder
```

The allele prefix is the **reagent's own HLA restriction** — a published property of the dextramer, independent of these cells.
So every antigen-specific cell yields a real triple: peptide, antigen, and the HLA allele that presents it.
Columns containing `NR(` are 10x's negative controls and are never treated as evidence.

## 2.5 Processing, step by step

**TCR (the half that decides what the network draws).**

1. Keep only **productive** TRA/TRB contigs — a non-productive contig carries a CDR3 the cell never displayed.
2. `scRepertoire::combineTCR(..., filterMulti = TRUE)` collapses a barcode's contigs into **one row per cell**, with the chains in underscore-joined `CTgene` / `CTaa` strings.
   `filterMulti` keeps the dominant chain when a barcode reports more than one, rather than letting an ambiguous cell contribute a CDR3 it may not carry.

Why start from the contigs rather than the (much smaller) binarized matrix: the binarized matrix has CDR3 amino acids but **no V/J gene at all**, and this source identifies a receptor by *(V gene, CDR3)*.
That is declared as `receptor_key = "v_gene+cdr3"`, which makes split-by-V the app's default so a node means what the source means.
It is also what the app's parser requires — `hla_parse_ir_segments()` takes `barcode` + `CTgene` + `CTaa` and drops any row without a V gene.

**Antigen specificity and HLA restriction.**

3. A cell is assigned **exactly one** specificity or none: cells binding several dextramers are **dropped, not guessed at**, because an ambiguous specificity would place a cell in the wrong HLA context — the one error this page must not make.
   Of 189,512 cells, 87,490 have exactly one.
4. The restricting allele, peptide and antigen are parsed off the winning column name.

**Cell selection.**

5. Keep cells that have a productive clonotype **and** exactly one specificity. This *is* the data set's defining property, not a convenience.
6. Deterministic subsample to **3,000 cells per donor = 12,000 cells** (`set.seed(20260721)`), so the shipped file stays a few MB.

**Expression and projection.**

7. Read each donor's matrix, subset to the kept barcodes **first** (nothing is computed on cells that are thrown away), then the standard Seurat path: normalise → 2,000 variable features → scale → PCA (30) → UMAP.
8. Ship the normalised data for the variable genes only.

**Assembly and self-verification.**

9. Build `Cerebro_v1.3` with expression, metadata, UMAP, groups, the per-donor repertoire list, the declared contracts, and the HLA table.
10. Re-read the written file and re-derive the network with the package's own motif core, so the numbers reported by the build are what the shipped object actually produces — a build that loses the motif structure reports it instead of shipping silently.

## 2.6 What the shipped object contains

```
12,000 cells x 2,000 genes, UMAP projection
groups: sample, cell_type, antigen, restricting_allele
metadata: cell_barcode, sample, cell_type, antigen, peptide, restricting_allele
21 antigens; 6 restricting alleles present on cells
immune_repertoire: donor1..donor4, chains TRA + TRB

measured on the shipped file:
  TRB  3,350 unique CDR3 -> 157 nodes in  31 motifs
  TRA  3,067 unique CDR3 -> 367 nodes in 130 motifs
```

Declared contracts:

| contract | value | why |
|---|---|---|
| `observation_unit` | `cell` | these really are sequenced cells, unlike the bulk demo |
| `receptor_key` | `v_gene+cdr3` | the source identifies a receptor by V **and** CDR3 |
| `tcr_selection` | `antigen-selected` | the repertoire was sorted for binding; the page must say so |
| `lineage_column` | `cell_type` | declared, so the app never has to infer which column holds the lineage |

---

# 3. Known problems with `demo_hla_tcr_dextramer.crb`

Recorded here so nobody has to rediscover them.

## 3.1 The genotypes are published — and inferring them instead was wrong

The donors' HLA haplotypes are in **table S1** of the paper's supplementary PDF.
The link printed in the paper (`advances.sciencemag.org/.../DC1`) is dead — that domain was retired when Science migrated — but the file is served from science.org:

```
https://www.science.org/doi/suppl/10.1126/sciadv.abf5835/suppl_file/abf5835_sm.pdf
```

The table is transcribed into [`donor_hla_haplotypes.csv`](donor_hla_haplotypes.csv), which the build reads. The PDF itself is not committed (this repository never commits third-party raw sources); a local copy lives in the gitignored cache as `vdj_10x_dextramer/paper_supplement_table_S1_donor_HLA.pdf`.

| donor | HLA-A | HLA-B |
|---|---|---|
| Donor 1 | A\*02:01, A\*11:01 | B\*35:01 |
| Donor 2 | A\*02:01, A\*01:01 | B\*08:01 |
| Donor 3 | A\*24:02, A\*29:02 | B\*35:02, B\*44:03 |
| Donor 4 | A\*03:01, A\*03:01 | B\*07:02, B\*57:01 |
| Donor V | A\*02:01, A\*29:02 | B\*35:01, B\*57:01 |

**Worth recording: an earlier version of this build inferred the genotypes from dextramer binding, and the inference was badly wrong.**
It required an allele to account for ≥ 200 cells and ≥ 10 % of a donor's antigen-specific cells — a cut that looked careful and reproduced the per-donor profile of the paper's own quality-controlled call set.
Against table S1 it still fails:

| donor | inferred from binding | published (table S1) |
|---|---|---|
| 1 | A\*02:01, A\*03:01, A\*11:01 | A\*02:01, A\*11:01, B\*35:01 — **no A\*03:01** |
| 2 | A\*02:01, A\*03:01, B\*08:01 | A\*02:01, **A\*01:01**, B\*08:01 — **no A\*03:01** |
| 3 | A\*03:01 | A\*24:02, A\*29:02, B\*35:02, B\*44:03 — **carries no A\*03:01 at all** |
| 4 | A\*03:01, A\*11:01 | A\*03:01 (homozygous), B\*07:02, B\*57:01 — **no A\*11:01** |

Donor 3 is the one that settles it: **25,674 of its cells — 92.8 % of its antigen-specific cells — bound A\*03:01-restricted dextramers, and the donor carries no A\*03:01.**
Cross-reactivity at that scale is not something a threshold can separate, so binding simply cannot stand in for a genotype.
The lesson is worth keeping even though the problem is now solved: an inference that looks well-calibrated against one derived data set can still be wrong about the thing it claims to measure.

## 3.2 What is still declared: the selection, not the genotypes

With published genotypes the earlier circularity is gone — a donor's alleles were measured independently of these cells, so a carrier / non-carrier contrast here is a real comparison.

What remains true, and is declared, is that the **repertoire is antigen-selected**: cells were sorted for dextramer binding, so which receptors are present was decided by the panel, and this is not an unbiased sample of the donors' repertoires.
That is stated in `technical_info$tcr_selection = "antigen-selected"` with the detail in `tcr_selection_detail`, and the app prints it above the Associations tables.
It is a statement about how the cells were chosen, not about the genotypes.

## 3.3 The paper's curated table cannot be joined to the expression matrices

`abf5835_data_file_s1.csv` is in several ways a better input than the raw files: it is the authors' ICON-filtered call set, already one row per cell with paired αβ and V/J parsed, and it carries real phenotype labels (Tem / Tcm / Tpm / Temra / Naïve) plus a fifth donor ("Donor V").

It is not used as a build input because its barcodes carry the paper's own cross-library aggregation index — `D1.AAACCTGAGATTACCC-20` — while the per-donor matrices use `-1`.
Of donor1's 7,573 curated cells, **11** match the donor1 matrix. No mapping key is published.

Using it would therefore mean giving up the transcriptome, and "real **single-cell**" is precisely what this demo exists to demonstrate.
It is used instead as an **independent check on the genotype inference** (§3.1).

## 3.4 Smaller limitations

- **CD8 only.** Every cell is a sorted CD8+ T cell, so `cell_type` has a single level and the Class I × Class II **pair scope cannot be exercised** on this demo.
- **No phenotype detail.** The Tem / Tcm / … labels live in the un-joinable curated table (§3.3), so `cell_type` is the flat `CD8 T`.
- **Largest demo shipped.** 7.8 MB, against 0.3–5.8 MB for the others. Driven by 12,000 cells × 2,000 genes; lower `CELLS_PER_DONOR` in the build script to trade network richness for size.
- **The same 98-reagent panel was used for every donor**, so panel composition carries no donor-specific HLA information — only binding does. This is why §3.1 has to infer rather than read off the design.

---

# 4. Rebuilding

```bash
Rscript data-raw/build_hla_tcr_dextramer_demo.R      # real antigen-selected single-cell demo
Rscript data-raw/build_hla_tcr_demo.R          # synthetic fixture
Rscript data-raw/build_hla_tcr_bulk_demo.R     # real bulk + real genotypes
```

Each script is self-verifying: it re-reads the `.crb` it wrote and re-derives the motif network with the package's own core, so a drifted build fails loudly rather than shipping an empty network.

The step-by-step walkthrough of the 10x pipeline, with the data shown before and after each transformation, is `vignettes/hla_tcr_antigen_selected.Rmd`.
