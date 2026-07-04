#!/usr/bin/env Rscript
#' Build three *distinct* demo .crb files for the multi-crb / immune-repertoire
#' demo.
#'
#' Unlike a naive "same data, different IR slot" demo, these three data sets
#' differ in their **cell composition** (so the UMAP and cell-type mix actually
#' change when you switch), carry **user-facing sample names**, and receive
#' **cell-type-constrained** clonotypes (TCR -> T cells, BCR -> B cells) so the
#' immune repertoire is biologically plausible rather than random noise:
#'
#'   demo_healthy_t.crb   "PBMC - Healthy (T/NK)"   T + Mono subset,  TCR only
#'   demo_bcell_rich.crb  "PBMC - B-cell rich"      B + some T subset, BCR only
#'   demo_full_tcr_bcr.crb "PBMC - Full (T+B)"      all cells, TCR->T + BCR->B
#'
#' Clonotype source: 10x Genomics public dataset vdj_v1_hs_pbmc3 (Human PBMC,
#' 5' VDJ), Cell Ranger 3.1.0. Purely public data; the only identity handling
#' is neutral, descriptive sample names.
#'
#' Pipeline: 10x filtered_contig_annotations.csv -> scRepertoire loadContigs +
#' combineTCR/combineBCR -> CT* clonotype pool -> assigned only to cells of the
#' matching lineage -> injected into a fresh Cerebro_v1.3 subset of example.crb.

suppressMessages({
  library(cerebroAppLite)
  library(scRepertoire)
})

## ---- Paths (override via env for a tmp dry-run) ---------------------------
src_crb <- Sys.getenv("SRC_CRB", "inst/extdata/v1.4/example.crb")
t_csv <- Sys.getenv("T_CSV", "data-raw/vdj_10x/pbmc3_t_contig.csv")
b_csv <- Sys.getenv("B_CSV", "data-raw/vdj_10x/pbmc3_b_contig.csv")
out_healthy <- Sys.getenv("OUT_HEALTHY", "inst/extdata/v1.4/demo_healthy_t.crb")
out_bcell <- Sys.getenv("OUT_BCELL", "inst/extdata/v1.4/demo_bcell_rich.crb")
out_full <- Sys.getenv("OUT_FULL", "inst/extdata/v1.4/demo_full_tcr_bcr.crb")

ct_cols <- c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict")

## Fields copied when reconstructing a Cerebro_v1.3 object.
data_fields <- c(
  "expression",
  "meta_data",
  "projections",
  "groups",
  "gene_lists",
  "trees",
  "trajectories",
  "enriched_pathways",
  "marker_genes",
  "most_expressed_genes",
  "extra_material",
  "cell_cycle",
  "parameters",
  "technical_info",
  "experiment",
  "version",
  "expression_backend"
)

## ---- 1. clonotype pools from 10x contigs ----------------------------------
message("[1/5] Building clonotype pools from 10x contigs ...")

pool_from <- function(csv, kind) {
  raw <- read.csv(csv, stringsAsFactors = FALSE)
  contigs <- loadContigs(raw, format = "10X")
  combined <- if (kind == "TCR") {
    combineTCR(contigs)
  } else {
    combineBCR(contigs, threshold = 0.85)
  }
  df <- do.call(rbind, combined)
  df <- df[, ct_cols, drop = FALSE]
  df <- df[
    !is.na(df$CTgene) &
      nzchar(df$CTgene) &
      !is.na(df$CTstrict) &
      nzchar(df$CTstrict),
    ,
    drop = FALSE
  ]
  unique(df)
}

pool_tcr <- pool_from(t_csv, "TCR")
pool_bcr <- pool_from(b_csv, "BCR")
message(sprintf(
  "  TCR pool: %d clonotypes | BCR pool: %d clonotypes",
  nrow(pool_tcr),
  nrow(pool_bcr)
))

## ---- 2. helpers: subset + cell-type-constrained IR injection --------------
old <- readRDS(src_crb)
full_meta <- old$getMetaData()
stopifnot("cell_type" %in% colnames(full_meta))

## Rebuild a fresh Cerebro_v1.3 restricted to the given cell barcodes. The
## per-cell slots (expression columns, meta rows, projection rows) are filtered
## consistently; pre-computed group-level analyses are carried over as-is.
subset_cerebro <- function(keep_barcodes, experiment_name) {
  new <- Cerebro_v1.3$new()
  for (f in data_fields) {
    val <- old[[f]]
    if (!is.null(val)) new[[f]] <- val
  }
  bc <- old$getMetaData()$cell_barcode
  keep <- bc %in% keep_barcodes

  ## expression: genes x cells -> keep matching columns
  if (!is.null(new$expression)) {
    new$expression <- old$expression[, keep, drop = FALSE]
  }
  ## meta_data: one row per cell
  new$meta_data <- old$meta_data[keep, , drop = FALSE]
  ## projections: one row per cell, keyed by barcode
  if (!is.null(old$projections)) {
    new$projections <- lapply(old$projections, function(p) {
      p[keep, , drop = FALSE]
    })
  }
  ## stamp a descriptive experiment name if the slot supports it
  if (!is.null(new$experiment) && is.list(new$experiment)) {
    new$experiment$experiment_name <- experiment_name
  }
  new
}

## Assign clonotypes from `pool` only to cells whose cell_type matches
## `lineage_regex`, sampling with replacement. Returns a data.frame with the
## five IR columns the Shiny app expects.
assign_ir <- function(meta, pool, lineage_regex, seed = 42) {
  set.seed(seed)
  lineage_cells <- meta$cell_barcode[
    grepl(lineage_regex, meta$cell_type, ignore.case = TRUE)
  ]
  if (length(lineage_cells) == 0 || nrow(pool) == 0) {
    return(NULL)
  }
  picks <- pool[
    sample(nrow(pool), length(lineage_cells), replace = TRUE),
    ct_cols,
    drop = FALSE
  ]
  data.frame(
    barcode = lineage_cells,
    CTgene = picks$CTgene,
    CTnt = picks$CTnt,
    CTaa = picks$CTaa,
    CTstrict = picks$CTstrict,
    stringsAsFactors = FALSE
  )
}

## ---- 3. demo_healthy_t: T + Monocytes, TCR on T cells ---------------------
message("[2/5] Building demo_healthy_t (T/NK, TCR) ...")
keep_healthy <- full_meta$cell_barcode[
  grepl("T cell|Mono", full_meta$cell_type, ignore.case = TRUE)
]
healthy <- subset_cerebro(keep_healthy, "PBMC - Healthy (T/NK)")
healthy_ir <- assign_ir(healthy$getMetaData(), pool_tcr, "T cell")
healthy$immune_repertoire <- setNames(list(healthy_ir), "healthy_t")

## ---- 4. demo_bcell_rich: B + subset of T, BCR on B cells ------------------
message("[3/5] Building demo_bcell_rich (B-cell rich, BCR) ...")
set.seed(7)
b_cells <- full_meta$cell_barcode[grepl("B cell", full_meta$cell_type)]
t_cells_all <- full_meta$cell_barcode[grepl("T cell", full_meta$cell_type)]
t_subset <- sample(t_cells_all, floor(length(t_cells_all) * 0.25))
keep_bcell <- c(b_cells, t_subset)
bcell <- subset_cerebro(keep_bcell, "PBMC - B-cell rich")
bcell_ir <- assign_ir(bcell$getMetaData(), pool_bcr, "B cell")
bcell$immune_repertoire <- setNames(list(bcell_ir), "bcell_rich")

## ---- 5. demo_full_tcr_bcr: all cells, TCR->T and BCR->B -------------------
message("[4/5] Building demo_full_tcr_bcr (Full, T+B) ...")
full <- subset_cerebro(full_meta$cell_barcode, "PBMC - Full (T+B)")
full_ir <- rbind(
  assign_ir(full_meta, pool_tcr, "T cell"),
  assign_ir(full_meta, pool_bcr, "B cell")
)
full$immune_repertoire <- setNames(list(full_ir), "full_tcr_bcr")

## ---- 6. save + verify ------------------------------------------------------
message("[5/5] Saving and verifying ...")
dir.create(dirname(out_full), recursive = TRUE, showWarnings = FALSE)
saveRDS(healthy, out_healthy)
saveRDS(bcell, out_bcell)
saveRDS(full, out_full)

verify <- function(path) {
  o <- readRDS(path)
  m <- o$getMetaData()
  ir <- o$immune_repertoire
  ct <- unlist(lapply(ir, function(d) d$CTgene))
  chains <- c(
    if (any(grepl("TRA|TRB|TRG|TRD", ct))) "TCR",
    if (any(grepl("IGH|IGK|IGL", ct))) "BCR"
  )
  ## sanity: are TCR barcodes actually on T cells (and BCR on B)?
  ir_df <- do.call(rbind, ir)
  ct_of <- m$cell_type[match(ir_df$barcode, m$cell_barcode)]
  tcr_on_t <- all(
    grepl("T cell", ct_of[grepl("TRA|TRB", ir_df$CTgene)])
  )
  bcr_on_b <- all(
    grepl("B cell", ct_of[grepl("IGH|IGK|IGL", ir_df$CTgene)])
  )
  message(sprintf(
    paste0(
      "  %-22s | %.0f KB | cells=%d | types=%s | IR=%s | rows=%d | ",
      "TCR-on-T=%s BCR-on-B=%s"
    ),
    basename(path),
    file.size(path) / 1024,
    nrow(m),
    paste(names(table(m$cell_type)), collapse = "/"),
    paste(chains, collapse = "+"),
    nrow(ir_df),
    tcr_on_t,
    bcr_on_b
  ))
}
verify(out_healthy)
verify(out_bcell)
verify(out_full)
cat("\nDone.\n")
