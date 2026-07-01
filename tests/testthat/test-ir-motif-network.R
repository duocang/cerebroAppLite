# test-ir-motif-network.R — pure-function tests for the Motif Network tab.
# Lifts the motif algorithm helpers + builders out of data.R via a stub env,
# same approach as test-ir-definition-sharing.R.

rel_data_r <- "shiny/v1.4/immune_repertoire/data.R"
inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
local_inst <- inst_candidates[
  file.exists(file.path(inst_candidates, rel_data_r))
][1]
data_r <- if (!is.na(local_inst)) {
  file.path(local_inst, rel_data_r)
} else {
  system.file(rel_data_r, package = "cerebroAppLite")
}
testthat::skip_if_not(
  nzchar(data_r) && file.exists(data_r),
  "immune_repertoire/data.R not found"
)
skip_if_not_installed("stringdist")
skip_if_not_installed("igraph")

ir_env <- new.env()
ir_env$reactive <- function(x) function() NULL
ir_env$reactiveVal <- function(...) function(...) NULL
ir_env$req <- function(...) invisible(NULL)
ir_env$observeEvent <- function(...) invisible(NULL)
ir_env$`%||%` <- function(a, b) if (is.null(a)) b else a
for (nm in c(
  "getImmuneRepertoire",
  "getMetaData",
  "availableProjections",
  "getProjection",
  "detect_chains",
  "input",
  "session",
  "data_set"
)) {
  ir_env[[nm]] <- function(...) NULL
}
sys.source(data_r, envir = ir_env, keep.source = FALSE)

ir_make_consensus <- ir_env$ir_make_consensus
ir_motif_variable_aa <- ir_env$ir_motif_variable_aa
ir_process_length_group <- ir_env$ir_process_length_group
ir_build_motif_groups <- ir_env$ir_build_motif_groups

test_that("ir_make_consensus marks differing positions with x", {
  expect_equal(ir_make_consensus(c("CASSL", "CASSF")), "CASSx")
  expect_equal(ir_make_consensus("CASSL"), "CASSL")
  expect_equal(ir_make_consensus(c("CASSL", "CASSL")), "CASSL")
})

test_that("ir_motif_variable_aa extracts residues at x positions", {
  expect_equal(ir_motif_variable_aa("CASSL", "CASSx"), "L")
  expect_equal(ir_motif_variable_aa("CASSF", "CASSx"), "F")
  expect_equal(ir_motif_variable_aa(NA_character_, "CASSx"), "")
})

test_that("ir_build_motif_groups clusters Hamming<=1 same-length CDR3s", {
  df <- data.frame(
    cdr3 = c("CASSL", "CASSF", "CASTL", "CWXYZ"),
    stringsAsFactors = FALSE
  )
  df$cdr3_length <- nchar(df$cdr3)
  res <- ir_build_motif_groups(df, by_v = FALSE, threshold = 1)
  sizes <- res$motif_df$motif_size[match(
    c("CASSL", "CASSF", "CASTL", "CWXYZ"),
    res$motif_df$cdr3
  )]
  expect_equal(sizes, c(3, 3, 3, 1))
  expect_true(!is.null(res$edges) && nrow(res$edges) >= 2)
})

test_that("ir_build_motif_groups does not connect different-length CDR3s", {
  df <- data.frame(cdr3 = c("CASSL", "CASSLL"), stringsAsFactors = FALSE)
  df$cdr3_length <- nchar(df$cdr3)
  res <- ir_build_motif_groups(df, by_v = FALSE, threshold = 1)
  expect_equal(unname(res$motif_df$motif_size), c(1, 1))
})

test_that("ir_build_motif_groups by_v keeps different-V CDR3s apart", {
  df <- data.frame(
    cdr3 = c("CASSL", "CASSL"),
    v_gene = c("TRBV1", "TRBV2"),
    stringsAsFactors = FALSE
  )
  df$cdr3_length <- nchar(df$cdr3)
  res <- ir_build_motif_groups(df, by_v = TRUE, threshold = 1)
  expect_equal(unname(res$motif_df$motif_size), c(1, 1))
})
