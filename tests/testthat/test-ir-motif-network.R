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
IR_MOTIF_MAX_LEGEND_CLUSTERS <- ir_env$IR_MOTIF_MAX_LEGEND_CLUSTERS

# Lift ir_apply_display out of server.R so we can test that the generic display
# hook does NOT clobber the legend.position a plot set for itself. server.R is
# full of reactive()/output$ top-level code that needs a live Shiny session, so
# we extract just the ir_apply_display definition rather than sourcing the file.
server_r <- if (!is.na(local_inst)) {
  file.path(local_inst, "shiny/v1.4/immune_repertoire/server.R")
} else {
  system.file(
    "shiny/v1.4/immune_repertoire/server.R",
    package = "cerebroAppLite"
  )
}
if (nzchar(server_r) && file.exists(server_r)) {
  src <- paste(readLines(server_r, warn = FALSE), collapse = "\n")
  # Grab the ir_apply_display <- function(...) { ... } block by brace-matching.
  start <- regexpr("ir_apply_display\\s*<-\\s*function", src)
  if (start > 0) {
    tail_src <- substring(src, start)
    open_brace <- regexpr("\\{", tail_src)
    depth <- 0L
    end_pos <- NA_integer_
    chars <- strsplit(substring(tail_src, open_brace), "")[[1]]
    for (i in seq_along(chars)) {
      if (chars[i] == "{") {
        depth <- depth + 1L
      } else if (chars[i] == "}") {
        depth <- depth - 1L
        if (depth == 0L) {
          end_pos <- i
          break
        }
      }
    }
    def <- substring(tail_src, 1, open_brace + end_pos - 1)
    eval(parse(text = def), envir = ir_env)
  }
}
ir_apply_display <- ir_env$ir_apply_display

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

test_that("ir_build_motif_groups handles a single-row length bin without crashing", {
  df <- data.frame(cdr3 = "CASSL", stringsAsFactors = FALSE)
  df$cdr3_length <- nchar(df$cdr3)
  res <- ir_build_motif_groups(df, by_v = FALSE, threshold = 1)
  expect_equal(unname(res$motif_df$motif_size), 1)
  expect_null(res$edges)
})

test_that("ir_build_motif_groups tags by_v edges with their V gene", {
  # Two CASSL/CASSF pairs, one per V gene: each V forms its own edge, and the
  # edge should carry the v_gene of its bin.
  df <- data.frame(
    cdr3 = c("CASSL", "CASSF", "CASSL", "CASSF"),
    v_gene = c("TRBV1", "TRBV1", "TRBV2", "TRBV2"),
    stringsAsFactors = FALSE
  )
  df$cdr3_length <- nchar(df$cdr3)
  res <- ir_build_motif_groups(df, by_v = TRUE, threshold = 1)
  expect_true("v_gene" %in% colnames(res$edges))
  expect_setequal(unique(res$edges$v_gene), c("TRBV1", "TRBV2"))
  expect_equal(nrow(res$edges), 2)
})

# --- ir_build_motif_graph --------------------------------------------------

test_that("ir_build_motif_graph builds a graph, drops isolates, keeps metadata", {
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", 1:4),
      CTgene = c(
        "TRBV1..TRBJ1.TRBC1",
        "TRBV1..TRBJ1.TRBC1",
        "TRBV1..TRBJ1.TRBC1",
        "TRBV2..TRBJ2.TRBC2"
      ),
      CTaa = c("CASSL", "CASSF", "CASTL", "CWXYZ"),
      sample = c("s1", "s1", "s1", "s1"),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1
  )
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 3)
  expect_true("cdr3" %in% igraph::vertex_attr_names(g))
  expect_true("sample" %in% igraph::vertex_attr_names(g))
})

test_that("ir_build_motif_graph returns NULL when no cluster survives", {
  data <- list(
    s1 = data.frame(
      barcode = c("b1", "b2"),
      CTgene = c("TRBV1..TRBJ1.TRBC1", "TRBV1..TRBJ1.TRBC1"),
      CTaa = c("CASSL", "CWXYZ"),
      sample = c("s1", "s1"),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1
  )
  expect_null(g)
})

test_that("ir_build_motif_graph min_size drops small clusters", {
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", 1:2),
      CTgene = c("TRBV1..TRBJ1.TRBC1", "TRBV1..TRBJ1.TRBC1"),
      CTaa = c("CASSL", "CASSF"),
      sample = c("s1", "s1"),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 2
  )
  expect_null(g)
})

# --- ir_build_motif_plot ---------------------------------------------------

test_that("ir_build_motif_plot returns a ggplot for a valid graph", {
  skip_if_not_installed("ggraph")
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", 1:3),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", 3),
      CTaa = c("CASSL", "CASSF", "CASTL"),
      sample = c("s1", "s1", "s1"),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1
  )
  p <- ir_build_motif_plot(g, color_by = NULL)
  expect_s3_class(p, "ggplot")
  # ggplot objects are lazy; force a full build so rendering-time errors
  # (broken aes, deprecated geom args) are actually caught by CI.
  built <- ggplot2::ggplot_build(p)
  expect_s3_class(built, "ggplot_built")
  expect_gt(nrow(built$data[[which.max(lengths(built$data))]]), 0)
})

test_that("ir_build_motif_plot returns NULL for a NULL graph", {
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  expect_null(ir_build_motif_plot(NULL, color_by = NULL))
})

test_that("ir_build_motif_plot adds BCR caveat subtitle for IGH", {
  skip_if_not_installed("ggraph")
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", 1:3),
      CTgene = rep("IGHV1..IGHJ1.IGHG1", 3),
      CTaa = c("CARDL", "CARDF", "CARTL"),
      sample = c("s1", "s1", "s1"),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "IGH",
    threshold = 1,
    by_v = FALSE,
    min_size = 1
  )
  p <- ir_build_motif_plot(g, color_by = NULL, chain = "IGH")
  expect_s3_class(p, "ggplot")
  expect_true(grepl("SHM", p$labels$subtitle %||% ""))
})

# --- ir_build_motif_graph show_isolated ------------------------------------

test_that("ir_build_motif_graph drops isolated CDR3s by default", {
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", 1:4),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", 4),
      CTaa = c("CASSL", "CASSF", "CASTL", "CWXYZ"),
      sample = rep("s1", 4),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1
  )
  # CWXYZ is isolated -> dropped; only the 3-node cluster remains.
  expect_equal(igraph::vcount(g), 3)
})

test_that("ir_build_motif_graph show_isolated keeps isolated CDR3s", {
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", 1:4),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", 4),
      CTaa = c("CASSL", "CASSF", "CASTL", "CWXYZ"),
      sample = rep("s1", 4),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1,
    show_isolated = TRUE
  )
  # All 4 CDR3s are nodes; CWXYZ is an isolated (degree-0) vertex.
  expect_equal(igraph::vcount(g), 4)
  expect_true("CWXYZ" %in% igraph::V(g)$cdr3)
  expect_equal(sum(igraph::degree(g) == 0), 1)
})

test_that("ir_build_motif_graph show_isolated renders even with no edges", {
  # Two CDR3s far apart (no Hamming-1 edge) — default returns NULL, but
  # show_isolated should still yield a 2-node edgeless graph.
  data <- list(
    s1 = data.frame(
      barcode = c("b1", "b2"),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", 2),
      CTaa = c("CASSL", "CWXYZ"),
      sample = c("s1", "s1"),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  expect_null(ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1
  ))
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1,
    show_isolated = TRUE
  )
  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 2)
  expect_equal(igraph::ecount(g), 0)
})

# --- ir_build_motif_plot legend suppression --------------------------------

test_that("ir_build_motif_plot hides the cluster legend when clusters are many", {
  skip_if_not_installed("ggraph")
  # 25 mutually dissimilar CDR3s -> 25 singleton clusters via show_isolated.
  aa <- vapply(
    1:25,
    function(i) {
      paste0("CASS", LETTERS[((i - 1) %% 26) + 1], sprintf("%02d", i))
    },
    character(1)
  )
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", seq_along(aa)),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", length(aa)),
      CTaa = aa,
      sample = rep("s1", length(aa)),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1,
    show_isolated = TRUE
  )
  expect_gt(
    length(unique(igraph::V(g)$cluster)),
    IR_MOTIF_MAX_LEGEND_CLUSTERS
  )
  p <- ir_build_motif_plot(g, color_by = NULL)
  expect_equal(p$theme$legend.position, "none")
})

test_that("ir_build_motif_plot keeps the legend for a few clusters", {
  skip_if_not_installed("ggraph")
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", 1:3),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", 3),
      CTaa = c("CASSL", "CASSF", "CASTL"),
      sample = rep("s1", 3),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1
  )
  p <- ir_build_motif_plot(g, color_by = NULL)
  expect_equal(p$theme$legend.position, "right")
})

test_that("ir_build_motif_plot hides the legend when show_legend = 'hide'", {
  skip_if_not_installed("ggraph")
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", 1:3),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", 3),
      CTaa = c("CASSL", "CASSF", "CASTL"),
      sample = rep("s1", 3),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1
  )
  # Only a few clusters (auto-hide wouldn't fire), but hide is explicit.
  p <- ir_build_motif_plot(g, color_by = NULL, show_legend = "hide")
  expect_equal(p$theme$legend.position, "none")
})

test_that("ir_build_motif_plot honours legend_pos when shown", {
  skip_if_not_installed("ggraph")
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", 1:3),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", 3),
      CTaa = c("CASSL", "CASSF", "CASTL"),
      sample = rep("s1", 3),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1
  )
  p <- ir_build_motif_plot(
    g,
    color_by = NULL,
    show_legend = "show",
    legend_pos = "bottom"
  )
  expect_equal(p$theme$legend.position, "bottom")
})

test_that("ir_build_motif_plot auto-hides many cluster legend even when shown", {
  skip_if_not_installed("ggraph")
  aa <- vapply(
    1:25,
    function(i) {
      paste0("CASS", LETTERS[((i - 1) %% 26) + 1], sprintf("%02d", i))
    },
    character(1)
  )
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", seq_along(aa)),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", length(aa)),
      CTaa = aa,
      sample = rep("s1", length(aa)),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1,
    show_isolated = TRUE
  )
  # show_legend = "show" but colouring by cluster with >10 levels -> still hidden.
  p <- ir_build_motif_plot(
    g,
    color_by = NULL,
    show_legend = "show",
    legend_pos = "right"
  )
  expect_equal(p$theme$legend.position, "none")
})

test_that("ir_build_motif_plot keeps a metadata legend even with many clusters", {
  skip_if_not_installed("ggraph")
  aa <- vapply(
    1:25,
    function(i) {
      paste0("CASS", LETTERS[((i - 1) %% 26) + 1], sprintf("%02d", i))
    },
    character(1)
  )
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", seq_along(aa)),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", length(aa)),
      CTaa = aa,
      sample = rep(c("s1", "s2"), length.out = length(aa)),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1,
    show_isolated = TRUE
  )
  # colouring by a metadata column (few categories) -> legend kept
  p <- ir_build_motif_plot(g, color_by = "sample")
  expect_equal(p$theme$legend.position, "right")
})

# --- ir_build_motif_plot consensus labels & cluster count ------------------

test_that("ir_build_motif_plot labels only multi-node clusters", {
  skip_if_not_installed("ggraph")
  # One real 3-node cluster + 24 isolated singletons via show_isolated.
  singles <- vapply(
    1:24,
    function(i) {
      paste0("CWXY", LETTERS[((i - 1) %% 26) + 1], sprintf("%02d", i))
    },
    character(1)
  )
  aa <- c("CASSL", "CASSF", "CASTL", singles)
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", seq_along(aa)),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", length(aa)),
      CTaa = aa,
      sample = rep("s1", length(aa)),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1,
    show_isolated = TRUE
  )
  p <- ir_build_motif_plot(g, color_by = NULL)
  # The consensus-label layer is the GeomLabel layer; only the single
  # multi-node cluster gets a label, not the 24 isolated singletons.
  lab_layer <- p$layers[[
    which(vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomLabel"),
      logical(1)
    ))
  ]]
  expect_equal(nrow(lab_layer$data), 1)
})

test_that("ir_build_motif_plot subtitle counts only multi-node clusters", {
  skip_if_not_installed("ggraph")
  singles <- vapply(
    1:24,
    function(i) {
      paste0("CWXY", LETTERS[((i - 1) %% 26) + 1], sprintf("%02d", i))
    },
    character(1)
  )
  aa <- c("CASSL", "CASSF", "CASTL", singles)
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", seq_along(aa)),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", length(aa)),
      CTaa = aa,
      sample = rep("s1", length(aa)),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1,
    show_isolated = TRUE
  )
  p <- ir_build_motif_plot(g, color_by = NULL)
  # Only the single multi-node cluster is a real motif cluster; the 24
  # singletons must not inflate the count.
  expect_match(p$labels$subtitle %||% "", "1 motif cluster")
})

# --- ir_apply_display legend precedence ------------------------------------
# Motif Network sets its own legend.position (manual Hide / auto-hide / place),
# then the plot flows through safeRenderPlot -> ir_apply_display. That hook must
# NOT re-apply legend.position when skip_legend = TRUE, or it clobbers the plot's
# own decision (the "legend flickers on then off" bug). These tests lock that in.

test_that("ir_apply_display leaves legend.position alone when skip_legend = TRUE", {
  skip_if(is.null(ir_apply_display), "ir_apply_display not extractable")
  # A plot that hid its own legend, as ir_build_motif_plot does for auto-hide.
  p <- ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3, g = letters[1:3]),
    ggplot2::aes(x, y, colour = g)
  ) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "none")
  params <- list(ir_d_legend_show = "show", ir_d_legend_pos = "right")
  out <- ir_apply_display(p, params = params, skip_legend = TRUE)
  # skip_legend must preserve the plot's own "none".
  expect_equal(out$theme$legend.position, "none")
})

test_that("ir_apply_display DOES apply legend.position when skip_legend = FALSE", {
  skip_if(is.null(ir_apply_display), "ir_apply_display not extractable")
  # Same plot; without skip_legend the generic hook takes over positioning,
  # overriding the plot's own value. This is the behaviour Motif Network must
  # opt out of, and the reason skip_legend exists.
  p <- ggplot2::ggplot(
    data.frame(x = 1:3, y = 1:3, g = letters[1:3]),
    ggplot2::aes(x, y, colour = g)
  ) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "none")
  params <- list(ir_d_legend_show = "show", ir_d_legend_pos = "bottom")
  out <- ir_apply_display(p, params = params, skip_legend = FALSE)
  expect_equal(out$theme$legend.position, "bottom")
})

test_that("auto-hidden motif legend survives ir_apply_display(skip_legend=TRUE)", {
  skip_if_not_installed("ggraph")
  skip_if(is.null(ir_apply_display), "ir_apply_display not extractable")
  # End-to-end: many clusters -> ir_build_motif_plot hides the legend; the
  # display hook (skip_legend = TRUE, as Motif Network calls it) must keep it
  # hidden in a single render, not flip it back to "right".
  aa <- vapply(
    1:25,
    function(i) {
      paste0("CASS", LETTERS[((i - 1) %% 26) + 1], sprintf("%02d", i))
    },
    character(1)
  )
  data <- list(
    s1 = data.frame(
      barcode = paste0("b", seq_along(aa)),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", length(aa)),
      CTaa = aa,
      sample = rep("s1", length(aa)),
      stringsAsFactors = FALSE
    )
  )
  ir_build_motif_graph <- ir_env$ir_build_motif_graph
  ir_build_motif_plot <- ir_env$ir_build_motif_plot
  g <- ir_build_motif_graph(
    data,
    chain = "TRB",
    threshold = 1,
    by_v = FALSE,
    min_size = 1,
    show_isolated = TRUE
  )
  p <- ir_build_motif_plot(
    g,
    color_by = NULL,
    show_legend = "show",
    legend_pos = "right"
  )
  expect_equal(p$theme$legend.position, "none")
  params <- list(ir_d_legend_show = "show", ir_d_legend_pos = "right")
  out <- ir_apply_display(p, params = params, skip_legend = TRUE)
  expect_equal(out$theme$legend.position, "none")
})
